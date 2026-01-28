/-
Copyright (c) 2024 Lean FRO LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Stephen Diehl
-/
module

prelude
public import Lean.Compiler.NameMangling
public import Lean.Compiler.IR.EmitUtil
public import Lean.Compiler.IR.NormIds
public import Lean.Compiler.IR.SimpCase
public import Lean.Compiler.IR.Boxing
public import Lean.Compiler.ModPkgExt

/-!
# Bytecode Emitter for Lean 4 VM

This module emits bytecode for the Lean 4 virtual machine (lean4-vm).
The bytecode format (.leanbc) is a compact binary representation.

## File Format

```
Header (24 bytes):
  magic: [u8; 4] = "LNBC"
  version: u32 = 1
  num_strings: u32
  num_functions: u32
  num_externs: u32
  entry_function: u32

String pool:
  For each string:
    length: u32
    data: [u8; length]

Extern table:
  For each extern:
    name_length: u32
    name: [u8; name_length]
    arity: u8

Function table:
  For each function:
    name_length: u32
    name: [u8; name_length]
    arity: u8
    num_locals: u16
    code_length: u32
    code: [u8; code_length]
```
-/

public section

namespace Lean.IR.EmitBytecode

/-- Bytecode magic number: "LNBC" -/
def magic : ByteArray := ⟨#[0x4C, 0x4E, 0x42, 0x43]⟩
def bcVersion : UInt32 := 1

/-- Opcodes matching lean4-vm/src/bytecode.rs -/
-- Stack/locals
def opLoadLocal    : UInt8 := 0x01
def opStoreLocal   : UInt8 := 0x02
def opLoadConst    : UInt8 := 0x03
def opPop          : UInt8 := 0x04
def opDup          : UInt8 := 0x05
-- Constructors
def opAllocCtor    : UInt8 := 0x10
def opCtorGet      : UInt8 := 0x11
def opCtorSet      : UInt8 := 0x12
def opCtorSetTag   : UInt8 := 0x13
def opGetTag       : UInt8 := 0x14
-- Closures
def opAllocClosure : UInt8 := 0x20
def opClosureGet   : UInt8 := 0x21
def opClosureSet   : UInt8 := 0x22
-- Reference counting
def opInc          : UInt8 := 0x30
def opDec          : UInt8 := 0x31
def opIsShared     : UInt8 := 0x32
def opIsExclusive  : UInt8 := 0x33
-- Function calls
def opCall         : UInt8 := 0x40
def opTailCall     : UInt8 := 0x41
def opApply        : UInt8 := 0x42
def opTailApply    : UInt8 := 0x43
def opPartialApp   : UInt8 := 0x44
def opCallExtern   : UInt8 := 0x45
-- Control flow
def opJump         : UInt8 := 0x50
def opJumpIf       : UInt8 := 0x51
def opJumpIfNot    : UInt8 := 0x52
def opSwitch       : UInt8 := 0x53
def opRet          : UInt8 := 0x54
def opUnreachable  : UInt8 := 0x55
-- Scalars/boxing
def opBox          : UInt8 := 0x60
def opUnbox        : UInt8 := 0x61
def opIsScalar     : UInt8 := 0x62
-- Nat primitives
def opNatLit       : UInt8 := 0x70
def opNatAdd       : UInt8 := 0x71
def opNatSub       : UInt8 := 0x72
def opNatMul       : UInt8 := 0x73
def opNatDiv       : UInt8 := 0x74
def opNatMod       : UInt8 := 0x75
def opNatLt        : UInt8 := 0x76
def opNatLe        : UInt8 := 0x77
def opNatEq        : UInt8 := 0x78
def opNatSucc      : UInt8 := 0x79
-- String primitives
def opStringLit    : UInt8 := 0x80
def opStringAppend : UInt8 := 0x81
def opStringLength : UInt8 := 0x82
def opStringEq     : UInt8 := 0x83
-- Other
def opUnitLit      : UInt8 := 0x90
def opBoolLit      : UInt8 := 0x91
-- Scalar projections
def opScalarProj   : UInt8 := 0xA0
def opScalarSet    : UInt8 := 0xA1
-- Debug
def opTrace        : UInt8 := 0xF0

/-- Compiled function -/
structure CompiledFn where
  name : String
  arity : UInt8
  numLocals : UInt16
  code : ByteArray
deriving Inhabited

/-- Extern declaration -/
structure ExternInfo where
  name : String
  arity : UInt8
deriving Inhabited

/-- Compilation context (read-only during function compilation) -/
structure Context where
  env : Environment
  modName : Name
  jpMap : JPParamsMap := {}
  mainFn : FunId := default
  mainParams : Array Param := #[]

/-- Module-level state -/
structure ModuleState where
  strings : Array String := #[]
  stringMap : Std.HashMap String UInt32 := {}
  externs : Array ExternInfo := #[]
  externMap : Std.HashMap String UInt32 := {}
  functions : Array CompiledFn := #[]
  funcMap : Std.HashMap Name UInt32 := {}
  entry : UInt32 := 0

/-- Function-level state (reset for each function) -/
structure FnState where
  code : ByteArray := ByteArray.empty
  varMap : Std.HashMap VarId UInt16 := {}
  nextLocal : UInt16 := 0
  /-- Forward jump patches: (offset_in_code, label_id) -/
  patches : Array (UInt32 × UInt32) := #[]
  /-- Label targets: label_id -> Some(target_offset) -/
  labels : Array (Option UInt32) := #[]
  /-- Join point labels: JoinPointId -> label_id -/
  jpLabels : Std.HashMap JoinPointId UInt32 := {}
  /-- Join point bodies to emit at the end -/
  jpBodies : Array (JoinPointId × Array Param × FnBody) := #[]

/-- Combined state -/
structure State where
  modState : ModuleState := {}
  fnState : FnState := {}

abbrev M := ReaderT Context (EStateM String State)

@[inline] def getEnv : M Environment := Context.env <$> read
@[inline] def getModName : M Name := Context.modName <$> read

def getDecl (n : Name) : M Decl := do
  let env ← getEnv
  match findEnvDecl env n with
  | some d => pure d
  | none   => throw s!"unknown declaration '{n}'"

-- ============================================================================
-- Bytecode emission helpers
-- ============================================================================

@[inline] def emitByte (b : UInt8) : M Unit :=
  modify fun s => { s with fnState.code := s.fnState.code.push b }

@[inline] def emitU16 (v : UInt16) : M Unit := do
  emitByte (v &&& 0xFF).toUInt8
  emitByte ((v >>> 8) &&& 0xFF).toUInt8

@[inline] def emitU32 (v : UInt32) : M Unit := do
  emitByte (v &&& 0xFF).toUInt8
  emitByte ((v >>> 8) &&& 0xFF).toUInt8
  emitByte ((v >>> 16) &&& 0xFF).toUInt8
  emitByte ((v >>> 24) &&& 0xFF).toUInt8

@[inline] def emitI32 (v : Int32) : M Unit := emitU32 v.toUInt32

@[inline] def emitU64 (v : UInt64) : M Unit := do
  emitU32 (v &&& 0xFFFFFFFF).toUInt32
  emitU32 ((v >>> 32) &&& 0xFFFFFFFF).toUInt32

def currentOffset : M UInt32 := do
  pure (← get).fnState.code.size.toUInt32

-- ============================================================================
-- Interning: strings, externs, functions
-- ============================================================================

def internString (s : String) : M UInt32 := do
  let st ← get
  match st.modState.stringMap[s]? with
  | some idx => pure idx
  | none =>
    let idx := st.modState.strings.size.toUInt32
    set { st with modState := { st.modState with
      strings := st.modState.strings.push s
      stringMap := st.modState.stringMap.insert s idx
    }}
    pure idx

def internExtern (name : String) (arity : UInt8) : M UInt32 := do
  let st ← get
  match st.modState.externMap[name]? with
  | some idx => pure idx
  | none =>
    let idx := st.modState.externs.size.toUInt32
    set { st with modState := { st.modState with
      externs := st.modState.externs.push { name, arity }
      externMap := st.modState.externMap.insert name idx
    }}
    pure idx

def registerFunction (name : Name) (idx : UInt32) : M Unit := do
  modify fun s => { s with modState := { s.modState with
    funcMap := s.modState.funcMap.insert name idx
  }}

def getFuncIndex (name : Name) : M (Option UInt32) := do
  let st ← get
  pure st.modState.funcMap[name]?

def getFuncIndexOrFail (name : Name) : M UInt32 := do
  match ← getFuncIndex name with
  | some idx => pure idx
  | none => throw s!"unknown function '{name}'"

-- ============================================================================
-- Variable slot management
-- ============================================================================

def getVarSlot (v : VarId) : M UInt16 := do
  let st ← get
  match st.fnState.varMap[v]? with
  | some slot => pure slot
  | none =>
    let slot := st.fnState.nextLocal
    set { st with fnState := { st.fnState with
      varMap := st.fnState.varMap.insert v slot
      nextLocal := slot + 1
    }}
    pure slot

def resetFnState : M Unit :=
  modify fun s => { s with fnState := {} }

-- ============================================================================
-- Label management for jumps
-- ============================================================================

def newLabel : M UInt32 := do
  let st ← get
  let id := st.fnState.labels.size.toUInt32
  set { st with fnState := { st.fnState with labels := st.fnState.labels.push none }}
  pure id

def markLabel (labelId : UInt32) : M Unit := do
  let offset ← currentOffset
  modify fun s => { s with fnState := { s.fnState with
    labels := s.fnState.labels.set! labelId.toNat (some offset)
  }}

def emitJumpToLabel (op : UInt8) (labelId : UInt32) : M Unit := do
  emitByte op
  let patchOffset ← currentOffset
  modify fun s => { s with fnState := { s.fnState with
    patches := s.fnState.patches.push (patchOffset, labelId)
  }}
  emitI32 0  -- placeholder

def patchJumps : M Unit := do
  let st ← get
  let mut code := st.fnState.code
  for (patchOffset, labelId) in st.fnState.patches do
    match st.fnState.labels[labelId.toNat]? with
    | some (some target) =>
      let offset : Int32 := (target.toInt32 - patchOffset.toInt32 - 4)
      let bytes := offset.toUInt32
      code := code.set! patchOffset.toNat (bytes &&& 0xFF).toUInt8
      code := code.set! (patchOffset.toNat + 1) ((bytes >>> 8) &&& 0xFF).toUInt8
      code := code.set! (patchOffset.toNat + 2) ((bytes >>> 16) &&& 0xFF).toUInt8
      code := code.set! (patchOffset.toNat + 3) ((bytes >>> 24) &&& 0xFF).toUInt8
    | _ => throw s!"unresolved label {labelId}"
  modify fun s => { s with fnState.code := code }

-- ============================================================================
-- Argument and variable emission
-- ============================================================================

def emitLoadVar (v : VarId) : M Unit := do
  let slot ← getVarSlot v
  emitByte opLoadLocal
  emitU16 slot

def emitStoreVar (v : VarId) : M Unit := do
  let slot ← getVarSlot v
  emitByte opStoreLocal
  emitU16 slot

def emitArg (arg : Arg) : M Unit := do
  match arg with
  | .var v => emitLoadVar v
  | .erased =>
    emitByte opNatLit
    emitU64 0
    emitByte opBox

def emitArgs (args : Array Arg) : M Unit := do
  for arg in args do
    emitArg arg

-- ============================================================================
-- Get C extern name for a function
-- ============================================================================

def getExternCName (name : Name) : M (Option String) := do
  let env ← getEnv
  pure (getExternNameFor env `c name)

-- ============================================================================
-- Expression emission
-- ============================================================================

def emitCtorScalarSize (usize : Nat) (ssize : Nat) : Nat :=
  8 * usize + ssize

partial def emitExpr (z : VarId) (t : IRType) (e : Expr) : M Unit := do
  match e with
  | .ctor info args =>
    if info.size == 0 && info.usize == 0 && info.ssize == 0 then
      -- Nullary constructor: emit boxed tag
      emitByte opNatLit
      emitU64 info.cidx.toUInt64
      emitByte opBox
    else
      -- Push all args first, then allocate
      emitArgs args
      emitByte opAllocCtor
      emitByte info.cidx.toUInt8
      emitByte info.size.toUInt8
    emitStoreVar z

  | .reset n x =>
    -- Reset: check exclusive, release fields, or box(0)
    emitLoadVar x
    emitByte opIsExclusive
    let elseLabel ← newLabel
    let endLabel ← newLabel
    emitJumpToLabel opJumpIfNot elseLabel
    -- exclusive branch: release fields
    for i in [:n] do
      emitLoadVar x
      emitByte opCtorGet
      emitByte i.toUInt8
      emitByte opDec
    emitLoadVar x
    emitStoreVar z
    emitJumpToLabel opJump endLabel
    -- not exclusive: dec ref, return box(0)
    markLabel elseLabel
    emitLoadVar x
    emitByte opDec
    emitByte opNatLit
    emitU64 0
    emitByte opBox
    emitStoreVar z
    markLabel endLabel

  | .reuse x info _updtHeader args =>
    -- Reuse: check if scalar, allocate or reuse
    emitLoadVar x
    emitByte opIsScalar
    let elseLabel ← newLabel
    let endLabel ← newLabel
    emitJumpToLabel opJumpIf elseLabel
    -- not scalar: reuse x
    emitLoadVar x
    emitStoreVar z
    emitJumpToLabel opJump endLabel
    -- scalar: allocate new
    markLabel elseLabel
    emitArgs args
    emitByte opAllocCtor
    emitByte info.cidx.toUInt8
    emitByte info.size.toUInt8
    emitStoreVar z
    markLabel endLabel
    -- Set fields
    for i in [:args.size] do
      emitLoadVar z
      emitArg args[i]!
      emitByte opCtorSet
      emitByte i.toUInt8

  | .proj i x =>
    emitLoadVar x
    emitByte opCtorGet
    emitByte i.toUInt8
    emitStoreVar z

  | .uproj i x =>
    emitLoadVar x
    emitByte opCtorGet
    emitByte i.toUInt8
    emitStoreVar z

  | .sproj n offset x =>
    emitLoadVar x
    emitByte opScalarProj
    emitByte n.toUInt8
    emitU16 offset.toUInt16
    emitStoreVar z

  | .fap f args =>
    match ← getExternCName f with
    | some extName =>
      emitArgs args
      let extIdx ← internExtern extName args.size.toUInt8
      emitByte opCallExtern
      emitU32 extIdx
      emitByte args.size.toUInt8
    | none =>
      match ← getFuncIndex f with
      | some funcIdx =>
        -- Local function call
        emitArgs args
        emitByte opCall
        emitU32 funcIdx
        emitByte args.size.toUInt8
      | none =>
        -- Imported module function - emit as extern call using mangled name
        let env ← getEnv
        let mangledName := getSymbolStem env f
        emitArgs args
        let extIdx ← internExtern mangledName args.size.toUInt8
        emitByte opCallExtern
        emitU32 extIdx
        emitByte args.size.toUInt8
    emitStoreVar z

  | .pap f args =>
    let decl ← getDecl f
    let arity := decl.params.size
    match ← getFuncIndex f with
    | some funcIdx =>
      -- Local function partial application
      emitArgs args
      emitByte opPartialApp
      emitU32 funcIdx
      emitByte arity.toUInt8
      emitByte args.size.toUInt8
    | none =>
      -- Imported function - emit as extern call and rely on runtime for partial app
      let env ← getEnv
      let mangledName := getSymbolStem env f
      emitArgs args
      let extIdx ← internExtern mangledName arity.toUInt8
      emitByte opCallExtern
      emitU32 extIdx
      emitByte args.size.toUInt8
    emitStoreVar z

  | .ap f args =>
    emitLoadVar f
    emitArgs args
    emitByte opApply
    emitByte args.size.toUInt8
    emitStoreVar z

  | .box _ty x =>
    emitLoadVar x
    emitByte opBox
    emitStoreVar z

  | .unbox x =>
    emitLoadVar x
    emitByte opUnbox
    emitStoreVar z

  | .isShared x =>
    emitLoadVar x
    emitByte opIsShared
    emitStoreVar z

  | .lit v =>
    match v with
    | .num n =>
      if t.isObj then
        emitByte opNatLit
        emitU64 n.toUInt64
      else
        emitByte opNatLit
        emitU64 n.toUInt64
        emitByte opUnbox
    | .str s =>
      let strIdx ← internString s
      emitByte opStringLit
      emitU32 strIdx
    emitStoreVar z

-- ============================================================================
-- Reference counting
-- ============================================================================

def emitInc (x : VarId) : M Unit := do
  emitLoadVar x
  emitByte opInc

def emitDec (x : VarId) : M Unit := do
  emitLoadVar x
  emitByte opDec

-- ============================================================================
-- Field operations
-- ============================================================================

def emitSet (x : VarId) (i : Nat) (y : Arg) : M Unit := do
  emitLoadVar x
  emitArg y
  emitByte opCtorSet
  emitByte i.toUInt8

def emitSetTag (x : VarId) (i : Nat) : M Unit := do
  emitLoadVar x
  emitByte opCtorSetTag
  emitByte i.toUInt8

def emitUSet (x : VarId) (i : Nat) (y : VarId) : M Unit := do
  emitLoadVar x
  emitLoadVar y
  emitByte opCtorSet
  emitByte i.toUInt8

def emitSSet (x : VarId) (n : Nat) (offset : Nat) (y : VarId) : M Unit := do
  emitLoadVar x
  emitLoadVar y
  emitByte opScalarSet
  emitByte n.toUInt8
  emitU16 offset.toUInt16

-- ============================================================================
-- Join point handling
-- ============================================================================

def getJPParams (j : JoinPointId) : M (Array Param) := do
  let ctx ← read
  match ctx.jpMap[j]? with
  | some ps => pure ps
  | none => throw "unknown join point"

/-- Register a join point and get its label -/
def registerJoinPoint (j : JoinPointId) (ps : Array Param) (body : FnBody) : M Unit := do
  let lbl ← newLabel
  modify fun s => { s with fnState := { s.fnState with
    jpLabels := s.fnState.jpLabels.insert j lbl
    jpBodies := s.fnState.jpBodies.push (j, ps, body)
  }}

/-- Get the label for a join point -/
def getJPLabel (j : JoinPointId) : M UInt32 := do
  let st ← get
  match st.fnState.jpLabels[j]? with
  | some lbl => pure lbl
  | none => throw s!"unknown join point {j}"

def emitJmp (j : JoinPointId) (xs : Array Arg) : M Unit := do
  let ps ← getJPParams j
  -- Assign join point parameters
  for i in [:xs.size] do
    if h : i < ps.size then
      let p := ps[i]
      unless p.ty.isVoid do
        emitArg xs[i]!
        emitStoreVar p.x
  -- Jump to the join point label
  let lbl ← getJPLabel j
  emitJumpToLabel opJump lbl

-- ============================================================================
-- Case/switch handling
-- ============================================================================

mutual

partial def emitCase (x : VarId) (xType : IRType) (alts : Array Alt) : M Unit := do
  -- Get tag
  if xType.isObj then
    emitLoadVar x
    emitByte opGetTag
  else
    emitLoadVar x

  let alts := ensureHasDefault alts
  let numCases := alts.size

  -- Create end label
  let endLabel ← newLabel

  -- For a simple switch with few cases, use if-else chain
  if numCases <= 4 then
    for i in [:numCases] do
      match alts[i]! with
      | .ctor info body =>
        emitByte opDup
        emitByte opNatLit
        emitU64 info.cidx.toUInt64
        emitByte opNatEq
        let nextLabel ← newLabel
        emitJumpToLabel opJumpIfNot nextLabel
        emitByte opPop  -- pop the tag
        emitBlock body
        emitJumpToLabel opJump endLabel
        markLabel nextLabel
      | .default body =>
        emitByte opPop  -- pop the tag
        emitBlock body
    markLabel endLabel
  else
    -- For many cases, use switch opcode
    emitByte opSwitch
    emitU16 numCases.toUInt16
    -- Emit placeholder offsets
    let _switchBase ← currentOffset
    let mut caseLabels : Array UInt32 := #[]
    for _ in [:numCases] do
      let lbl ← newLabel
      caseLabels := caseLabels.push lbl
      let st ← get
      let patchOffset := st.fnState.code.size.toUInt32
      modify fun s => { s with fnState.patches := s.fnState.patches.push (patchOffset, lbl) }
      emitI32 0
    -- Default offset
    let defaultLabel ← newLabel
    let st ← get
    let defPatch := st.fnState.code.size.toUInt32
    modify fun s => { s with fnState.patches := s.fnState.patches.push (defPatch, defaultLabel) }
    emitI32 0
    -- Emit case bodies
    for i in [:numCases] do
      markLabel caseLabels[i]!
      match alts[i]! with
      | .ctor _ body =>
        emitBlock body
        emitJumpToLabel opJump endLabel
      | .default body =>
        markLabel defaultLabel
        emitBlock body
    markLabel endLabel

partial def emitBlock (b : FnBody) : M Unit := do
  match b with
  | .jdecl j ps jpBody b =>
    -- Register the join point and its body for later emission
    registerJoinPoint j ps jpBody
    -- Emit the main body (which may contain jmp to this join point)
    emitBlock b

  | .vdecl x t v b =>
    emitExpr x t v
    emitBlock b

  | .inc x _ _ persistent b =>
    unless persistent do emitInc x
    emitBlock b

  | .dec x _ _ persistent b =>
    unless persistent do emitDec x
    emitBlock b

  | .del x b =>
    emitDec x
    emitBlock b

  | .setTag x i b =>
    emitSetTag x i
    emitBlock b

  | .set x i y b =>
    emitSet x i y
    emitBlock b

  | .uset x i y b =>
    emitUSet x i y
    emitBlock b

  | .sset x n offset y _ b =>
    emitSSet x n offset y
    emitBlock b

  | .ret arg =>
    emitArg arg
    emitByte opRet

  | .case _ x xType alts =>
    emitCase x xType alts

  | .jmp j xs =>
    emitJmp j xs

  | .unreachable =>
    emitByte opUnreachable

end

/-- Emit all registered join point bodies. They are emitted after the main code. -/
partial def emitJoinPointBodies : M Unit := do
  let st ← get
  let jpBodies := st.fnState.jpBodies
  -- Clear the jpBodies to avoid infinite loops if a JP body contains more JP declarations
  modify fun s => { s with fnState.jpBodies := #[] }
  for (j, _ps, body) in jpBodies do
    -- Mark the label for this join point
    let lbl ← getJPLabel j
    markLabel lbl
    -- Emit the join point body
    emitBlock body
  -- If more join points were registered while emitting, emit them too
  let st' ← get
  unless st'.fnState.jpBodies.isEmpty do
    emitJoinPointBodies

-- ============================================================================
-- Function compilation
-- ============================================================================

def compileFunction (d : Decl) (funcIdx : UInt32) : M Unit := do
  let env ← getEnv
  match d with
  | .fdecl (f := name) (xs := params) (body := body) .. =>
    unless hasInitAttr env name do
      resetFnState

      -- Allocate slots for parameters first
      for p in params do
        discard <| getVarSlot p.x

      -- Pre-allocate slots for all body variables
      let (varMap, jpMap) := mkVarJPMaps d
      for (v, _) in varMap.toList do
        discard <| getVarSlot v

      -- Compile body with join point map
      withReader (fun ctx => { ctx with jpMap, mainFn := name, mainParams := params }) do
        emitBlock body
        -- Emit all join point bodies that were registered
        emitJoinPointBodies

      -- Patch jumps
      patchJumps

      -- Save compiled function
      let st ← get
      let cf : CompiledFn := {
        name := getSymbolStem env name
        arity := params.size.toUInt8
        numLocals := st.fnState.nextLocal
        code := st.fnState.code
      }
      modify fun s => { s with modState := { s.modState with
        functions := s.modState.functions.set! funcIdx.toNat cf
      }}

  | .extern (f := _name) (xs := params) (ext := extData) .. =>
    -- Register extern
    match getExternEntryFor extData `c with
    | some (.standard _ extName) =>
      discard <| internExtern extName params.size.toUInt8
    | _ => pure ()

-- ============================================================================
-- Module compilation (two-pass)
-- ============================================================================

def pass1_registerFunctions : M Unit := do
  let env ← getEnv
  let decls := getDecls env
  let mut idx : UInt32 := 0
  for d in decls.reverse do
    match d with
    | .fdecl (f := name) .. =>
      unless hasInitAttr env name do
        registerFunction name idx
        -- Also push a placeholder function
        modify fun s => { s with modState := { s.modState with
          functions := s.modState.functions.push { name := "", arity := 0, numLocals := 0, code := ByteArray.empty }
        }}
        idx := idx + 1
    | _ => pure ()

def pass2_compileFunctions : M Unit := do
  let env ← getEnv
  let decls := getDecls env
  for d in decls.reverse do
    let d := d.normalizeIds
    match d with
    | .fdecl (f := name) .. =>
      unless hasInitAttr env name do
        let idx ← getFuncIndexOrFail name
        compileFunction d idx
    | .extern .. =>
      compileFunction d 0

def setEntryFunction : M Unit := do
  let env ← getEnv
  let decls := getDecls env
  for d in decls do
    if d.name == `main then
      let idx ← getFuncIndexOrFail `main
      modify fun s => { s with modState.entry := idx }
      return
  -- Try _lean_main
  let st ← get
  if let some idx := st.modState.funcMap[`_lean_main]? then
    modify fun s => { s with modState.entry := idx }
    return
  -- Default to first function
  modify fun s => { s with modState.entry := 0 }

-- ============================================================================
-- Serialization
-- ============================================================================

def serializeModule : M ByteArray := do
  let st ← get
  let ms := st.modState
  let mut out := ByteArray.empty

  -- Header
  out := out ++ magic
  out := out ++ toLE32 bcVersion
  out := out ++ toLE32 ms.strings.size.toUInt32
  out := out ++ toLE32 ms.functions.size.toUInt32
  out := out ++ toLE32 ms.externs.size.toUInt32
  out := out ++ toLE32 ms.entry

  -- String pool
  for s in ms.strings do
    let bytes := s.toUTF8
    out := out ++ toLE32 bytes.size.toUInt32
    out := out ++ bytes

  -- Extern table
  for ext in ms.externs do
    let nameBytes := ext.name.toUTF8
    out := out ++ toLE32 nameBytes.size.toUInt32
    out := out ++ nameBytes
    out := out.push ext.arity

  -- Functions
  for func in ms.functions do
    let nameBytes := func.name.toUTF8
    out := out ++ toLE32 nameBytes.size.toUInt32
    out := out ++ nameBytes
    out := out.push func.arity
    out := out ++ toLE16 func.numLocals
    out := out ++ toLE32 func.code.size.toUInt32
    out := out ++ func.code

  pure out
where
  toLE32 (v : UInt32) : ByteArray :=
    ⟨#[(v &&& 0xFF).toUInt8,
       ((v >>> 8) &&& 0xFF).toUInt8,
       ((v >>> 16) &&& 0xFF).toUInt8,
       ((v >>> 24) &&& 0xFF).toUInt8]⟩
  toLE16 (v : UInt16) : ByteArray :=
    ⟨#[(v &&& 0xFF).toUInt8, ((v >>> 8) &&& 0xFF).toUInt8]⟩

-- ============================================================================
-- Main entry point
-- ============================================================================

def main : M ByteArray := do
  pass1_registerFunctions
  pass2_compileFunctions
  setEntryFunction
  serializeModule

end EmitBytecode

/-- Emit bytecode for a module -/
def emitBytecode (env : Environment) (modName : Name) : Except String ByteArray :=
  match EmitBytecode.main { env, modName } |>.run {} with
  | .ok bytes _ => .ok bytes
  | .error err _ => .error err

end Lean.IR
