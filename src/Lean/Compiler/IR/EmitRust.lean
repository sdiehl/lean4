module

prelude
public import Lean.Compiler.NameMangling
public import Lean.Compiler.IR.EmitUtil
public import Lean.Compiler.IR.NormIds
public import Lean.Compiler.IR.SimpCase
public import Lean.Compiler.IR.Boxing
public import Lean.Compiler.ModPkgExt

public section

namespace Lean.IR.EmitRust

def leanMainFn := "_lean_main"

structure Context where
  env        : Environment
  modName    : Name
  jpMap      : JPParamsMap := {}
  mainFn     : FunId := default
  mainParams : Array Param := #[]
  hasJPs     : Bool := false

abbrev M := ReaderT Context (EStateM String String)

@[inline] def getEnv : M Environment := Context.env <$> read
@[inline] def getModName : M Name := Context.modName <$> read

@[inline] def getModInitFn : M String := do
  let pkg? := (← getEnv).getModulePackage?
  return mkModuleInitializationFunctionName (← getModName) pkg?

def getDecl (n : Name) : M Decl := do
  let env ← getEnv
  match findEnvDecl env n with
  | some d => pure d
  | none   => throw s!"unknown declaration '{n}'"

@[inline] def emit {α : Type} [ToString α] (a : α) : M Unit :=
  modify fun out => out ++ toString a

@[inline] def emitLn {α : Type} [ToString α] (a : α) : M Unit := do
  emit a; emit "\n"

def emitLns {α : Type} [ToString α] (as : List α) : M Unit :=
  as.forM fun a => emitLn a

def argToRustString (x : Arg) : String :=
  match x with
  | .var x => toString x
  | .erased => "lean_box(0)"

def emitArg (x : Arg) : M Unit :=
  emit (argToRustString x)

def toRustType : IRType → String
  | IRType.float      => "f64"
  | IRType.float32    => "f32"
  | IRType.uint8      => "u8"
  | IRType.uint16     => "u16"
  | IRType.uint32     => "u32"
  | IRType.uint64     => "u64"
  | IRType.usize      => "usize"
  | IRType.object     => "*mut LeanObject"
  | IRType.tagged     => "*mut LeanObject"
  | IRType.tobject    => "*mut LeanObject"
  | IRType.erased     => "*mut LeanObject"
  | IRType.void       => "()"
  | IRType.struct _ _ => panic! "struct not implemented"
  | IRType.union _ _  => panic! "union not implemented"

def toRustName (n : Name) : M String := do
  let env ← getEnv
  match getExportNameFor? env n with
  | some (.str .anonymous s) => return s
  | some _ => throw s!"invalid export name '{n}'"
  | none => return if n == `main then leanMainFn else getSymbolStem env n

def emitRustName (n : Name) : M Unit :=
  toRustName n >>= emit

/-- Convert a Lean module name to a valid Rust module identifier.
    e.g. `Init.Prelude` → `"Init_Prelude"`, `MultiLib` → `"MultiLib"` -/
def moduleNameToRustIdent (n : Name) : String :=
  n.mangle ""

def getJPParams (j : JoinPointId) : M (Array Param) := do
  let ctx ← read
  match ctx.jpMap[j]? with
  | some ps => pure ps
  | none    => throw "unknown join point"

def defaultValueForType : IRType → String
  | IRType.float      => "0.0"
  | IRType.float32    => "0.0"
  | IRType.uint8      => "0"
  | IRType.uint16     => "0"
  | IRType.uint32     => "0"
  | IRType.uint64     => "0"
  | IRType.usize      => "0"
  | IRType.object     => "std::ptr::null_mut()"
  | IRType.tagged     => "std::ptr::null_mut()"
  | IRType.tobject    => "std::ptr::null_mut()"
  | IRType.erased     => "std::ptr::null_mut()"
  | IRType.void       => "()"
  | IRType.struct _ _ => "std::ptr::null_mut()"
  | IRType.union _ _  => "std::ptr::null_mut()"

def declareVar (x : VarId) (t : IRType) : M Unit := do
  if t.isVoid then pure ()
  else
    emit "let mut "; emit x; emit ": "; emit (toRustType t)
    emit " = "; emit (defaultValueForType t); emitLn ";"

def declareParams (ps : Array Param) : M Unit :=
  ps.forM fun p => declareVar p.x p.ty

partial def declareVars : FnBody → M Unit
  | FnBody.vdecl x t _ b => do
    let ctx ← read
    if isTailCallTo ctx.mainFn (FnBody.vdecl x t (Expr.lit (.num 0)) b) then
      pure ()
    else
      declareVar x t; declareVars b
  | FnBody.jdecl _ xs v b => do
    declareParams xs
    declareVars v
    declareVars b
  | FnBody.case _ _ _ alts => do
    alts.forM fun alt =>
      match alt with
      | Alt.ctor _ b  => declareVars b
      | Alt.default b => declareVars b
  | e => if e.isTerminal then pure () else declareVars e.body

partial def collectJoinPoints : FnBody → Array JoinPointId → Array JoinPointId
  | FnBody.jdecl j _ v b, acc =>
    let acc := acc.push j
    let acc := collectJoinPoints v acc
    collectJoinPoints b acc
  | FnBody.case _ _ _ alts, acc =>
    alts.foldl (fun acc alt =>
      match alt with
      | Alt.ctor _ b  => collectJoinPoints b acc
      | Alt.default b => collectJoinPoints b acc) acc
  | e, acc => if e.isTerminal then acc else collectJoinPoints e.body acc

def emitTag (x : VarId) (xType : IRType) : M Unit := do
  if xType.isObj then
    emit "lean_obj_tag("; emit x; emit ")"
  else
    emit x

def emitInc (x : VarId) (n : Nat) (checkRef : Bool) : M Unit := do
  let fn := if checkRef then (if n == 1 then "lean_inc" else "lean_inc_n") else (if n == 1 then "lean_inc_ref" else "lean_inc_ref_n")
  emit fn; emit "("; emit x
  if n != 1 then emit ", "; emit n
  emitLn ");"

def emitDec (x : VarId) (n : Nat) (checkRef : Bool) : M Unit := do
  emit (if checkRef then "lean_dec" else "lean_dec_ref")
  emit "("; emit x
  if n != 1 then emit ", "; emit n
  emitLn ");"

def emitDel (x : VarId) : M Unit := do
  emit "lean_free_object_only("; emit x; emitLn ");"

def emitSetTag (x : VarId) (i : Nat) : M Unit := do
  emit "lean_ctor_set_tag("; emit x; emit ", "; emit i; emitLn ");"

def emitSet (x : VarId) (i : Nat) (y : Arg) : M Unit := do
  emit "lean_ctor_set("; emit x; emit ", "; emit i; emit ", "; emitArg y; emitLn ");"

def emitOffset (n : Nat) (offset : Nat) : M Unit := do
  if n > 0 then
    emit "8*"; emit n
    if offset > 0 then emit " + "; emit offset
  else
    emit offset

def emitUSet (x : VarId) (n : Nat) (y : VarId) : M Unit := do
  emit "lean_ctor_set_usize("; emit x; emit ", "; emit n; emit ", "; emit y; emitLn ");"

def emitSSet (x : VarId) (n : Nat) (offset : Nat) (y : VarId) (t : IRType) : M Unit := do
  let fn := match t with
    | IRType.float   => "lean_ctor_set_float"
    | IRType.float32 => "lean_ctor_set_float32"
    | IRType.uint8   => "lean_ctor_set_uint8"
    | IRType.uint16  => "lean_ctor_set_uint16"
    | IRType.uint32  => "lean_ctor_set_uint32"
    | IRType.uint64  => "lean_ctor_set_uint64"
    | _              => "lean_ctor_set_uint64"
  emit fn; emit "("; emit x; emit ", "; emitOffset n offset; emit ", "; emit y; emitLn ");"

def emitJmp (j : JoinPointId) (xs : Array Arg) : M Unit := do
  let ps ← getJPParams j
  let ctx ← read
  if h : xs.size = ps.size then
    xs.size.forM fun i _ => do
      let p := ps[i]
      if !p.ty.isVoid then
        let x := xs[i]
        emit p.x; emit " = "; emitArg x; emitLn ";"
    if ctx.hasJPs then
      emit "_jp_state = "; emit j.idx; emitLn "; continue '_start;"
    else
      emitLn "continue '_start;"
  else
    throw "invalid goto"

def emitLhs (z : VarId) : M Unit := do
  emit z; emit " = "

def emitArgs (ys : Array Arg) : M Unit :=
  ys.size.forM fun i _ => do
    if i > 0 then emit ", "
    emitArg ys[i]

def emitCtorScalarSize (usize : Nat) (ssize : Nat) : M Unit := do
  if usize == 0 then emit ssize
  else if ssize == 0 then emit "8*"; emit usize
  else emit "8*"; emit usize; emit " + "; emit ssize

def emitAllocCtor (c : CtorInfo) : M Unit := do
  emit "lean_alloc_ctor("; emit c.cidx; emit ", "; emit c.size; emit ", "
  emitCtorScalarSize c.usize c.ssize; emit ")"

def emitCtorSetArgs (z : VarId) (ys : Array Arg) : M Unit :=
  ys.size.forM fun i _ => do
    emit "lean_ctor_set("; emit z; emit ", "; emit i; emit ", "; emitArg ys[i]; emitLn ");"

def emitCtor (z : VarId) (c : CtorInfo) (ys : Array Arg) : M Unit := do
  emitLhs z
  if c.size == 0 && c.usize == 0 && c.ssize == 0 then
    emit "lean_box("; emit c.cidx; emitLn ");"
  else
    emitAllocCtor c; emitLn ";"; emitCtorSetArgs z ys

def emitReset (z : VarId) (n : Nat) (x : VarId) : M Unit := do
  emit "if lean_is_exclusive("; emit x; emitLn ") {"
  n.forM fun i _ => do
    emit " lean_ctor_release("; emit x; emit ", "; emit i; emitLn ");"
  emit " "; emitLhs z; emit x; emitLn ";"
  emitLn "} else {"
  emit " lean_dec_ref("; emit x; emitLn ");"
  emit " "; emitLhs z; emitLn "lean_box(0);"
  emitLn "}"

def emitReuse (z : VarId) (x : VarId) (c : CtorInfo) (updtHeader : Bool) (ys : Array Arg) : M Unit := do
  emit "if lean_is_scalar("; emit x; emitLn ") {"
  emit " "; emitLhs z; emitAllocCtor c; emitLn ";"
  emitLn "} else {"
  emit " "; emitLhs z; emit x; emitLn ";"
  if updtHeader then emit " lean_ctor_set_tag("; emit z; emit ", "; emit c.cidx; emitLn ");"
  emitLn "}"
  emitCtorSetArgs z ys

def emitProj (z : VarId) (i : Nat) (x : VarId) : M Unit := do
  emitLhs z; emit "lean_ctor_get("; emit x; emit ", "; emit i; emitLn ");"

def emitUProj (z : VarId) (i : Nat) (x : VarId) : M Unit := do
  emitLhs z; emit "lean_ctor_get_usize("; emit x; emit ", "; emit i; emitLn ");"

def emitSProj (z : VarId) (t : IRType) (n offset : Nat) (x : VarId) : M Unit := do
  emitLhs z
  let fn := match t with
    | IRType.float    => "lean_ctor_get_float"
    | IRType.float32  => "lean_ctor_get_float32"
    | IRType.uint8    => "lean_ctor_get_uint8"
    | IRType.uint16   => "lean_ctor_get_uint16"
    | IRType.uint32   => "lean_ctor_get_uint32"
    | IRType.uint64   => "lean_ctor_get_uint64"
    | _               => "lean_ctor_get_uint64"
  emit fn; emit "("; emit x; emit ", "; emitOffset n offset; emitLn ");"

def toStringArgs (ys : Array Arg) : List String :=
  ys.toList.map argToRustString

def emitSimpleExternalCall (f : String) (ps : Array Param) (ys : Array Arg) : M Unit := do
  emit f; emit "("
  discard <| ys.size.foldM
    (fun i _ (first : Bool) =>
      let ty := ps[i]!.ty
      if ty.isErased || ty.isVoid then
        pure first
      else do
        unless first do emit ", "
        emitArg ys[i]
        pure false)
    true
  emitLn ");"

def emitExternCall (f : FunId) (ps : Array Param) (extData : ExternAttrData) (ys : Array Arg) : M Unit :=
  match getExternEntryFor extData `c with
  | some (ExternEntry.standard _ extFn) => emitSimpleExternalCall extFn ps ys
  | some (ExternEntry.inline _ pat) => do emit (expandExternPattern pat (toStringArgs ys)); emitLn ";"
  | _ => throw s!"failed to emit extern application '{f}'"

def emitFullApp (z : VarId) (f : FunId) (ys : Array Arg) : M Unit := do
  emitLhs z
  let decl ← getDecl f
  match decl with
  | .fdecl (xs := ps) .. | .extern (xs := ps) (ext := { entries := [.opaque], .. }) .. =>
    if ps.size == 0 then
      -- Read from static variable (initialized by emitInitFn)
      emitRustName f
    else if ys.size > 0 then
      emitRustName f
      let (ys, _) := ys.zip ps |>.filter (fun (_, p) => !p.ty.isVoid) |>.unzip
      emit "("; emitArgs ys; emit ")"
    else
      emitRustName f
      emit "()"
    emitLn ";"
  | Decl.extern _ ps _ extData => emitExternCall f ps extData ys

def emitPartialApp (z : VarId) (f : FunId) (ys : Array Arg) : M Unit := do
  let decl ← getDecl f
  let arity := decl.params.size
  emitLhs z; emit "lean_alloc_closure("; emitRustName f; emit " as *const (), "; emit arity; emit ", "; emit ys.size; emitLn ");"
  ys.size.forM fun i _ => do
    let y := ys[i]
    emit "lean_closure_set("; emit z; emit ", "; emit i; emit ", "; emitArg y; emitLn ");"

def closureMaxArgs : Nat := 8

def emitApp (z : VarId) (f : VarId) (ys : Array Arg) : M Unit := do
  if ys.size > closureMaxArgs then
    emit "{ let _aargs: [*mut LeanObject; "; emit ys.size; emit "] = ["; emitArgs ys; emitLn "];"
    emitLhs z; emit "lean_apply_m("; emit f; emit ", "; emit ys.size; emitLn ", _aargs.as_ptr()); }"
  else
    emitLhs z; emit "lean_apply_"; emit ys.size; emit "("; emit f; emit ", "; emitArgs ys; emitLn ");"

def emitBoxFn (xType : IRType) : M Unit :=
  match xType with
  | IRType.usize   => emit "lean_box_usize"
  | IRType.uint32  => emit "lean_box_uint32"
  | IRType.uint64  => emit "lean_box_uint64"
  | IRType.float   => emit "lean_box_float"
  | IRType.float32 => emit "lean_box_float32"
  | _              => emit "lean_box"

def emitBox (z : VarId) (x : VarId) (xType : IRType) : M Unit := do
  emitLhs z; emitBoxFn xType; emit "("; emit x
  match xType with
  | IRType.uint8  => emit " as usize"
  | IRType.uint16 => emit " as usize"
  | _ => pure ()
  emitLn ");"

def emitUnbox (z : VarId) (t : IRType) (x : VarId) : M Unit := do
  emitLhs z
  emit (getUnboxOpName t)
  emit "("; emit x; emit ")"
  match t with
  | IRType.uint8  => emit " as u8"
  | IRType.uint16 => emit " as u16"
  | IRType.uint32 => emit " as u32"
  | _ => pure ()
  emitLn ";"

def emitIsShared (z : VarId) (x : VarId) : M Unit := do
  emitLhs z; emit "!lean_is_exclusive("; emit x; emitLn ") as u8;"

def hexDigit (n : UInt8) : Char :=
  if n < 10 then Char.ofNat (n.toNat + '0'.toNat)
  else Char.ofNat (n.toNat - 10 + 'a'.toNat)

def quoteString (s : String) : String :=
  let q := "\""
  let q := s.foldl
    (fun q c => q ++
      if c == '\n' then "\\n"
      else if c == '\r' then "\\r"
      else if c == '\t' then "\\t"
      else if c == '\\' then "\\\\"
      else if c == '\"' then "\\\""
      else if c.toNat < 32 then
        -- Control characters: use \x (always <= 0x1f, so valid in Rust)
        let bytes := String.singleton c |>.toUTF8
        bytes.foldl (fun acc b =>
          acc ++ "\\x" ++ String.singleton (hexDigit (b / 16)) ++ String.singleton (hexDigit (b % 16))) ""
      else if c.toNat > 126 then
        -- Non-ASCII: use \u{NNNN} Unicode escape (Rust-compatible)
        "\\u{" ++ String.ofList (Nat.toDigits 16 c.toNat) ++ "}"
      else String.singleton c)
    q
  q ++ "\""

def emitNumLit (t : IRType) (v : Nat) : M Unit := do
  if t.isObj then
    if v < UInt32.size then
      emit "lean_unsigned_to_nat("; emit v; emit ")"
    else
      emit "lean_cstr_to_nat(\""; emit v; emit "\")"
  else
    emit v

def emitLit (z : VarId) (t : IRType) (v : LitVal) : M Unit := do
  emitLhs z
  match v with
  | LitVal.num v => emitNumLit t v; emitLn ";"
  | LitVal.str v =>
    emit "lean_mk_string_unchecked("
    emit (quoteString v); emit ".as_ptr(), "
    emit v.utf8ByteSize; emit ", "
    emit v.length; emitLn ");"

def emitVDecl (z : VarId) (t : IRType) (v : Expr) : M Unit := do
  if t.isVoid then
    match v with
    | Expr.ctor _ _ | Expr.lit _ => pure ()
    | _ => emitVDeclNonVoid z t v
  else
    emitVDeclNonVoid z t v
where
  emitVDeclNonVoid (z : VarId) (t : IRType) (v : Expr) : M Unit :=
    match v with
    | Expr.ctor c ys      => emitCtor z c ys
    | Expr.reset n x      => emitReset z n x
    | Expr.reuse x c u ys => emitReuse z x c u ys
    | Expr.proj i x       => emitProj z i x
    | Expr.uproj i x      => emitUProj z i x
    | Expr.sproj n o x    => emitSProj z t n o x
    | Expr.fap c ys       => emitFullApp z c ys
    | Expr.pap c ys       => emitPartialApp z c ys
    | Expr.ap x ys        => emitApp z x ys
    | Expr.box t x        => emitBox z x t
    | Expr.unbox x        => emitUnbox z t x
    | Expr.isShared x     => emitIsShared z x
    | Expr.lit v          => emitLit z t v

def isTailCall (x : VarId) (v : Expr) (b : FnBody) : M Bool := do
  let ctx ← read
  match v, b with
  | Expr.fap f _, FnBody.ret (.var y) => return f == ctx.mainFn && x == y
  | _, _ => pure false

def paramEqArg (p : Param) (x : Arg) : Bool :=
  match x with
  | .var x => p.x == x
  | .erased => false

/--
Check whether any parameter `p_i` appears as argument `y_j` for some `j > i`.
If so, sequential assignment `p_i = y_i; ...; p_j = p_i` would clobber `p_i`.
-/
def overwriteParam (ps : Array Param) (ys : Array Arg) : Bool :=
  let n := ps.size
  n.any fun i _ =>
    let p := ps[i]
    (i+1, n).anyI fun j _ _ => paramEqArg p ys[j]!

def emitTailCall (v : Expr) : M Unit :=
  match v with
  | Expr.fap _ ys => do
    let ctx ← read
    let ps := ctx.mainParams
    if h : ps.size = ys.size then
      let (ps, ys) := ps.zip ys |>.filter (fun (p, _) => !p.ty.isVoid) |>.unzip
      if overwriteParam ps ys then
        emitLn "{"
        ps.size.forM fun i _ => do
          let p := ps[i]
          let y := ys[i]!
          unless paramEqArg p y do
            emit "let _tmp_"; emit i; emit ": "; emit (toRustType p.ty); emit " = "; emitArg y; emitLn ";"
        ps.size.forM fun i _ => do
          let p := ps[i]
          let y := ys[i]!
          unless paramEqArg p y do emit p.x; emit " = _tmp_"; emit i; emitLn ";"
        emitLn "}"
      else
        ps.size.forM fun i _ => do
          let p := ps[i]
          let y := ys[i]!
          unless paramEqArg p y do emit p.x; emit " = "; emitArg y; emitLn ";"
      if ctx.hasJPs then
        emitLn "_jp_state = 0; continue '_start;"
      else
        emitLn "continue '_start;"
    else
      throw "invalid tail call"
  | _ => throw "bug at emitTailCall"

mutual

partial def emitCase (x : VarId) (xType : IRType) (alts : Array Alt) : M Unit := do
  emit "match "; emitTag x xType; emitLn " {"
  let alts := ensureHasDefault alts
  alts.forM fun alt => do
    match alt with
    | Alt.ctor c b  => emit c.cidx; emitLn " => {"; emitBlock b; emitLn "}"
    | Alt.default b => emitLn "_ => {"; emitBlock b; emitLn "}"
  emitLn "}"

partial def emitBlock (b : FnBody) : M Unit := do
  match b with
  | FnBody.jdecl _ _ _ b         => emitBlock b
  | d@(FnBody.vdecl x t v b)     =>
    let ctx ← read
    if isTailCallTo ctx.mainFn d then
      emitTailCall v
    else
      emitVDecl x t v
      emitBlock b
  | FnBody.inc x n c p b         =>
    unless p do emitInc x n c
    emitBlock b
  | FnBody.dec x n c p b         =>
    unless p do emitDec x n c
    emitBlock b
  | FnBody.del x b               => emitDel x; emitBlock b
  | FnBody.setTag x i b          => emitSetTag x i; emitBlock b
  | FnBody.set x i y b           => emitSet x i y; emitBlock b
  | FnBody.uset x i y b          => emitUSet x i y; emitBlock b
  | FnBody.sset x i o y t b      => emitSSet x i o y t; emitBlock b
  | FnBody.ret x                 => emit "return "; emitArg x; emitLn ";"
  | FnBody.case _ x xType alts   => emitCase x xType alts
  | FnBody.jmp j xs              => emitJmp j xs
  | FnBody.unreachable           => emitLn "unreachable!();"

partial def emitJPBody (j : JoinPointId) : FnBody → M Unit
  | FnBody.jdecl j' _ v b => do
    if j == j' then emitBlock v
    else do
      emitJPBody j v
      emitJPBody j b
  | FnBody.case _ _ _ alts => do
    for alt in alts do
      match alt with
      | Alt.ctor _ body => emitJPBody j body
      | Alt.default body => emitJPBody j body
  | e => unless e.isTerminal do emitJPBody j e.body

end

def emitFnBodyWithJPs (b : FnBody) (jps : Array JoinPointId) : M Unit := do
  if jps.isEmpty then
    emitBlock b
  else
    emitLn "match _jp_state {"
    emitLn "0 => {"
    emitBlock b
    emitLn "}"
    jps.forM fun j => do
      emit j.idx; emitLn " => {"
      emitJPBody j b
      emitLn "}"
    emitLn "_ => unreachable!()"
    emitLn "}"

def emitDeclAux (d : Decl) : M Unit := do
  let env ← getEnv
  let (_, jpMap) := mkVarJPMaps d
  withReader (fun ctx => { ctx with jpMap := jpMap }) do
  unless hasInitAttr env d.name do
    match d with
    | .fdecl (f := f) (xs := xs) (type := t) (body := b) .. =>
      let baseName ← toRustName f
      emit "pub unsafe fn "
      if xs.size > 0 then
        let xs := xs.filter (fun p => !p.ty.isVoid)
        emit baseName
        emit "("
        xs.size.forM fun i _ => do
          if i > 0 then emit ", "
          let x := xs[i]
          emit "mut "; emit x.x; emit ": "; emit (toRustType x.ty)
        emit ")"
      else
        emit "_init_"; emit baseName; emit "()"
      emit " -> "; emit (toRustType t); emitLn " {"
      let jps := collectJoinPoints b #[]
      let hasJPs := !jps.isEmpty
      if hasJPs then
        emitLn "let mut _jp_state: usize = 0;"
      withReader (fun ctx => { ctx with mainFn := f, mainParams := xs, hasJPs := hasJPs }) do
        declareVars b
        emitLn "'_start: loop {"
        emitFnBodyWithJPs b jps
        emitLn "}"
      emitLn "}"
    | _ => pure ()

def emitDecl (d : Decl) : M Unit := do
  let d := d.normalizeIds
  try
    emitDeclAux d
  catch err =>
    throw s!"{err}\ncompiling:\n{d}"

def emitFns : M Unit := do
  let env ← getEnv
  let decls := getDecls env
  decls.reverse.forM emitDecl

/-- Emit `pub static mut` declarations for zero-parameter declarations
    (closed constants). These are initialized once by `emitInitFn`. -/
def emitStaticDecls : M Unit := do
  let env ← getEnv
  let decls := getDecls env
  decls.reverse.forM fun d => do
    unless hasInitAttr env d.name do
      if d.params.size == 0 then
        emit "pub static mut "; emitRustName d.name
        emit ": "; emit (toRustType d.resultType)
        emit " = "; emit (defaultValueForType d.resultType); emitLn ";"
  emitLn ""

/-- Mark a declaration's static variable as persistent if it has object type. -/
def emitMarkPersistent (d : Decl) : M Unit := do
  if d.resultType.isObj then
    emit "if !lean_is_scalar("; emitRustName d.name; emit ") { lean_mark_persistent("; emitRustName d.name; emitLn "); }"

/-- Emit initialization code for a single declaration inside `emitInitFn`.
    Mirrors EmitC.lean's `emitDeclInit`. -/
def emitDeclInit (d : Decl) : M Unit := do
  let env ← getEnv
  let n := d.name
  if isIOUnitInitFn env n then
    if isIOUnitBuiltinInitFn env n then
      emitLn "if builtin != 0 {"
    emit "res = "; emitRustName n; emitLn "();"
    emitLn "if !lean_io_result_is_ok(res) { return res; }"
    emitLn "lean_dec_ref(res);"
    if isIOUnitBuiltinInitFn env n then
      emitLn "}"
  else if d.params.size == 0 then
    match getInitFnNameFor? env d.name with
    | some initFn =>
      if getBuiltinInitFnNameFor? env d.name |>.isSome then
        emitLn "if builtin != 0 {"
      emit "res = "; emitRustName initFn; emitLn "();"
      emitLn "if !lean_io_result_is_ok(res) { return res; }"
      emitRustName n
      if d.resultType.isScalar then
        emit " = "; emit (getUnboxOpName d.resultType); emitLn "(lean_io_result_get_value(res));"
      else
        emitLn " = lean_io_result_get_value(res);"
        emitMarkPersistent d
      emitLn "lean_dec_ref(res);"
      if getBuiltinInitFnNameFor? env d.name |>.isSome then
        emitLn "}"
    | _ =>
      emitRustName n; emit " = _init_"; emitRustName n; emitLn "();"
      emitMarkPersistent d

/-- Emit the module initialization function. Each module gets one.
    Guards against re-initialization, calls imported module inits,
    then initializes local closed constants. Mirrors EmitC.lean's `emitInitFn`. -/
def emitInitFn : M Unit := do
  let env ← getEnv
  let modInitFn ← getModInitFn
  -- Determine if we have external Lean-defined declarations (multi-module mode)
  let decls := getDecls env
  let modDecls : NameSet := decls.foldl (fun s d => s.insert d.name) {}
  let usedDecls : NameSet := decls.foldl (fun s d => collectUsedDecls env d (s.insert d.name)) {}
  let externDecls := usedDecls.toList.filter (fun n => !modDecls.contains n)
  let mut hasExternalLeanDecls := false
  for n in externDecls do
    let decl ← getDecl n
    -- Check if this declaration is a C extern or a boxed wrapper of one
    let isCExtern := (getExternNameFor env `c decl.name).isSome
    let isBoxedCExtern := n.isStr && n.getString! == "_boxed" &&
      (getExternNameFor env `c n.getPrefix).isSome
    unless isCExtern || isBoxedCExtern do
      hasExternalLeanDecls := true; break
  -- Collect imported module init functions (only in multi-module mode)
  let mut impInitCalls : Array (String × String) := #[]  -- (rustModIdent, initFnName)
  if hasExternalLeanDecls then
    for imp in env.imports do
      let some idx := env.getModuleIdx? imp.module
        | throw "(internal) import without module index"
      let pkg? := env.getModulePackageByIdx? idx
      let fn := mkModuleInitializationFunctionName imp.module pkg?
      let rustMod := moduleNameToRustIdent imp.module
      impInitCalls := impInitCalls.push (rustMod, fn)
  -- Emit the init function
  emitLn ""
  emitLn "static mut G_INITIALIZED: bool = false;"
  emit "pub unsafe fn "; emit modInitFn; emitLn "(builtin: u8) -> *mut LeanObject {"
  emitLn "if G_INITIALIZED { return lean_io_result_mk_ok(lean_box(0)); }"
  emitLn "G_INITIALIZED = true;"
  emitLn "let mut res: *mut LeanObject;"
  -- Call imported module init functions
  for (rustMod, fn) in impInitCalls do
    emit "res = crate::"; emit rustMod; emit "::"; emit fn; emitLn "(builtin);"
    emitLn "if !lean_io_result_is_ok(res) { return res; }"
    emitLn "lean_dec_ref(res);"
  -- Initialize local declarations
  decls.reverse.forM emitDeclInit
  emitLn "lean_io_result_mk_ok(lean_box(0))"
  emitLn "}"

/-- Emit `use crate::module::*` imports for functions defined in imported modules
    that are referenced by the current module's function bodies. In multi-module
    mode, each module is a separate .rs file within a Cargo crate, and cross-module
    references use Rust's module system. -/
def emitExternDecls : M Unit := do
  let env ← getEnv
  let decls := getDecls env
  -- Build set of names defined in this module
  let modDecls : NameSet := decls.foldl (fun s d => s.insert d.name) {}
  -- Collect all function names referenced by this module's function bodies
  let usedDecls : NameSet := decls.foldl (fun s d => collectUsedDecls env d (s.insert d.name)) {}
  -- Find imported declarations (used but not defined locally)
  let externDecls := usedDecls.toList.filter (fun n => !modDecls.contains n)
  -- Collect unique source modules for non-C-extern declarations
  let mut moduleNames : NameSet := {}
  for n in externDecls do
    let decl ← getDecl n
    -- Check if this declaration is a C extern (e.g. @[extern "lean_nat_dec_eq"])
    let isCExtern := (getExternNameFor env `c decl.name).isSome
    -- Check if this is a boxed wrapper of a C extern (e.g. Nat.decEq._boxed)
    let isBoxedCExtern := n.isStr && n.getString! == "_boxed" &&
      (getExternNameFor env `c n.getPrefix).isSome
    if isCExtern || isBoxedCExtern then
      pure ()  -- C runtime function (or its boxed wrapper), provided by lean_runtime crate
    else
      match env.getModuleIdxFor? n with
      | some idx =>
        if h : idx.toNat < env.header.moduleNames.size then
          let modName := env.header.moduleNames[idx.toNat]
          moduleNames := moduleNames.insert modName
      | none => pure ()
  -- Emit use declarations for each source module
  for modName in moduleNames.toList do
    emit "use crate::"; emit (moduleNameToRustIdent modName); emitLn "::*;"
  unless moduleNames.isEmpty do emitLn ""

def emitFileHeader : M Unit := do
  let env ← getEnv
  let modName ← getModName
  emitLn "// Lean compiler output (Rust backend)"
  emit "// Module: "; emitLn modName
  emit "// Imports:"
  env.imports.forM fun m => emit (" " ++ toString m.module)
  emitLn ""
  emitLn "#![allow(unused_variables)]"
  emitLn "#![allow(unused_mut)]"
  emitLn "#![allow(unused_parens)]"
  emitLn "#![allow(non_snake_case)]"
  emitLn "#![allow(non_upper_case_globals)]"
  emitLn "#![allow(dead_code)]"
  emitLn "#![allow(unreachable_code)]"
  emitLn "#![allow(unused_assignments)]"
  emitLn "#![allow(unused_unsafe)]"
  emitLn ""
  emitLn "use lean_runtime::*;"
  emitLn ""

def emitMainFn : M Unit := do
  let d ← getDecl `main
  let modInitFn ← getModInitFn
  match d with
  | .fdecl (xs := xs) .. => do
    let realParams := xs.filter (fun p => !p.ty.isVoid)
    let hasArgs := realParams.size >= 2
    emitLn ""
    emitLn "pub fn main() {"
    emitLn "    unsafe {"
    -- Initialize module (and transitively all imports)
    emitLn "        lean_set_panic_messages(0);"
    emit "        let res = "; emit modInitFn; emitLn "(1);"
    emitLn "        lean_set_panic_messages(1);"
    emitLn "        lean_io_mark_end_initialization();"
    emitLn "        if !lean_io_result_is_ok(res) {"
    emitLn "            lean_io_result_show_error(res);"
    emitLn "            lean_dec_ref(res);"
    emitLn "            std::process::exit(1);"
    emitLn "        }"
    emitLn "        lean_dec_ref(res);"
    -- Call the Lean main function
    if hasArgs then
      emitLn "        let args: Vec<String> = std::env::args().collect();"
      emitLn "        let mut in_list = lean_box(0);"
      emitLn "        for arg in args.into_iter().rev() {"
      emitLn "            let s = lean_mk_string(&arg);"
      emitLn "            let cons = lean_alloc_ctor(1, 2, 0);"
      emitLn "            lean_ctor_set(cons, 0, s);"
      emitLn "            lean_ctor_set(cons, 1, in_list);"
      emitLn "            in_list = cons;"
      emitLn "        }"
      emit "        let res = "; emit leanMainFn; emitLn "(in_list);"
    else
      emit "        let res = "; emit leanMainFn; emitLn "();"
    emitLn "        if !lean_io_result_is_ok(res) {"
    emitLn "            lean_io_result_show_error(res);"
    emitLn "            lean_dec_ref(res);"
    emitLn "            std::process::exit(1);"
    emitLn "        }"
    emitLn "        lean_dec(res);"
    emitLn "    }"
    emitLn "}"
  | _ => throw "function declaration expected"

def hasMainFn : M Bool := do
  let env ← getEnv
  let decls := getDecls env
  return decls.any (fun d => d.name == `main)

def emitMainFnIfNeeded : M Unit := do
  if (← hasMainFn) then emitMainFn

def main : M Unit := do
  emitFileHeader
  emitStaticDecls
  emitExternDecls
  emitFns
  emitInitFn
  emitMainFnIfNeeded

end EmitRust

def emitRust (env : Environment) (modName : Name) : Except String String :=
  match EmitRust.main { env, modName } |>.run "" with
  | EStateM.Result.ok _ s => Except.ok s
  | EStateM.Result.error err _ => Except.error err

end Lean.IR
