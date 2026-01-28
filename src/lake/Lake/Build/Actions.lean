/-
Copyright (c) 2017 Microsoft Corporation. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Gabriel Ebner, Sebastian Ullrich, Mac Malone, Siddharth Bhat
-/
module

prelude
public import Lake.Util.Log
import Lake.Config.Dynlib
import Lake.Util.Proc
import Lake.Util.NativeLib
import Lake.Util.FilePath
import Lake.Util.IO
import Init.Data.String.Search

/-! # Common Build Actions
Low level actions to build common Lean artifacts via the Lean toolchain.
-/

open System
open Lean hiding SearchPath

namespace Lake

public def compileLeanModule
  (leanFile relLeanFile : FilePath)
  (setup : ModuleSetup) (setupFile : FilePath)
  (arts : ModuleArtifacts)
  (leanArgs : Array String := #[])
  (leanPath : SearchPath := [])
  (lean : FilePath := "lean")
: LogIO Unit := do
  let mut args := leanArgs.push leanFile.toString
  if let some oleanFile := arts.olean? then
    createParentDirs oleanFile
    args := args ++ #["-o", oleanFile.toString]
  if let some ileanFile := arts.ilean? then
    createParentDirs ileanFile
    args := args ++ #["-i", ileanFile.toString]
  if let some cFile := arts.c? then
    createParentDirs cFile
    args := args ++ #["-c", cFile.toString]
  if let some bcFile := arts.bc? then
    createParentDirs bcFile
    args := args ++ #["-b", bcFile.toString]
  if let some rsFile := arts.rs? then
    createParentDirs rsFile
    args := args ++ #["--rust", rsFile.toString]
  if let some vmFile := arts.vm? then
    createParentDirs vmFile
    args := args ++ #["-Y", vmFile.toString]
  createParentDirs setupFile
  IO.FS.writeFile setupFile (toJson setup).pretty
  args := args ++ #["--setup", setupFile.toString]
  args := args.push "--json"
  withLogErrorPos do
  let out ← rawProc {
    args
    cmd := lean.toString
    env := #[
      ("LEAN_PATH", leanPath.toString)
    ]
  }
  unless out.stdout.isEmpty do
    let txt ← out.stdout.split '\n' |>.foldM (init := "") fun (txt : String) ln => do
      let ln := ln.copy
      if let .ok (msg : SerialMessage) := Json.parse ln >>= fromJson? then
        unless txt.isEmpty do
          logInfo s!"stdout:\n{txt}"
        let msg := {msg with fileName := mkRelPathString relLeanFile}
        logSerialMessage msg
        return txt
      else if txt.isEmpty && ln.isEmpty then
        return txt
      else
        return txt ++ ln ++ "\n"
    unless txt.isEmpty do
      logInfo s!"stdout:\n{txt}"
  unless out.stderr.isEmpty do
    logInfo s!"stderr:\n{out.stderr.trimAscii}"
  if out.exitCode ≠ 0 then
    error s!"Lean exited with code {out.exitCode}"

public def compileO
  (oFile srcFile : FilePath)
  (moreArgs : Array String := #[]) (compiler : FilePath := "cc")
: LogIO Unit := do
  createParentDirs oFile
  proc {
    cmd := compiler.toString
    args := #["-c", "-o", oFile.toString, srcFile.toString] ++ moreArgs
  }

public def mkArgs (basePath : FilePath) (args : Array String) : LogIO (Array String) := do
  if Platform.isWindows then
    -- Use response file to avoid potentially exceeding CLI length limits.
    let rspFile := basePath.addExtension "rsp"
    let h ← IO.FS.Handle.mk rspFile .write
    args.forM fun arg =>
      -- Escape special characters
      let arg := arg.foldl (init := "") fun s c =>
        if c == '\\' || c == '"' then
          s.push '\\' |>.push c
        else
          s.push c
      h.putStr s!"\"{arg}\"\n"
    return #[s!"@{rspFile}"]
  else
    return args

public def compileStaticLib
  (libFile : FilePath) (oFiles : Array FilePath)
  (ar : FilePath := "ar") (thin := false)
: LogIO Unit := do
  createParentDirs libFile
  -- `ar rcs` does not remove old files from the archive, so it must be deleted first
  removeFileIfExists libFile
  let args := #["rcs"]
  let args := if thin then args.push "--thin" else args
  let args := args.push libFile.toString ++ (← mkArgs libFile <| oFiles.map toString)
  proc {cmd := ar.toString, args}

private def getMacOSXDeploymentEnv : BaseIO (Array (String × Option String)) := do
  -- It is difficult to identify the correct minor version here, leading to linking warnings like:
  -- `ld64.lld: warning: /usr/lib/system/libsystem_kernel.dylib has version 13.5.0, which is newer than target minimum of 13.0.0`
  -- In order to suppress these we set the MACOSX_DEPLOYMENT_TARGET variable into the far future.
  if System.Platform.isOSX then
    match (← IO.getEnv "MACOSX_DEPLOYMENT_TARGET") with
    | some _ => return #[]
    | none => return #[("MACOSX_DEPLOYMENT_TARGET", some "99.0")]
  else
    return #[]

public def compileSharedLib
  (libFile : FilePath) (linkArgs : Array String) (linker : FilePath := "cc")
: LogIO Unit := do
  createParentDirs libFile
  proc {
    cmd := linker.toString
    args := #["-shared", "-o", libFile.toString] ++ (← mkArgs libFile linkArgs)
    env := ← getMacOSXDeploymentEnv
  }

public def compileExe
  (binFile : FilePath) (linkArgs : Array String) (linker : FilePath := "cc")
: LogIO Unit := do
  createParentDirs binFile
  proc {
    cmd := linker.toString
    args := #["-o", binFile.toString] ++ (← mkArgs binFile linkArgs)
    env := ← getMacOSXDeploymentEnv
  }

/--
Compile multiple Rust source files into an executable using `rustc`.

This is used by the Rust backend to compile all module .rs files together.
The function:
1. Creates a temporary build directory
2. Copies all .rs files, prepending `mod` declarations to the entry module
3. Optionally runs rustfmt on the generated code
4. Invokes rustc with the lean-runtime as an external crate

The entry module (the one containing `fn main()`) becomes the crate root.
Other modules are declared via `mod` at the top of the entry module so that
`use crate::other_module::*` imports work correctly.

Parameters:
- `rustfmt`: If true (default), run rustfmt on generated code before compilation
- `keepArtifacts`: If true, keep the .rust-build directory after compilation for debugging
- `initRustDir`: Directory containing pre-compiled Init .rs files (optional)
-/
public def compileRustExe
  (binFile : FilePath) (entryRsFile : FilePath) (importRsFiles : Array FilePath)
  (runtimeLib : FilePath) (runtimeDeps : FilePath)
  (rustc : FilePath := "rustc")
  (rustfmt : Bool := true)
  (keepArtifacts : Bool := false)
  (initRustDir : Option FilePath := none)
: LogIO Unit := do
  createParentDirs binFile
  -- Create a temporary build directory for the Rust crate
  let buildDir := binFile.parent.getD "." / ".rust-build"
  IO.FS.createDirAll buildDir
  -- Generate mod declarations for all imported modules (user modules)
  let mut modDecls := ""
  for rsFile in importRsFiles do
    let modName := rsFile.fileStem.getD "unknown"
    modDecls := modDecls ++ s!"#[path = \"{rsFile}\"]\nmod {modName};\n"
  -- Add Init modules if initRustDir is provided
  if let some initDir := initRustDir then
    let initFiles ← initDir.readDir
    for entry in initFiles do
      if entry.path.extension == some "rs" then
        let modName := entry.path.fileStem.getD "unknown"
        modDecls := modDecls ++ s!"#[path = \"{entry.path}\"]\nmod {modName};\n"
  -- Create main.rs with inner attributes, mod declarations, then stripped entry content
  -- We provide our own standard set of inner attributes and strip them from the entry module
  let mainRs := buildDir / "main.rs"
  let entryContent ← IO.FS.readFile entryRsFile
  -- Standard inner attributes for Lean-generated Rust code
  let stdAttrs := "#![allow(unused_variables)]\n#![allow(unused_mut)]\n#![allow(unused_parens)]\n#![allow(non_snake_case)]\n#![allow(non_upper_case_globals)]\n#![allow(dead_code)]\n#![allow(unreachable_code)]\n#![allow(unused_assignments)]\n#![allow(unused_unsafe)]\n"
  -- Strip the inner attributes from entryContent by finding where "use lean_runtime" starts
  -- and taking everything from there
  let mut startPos := 0
  let target := "use lean_runtime"
  let targetLen := target.length
  for i in [0:entryContent.length] do
    if i + targetLen <= entryContent.length then
      let mut found := true
      for j in [0:targetLen] do
        if String.Pos.Raw.get entryContent ⟨i + j⟩ != String.Pos.Raw.get target ⟨j⟩ then
          found := false
          break
      if found then
        startPos := i
        break
  let strippedContent := entryContent.drop startPos
  let mainContent := s!"{stdAttrs}\n// Module declarations for cross-module imports (generated by Lake)\n{modDecls}\n{strippedContent}"
  IO.FS.writeFile mainRs mainContent
  -- Optionally format with rustfmt (ignore errors if rustfmt is not installed)
  if rustfmt then
    let fmtResult ← IO.Process.output {cmd := "rustfmt", args := #[mainRs.toString]}
    if fmtResult.exitCode != 0 then
      -- rustfmt failed or not installed - continue without formatting
      IO.eprintln s!"warning: rustfmt failed (exit code {fmtResult.exitCode}), continuing without formatting"
  -- Compile with rustc
  let args := #[
    mainRs.toString,
    "--edition", "2021",
    "--extern", s!"lean_runtime={runtimeLib}",
    "-L", runtimeDeps.toString,
    "-o", binFile.toString
  ]
  proc {cmd := rustc.toString, args}
  -- Clean up build directory unless keepArtifacts is set
  if !keepArtifacts then
    IO.FS.removeDirAll buildDir

/--
Run a Lean program using the bytecode VM.

This function invokes `lean4-vm` with the given bytecode files. The entry module
is the main program, and import modules are loaded first using `-I` flags.

The VM is found in one of these locations (in order):
1. LEAN4_VM environment variable
2. Bundled in sysroot at `lib/lean4-vm/lean4-vm`
3. `lean4-vm` in PATH
-/
public def runVmExe
  (entryVmFile : FilePath) (importVmFiles : Array FilePath)
  (programArgs : Array String := #[])
: LogIO UInt32 := do
  -- Find lean4-vm
  let vmExe ← do
    if let some path ← IO.getEnv "LEAN4_VM" then
      pure <| FilePath.mk path
    else
      -- Try bundled location or PATH
      pure <| FilePath.mk "lean4-vm"
  -- Build command: lean4-vm -I import1 -I import2 ... entry args...
  let mut args : Array String := #[]
  for imp in importVmFiles do
    args := args ++ #["-I", imp.toString]
  args := args.push entryVmFile.toString
  args := args ++ programArgs
  let child ← IO.Process.spawn {
    cmd := vmExe.toString
    args
    stdin := .inherit
    stdout := .inherit
    stderr := .inherit
  }
  child.wait

/-- Download a file using `curl`, clobbering any existing file. -/
public def download
  (url : String) (file : FilePath) (headers : Array String := #[])
: LogIO PUnit := do
  if (← file.pathExists) then
    IO.FS.removeFile file
  else
    createParentDirs file
  let args := #["-s", "-S", "-f", "-o", file.toString, "-L", url]
  let args := headers.foldl (init := args) (· ++ #["-H", ·])
  proc (quiet := true) {cmd := "curl", args}

/-- Unpack an archive `file` using `tar` into the directory `dir`. -/
public def untar (file : FilePath) (dir : FilePath) (gzip := true) : LogIO PUnit := do
  IO.FS.createDirAll dir
  let mut opts := "-xvv"
  if gzip then
    opts := opts.push 'z'
  proc (quiet := true) {
    cmd := "tar",
    args := #[opts, "-f", file.toString, "-C", dir.toString]
  }

/-- Pack a directory `dir` using `tar` into the archive `file`. -/
public def tar
  (dir : FilePath) (file : FilePath)
  (gzip := true) (excludePaths : Array FilePath := #[])
: LogIO PUnit := do
  createParentDirs file
  let mut args := #["-cvv"]
  if gzip then
    args := args.push "-z"
  for path in excludePaths do
    args := args.push s!"--exclude={path}"
  proc (quiet := true) {
    cmd := "tar"
    args := args ++ #["-f", file.toString, "-C", dir.toString, "."]
    -- don't pack `._` files on MacOS
    env := if Platform.isOSX then #[("COPYFILE_DISABLE", "true")] else #[]
  }
