/-
Copyright (c) 2022 Mac Malone. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Mac Malone
-/
module

prelude
public import Lake.Config.FacetConfig
import Lake.Build.Job.Register
import Lake.Build.Target.Fetch
import Lake.Build.Common
import Lake.Build.Infos

namespace Lake
open System (FilePath)

/-! # Lean Executable Build
The build function definition for a Lean executable.
-/

private def LeanExe.recBuildExe (self : LeanExe) : FetchM (Job FilePath) :=
  withRegisterJob s!"{self.name}:exe" <| withCurrPackage self.pkg do
  /-
  Remark: We must build the root before we fetch the transitive imports
  so that errors in the import block of transitive imports will not kill this
  job before the root is built.
  -/
  -- Check if we're using the Rust backend
  if self.root.backend == .rust then
    -- Rust backend: collect .rs files and compile with rustc
    let entryRsJob ← self.root.rs.fetch
    let .ok imports _ ← (← self.root.transImports.fetch).wait
      | error s!"bad imports (see the '{self.root.name.toString}' job for details)"
    let mut importRsJobs := #[]
    for mod in imports do
      importRsJobs := importRsJobs.push <| ← mod.rs.fetch
    buildRustExe self.file entryRsJob importRsJobs
  else if self.root.backend == .vm then
    -- VM backend: collect .leanbc files (lake exe handles invocation)
    let entryVmJob ← self.root.vm.fetch
    let .ok imports _ ← (← self.root.transImports.fetch).wait
      | error s!"bad imports (see the '{self.root.name.toString}' job for details)"
    let mut importVmJobs := #[]
    for mod in imports do
      importVmJobs := importVmJobs.push <| ← mod.vm.fetch
    buildVmExe entryVmJob importVmJobs
  else
    -- C/LLVM backend: collect object files and link with cc
    let mut objJobs := #[]
    let mut libJobs := #[]
    let shouldExport := self.supportInterpreter
    for facet in self.root.nativeFacets shouldExport do
      objJobs := objJobs.push <| ← facet.fetch self.root
    let .ok imports _ ← (← self.root.transImports.fetch).wait
      | error s!"bad imports (see the '{self.root.name.toString}' job for details)"
    for mod in imports do
      for facet in mod.nativeFacets shouldExport do
        objJobs := objJobs.push <| ← facet.fetch mod
    for link in self.moreLinkObjs do
      objJobs := objJobs.push <| ← link.fetchIn self.pkg
    let libs := imports.foldl (·.insert ·.lib) OrdHashSet.empty |>.toArray
    for lib in libs do
      for link in lib.moreLinkObjs do
        objJobs := objJobs.push <| ← link.fetchIn lib.pkg
      for link in lib.moreLinkLibs do
        libJobs := libJobs.push <| ← link.fetchIn lib.pkg
    for link in self.moreLinkLibs do
      libJobs := libJobs.push <| ← link.fetchIn self.pkg
    let deps := (← (← self.pkg.transDeps.fetch).await).push self.pkg
    for dep in deps do
      for lib in dep.externLibs do
        objJobs := objJobs.push <| ← lib.static.fetch
    buildLeanExe self.file objJobs libJobs self.weakLinkArgs self.linkArgs self.sharedLean

/-- The facet configuration for the builtin `LeanExe.exeFacet`. -/
public def LeanExe.exeFacetConfig : LeanExeFacetConfig exeFacet :=
  mkFacetJobConfig recBuildExe

private def LeanExe.recBuildDefault (lib : LeanExe) : FetchM (Job FilePath) :=
  lib.exe.fetch

/-- The facet configuration for the builtin `ExternLib.dynlibFacet`. -/
public def LeanExe.defaultFacetConfig : LeanExeFacetConfig defaultFacet :=
  mkFacetJobConfig recBuildDefault (memoize := false)

/--
A name-configuration map for the initial set of
Lean executable facets (e.g., `exe`).
-/
public def LeanExe.initFacetConfigs : DNameMap LeanExeFacetConfig :=
  DNameMap.empty
  |>.insert defaultFacet defaultFacetConfig
  |>.insert exeFacet exeFacetConfig
