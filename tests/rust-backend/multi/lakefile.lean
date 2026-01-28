import Lake
open Lake DSL

package multi_test where
  leanOptions := #[]

lean_lib MultiLib where
  backend := .rust

@[default_target]
lean_exe multi where
  root := `MultiApp
  backend := .rust
