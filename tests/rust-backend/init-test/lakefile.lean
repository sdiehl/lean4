import Lake
open Lake DSL

package init_test

@[default_target]
lean_exe init_test where
  root := `Main
  backend := .rust
