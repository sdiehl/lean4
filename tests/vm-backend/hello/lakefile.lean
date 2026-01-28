import Lake
open Lake DSL

package hello_test where
  leanOptions := #[]

@[default_target]
lean_exe hello where
  root := `Main
  backend := .vm
