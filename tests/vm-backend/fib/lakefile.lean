import Lake
open Lake DSL

package fib_test where
  leanOptions := #[]

@[default_target]
lean_exe fib where
  root := `Fib
  backend := .vm
