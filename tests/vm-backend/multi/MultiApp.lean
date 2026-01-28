-- Multi-module test: application module
import MultiLib

def main : IO Unit := do
  IO.println (MultiLib.greet "World")
  IO.println s!"{MultiLib.double 21}"
  IO.println s!"{MultiLib.sumTo 10}"
