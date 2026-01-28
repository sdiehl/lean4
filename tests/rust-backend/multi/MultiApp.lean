-- Multi-module test: application module
import MultiLib

def main : IO Unit := do
  IO.println (MultiLib.greet "World")
  IO.println (MultiLib.natToStr (MultiLib.double 21))
  IO.println (MultiLib.natToStr (MultiLib.sumTo 10))
