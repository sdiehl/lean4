-- Multi-module test: library module
namespace MultiLib

def double (n : Nat) : Nat := n * 2

def greet (name : String) : String :=
  "Hello, " ++ name ++ "!"

def sumTo : Nat → Nat
  | 0 => 0
  | n + 1 => (n + 1) + sumTo n

end MultiLib
