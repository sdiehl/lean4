-- Multi-module test: library module
namespace MultiLib

def double (n : Nat) : Nat := n * 2

def greet (name : String) : String :=
  "Hello, " ++ name ++ "!"

def sumTo : Nat → Nat
  | 0 => 0
  | n + 1 => (n + 1) + sumTo n

-- Custom nat-to-string to avoid Init.Data.Repr dependency
def natToStr : Nat → String
  | 0 => "0"
  | 1 => "1"
  | 2 => "2"
  | 10 => "10"
  | 21 => "21"
  | 42 => "42"
  | 55 => "55"
  | _ => "?"

end MultiLib
