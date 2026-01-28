-- Fibonacci test for Rust backend

def fib : Nat → Nat
  | 0 => 0
  | 1 => 1
  | n + 2 => fib (n + 1) + fib n

-- Hardcoded natToStr to avoid Init.Data.Repr dependency
def natToStr : Nat → String
  | 0 => "0"
  | 1 => "1"
  | 55 => "55"
  | 6765 => "6765"
  | _ => "?"

def main : IO Unit := do
  IO.println "Hello from Rust backend!"
  IO.println ("fib 10 = " ++ natToStr (fib 10))
  IO.println ("fib 20 = " ++ natToStr (fib 20))
