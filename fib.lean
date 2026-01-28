def fib : Nat → Nat
  | 0     => 0
  | 1     => 1
  | n + 2 => fib n + fib (n + 1)

#eval fib 0   -- 0
#eval fib 1   -- 1
#eval fib 5   -- 5
#eval fib 10  -- 55

def main : IO Unit := do
  IO.println s!"Fibonacci of 0: {fib 0}"
  IO.println s!"Fibonacci of 1: {fib 1}"
  IO.println s!"Fibonacci of 5: {fib 5}"
  IO.println s!"Fibonacci of 10: {fib 10}"
