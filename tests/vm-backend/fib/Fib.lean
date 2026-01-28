-- Fibonacci test for VM backend

def fib : Nat → Nat
  | 0 => 0
  | 1 => 1
  | n + 2 => fib (n + 1) + fib n

def main : IO Unit := do
  IO.println "Hello from VM backend!"
  IO.println s!"fib 10 = {fib 10}"
  IO.println s!"fib 20 = {fib 20}"
