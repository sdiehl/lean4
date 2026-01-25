def fib : Nat → Nat
  | 0 => 0
  | 1 => 1
  | n + 2 => fib (n + 1) + fib n

def main : IO Unit := do
  IO.println "Fibonacci sequence:"
  IO.println "fib 0 = 0"
  IO.println "fib 1 = 1"
  IO.println "fib 2 = 1"
  IO.println "fib 3 = 2"
  IO.println "fib 4 = 3"
  IO.println "fib 5 = 5"
  IO.println "fib 10 = 55"
  IO.println "fib 20 = 6765"

  if fib 0 == 0 && fib 1 == 1 && fib 2 == 1 && fib 3 == 2 &&
     fib 4 == 3 && fib 5 == 5 && fib 10 == 55 && fib 20 == 6765 then
    IO.println "All correct!"
  else
    IO.println "FAILED"
