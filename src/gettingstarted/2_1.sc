def fib(n: Long): Long = {
  @annotation.tailrec
  def _fib(step: Long, prev: Long, current: Long): Long = {
    if(step <= 2) current
    else _fib(step - 1, current, prev + current)
  }
  if(n == 1) 0
  else _fib(n, 0, 1)
}

fib(1)
fib(2)
fib(3)
fib(5)
fib(7)
fib(10)
fib(80)


