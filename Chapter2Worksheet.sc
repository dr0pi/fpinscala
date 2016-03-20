import scala.annotation.tailrec

def fib(n: Int): Long = {
  @tailrec
  def fibRec(n: Int, prev: Long, cur: Long): Long = {
    if (n < 1) {
      prev
    } else {
      fibRec(n - 1, cur, prev + cur)
    }
  }
  fibRec(n, 0, 1)
}

(0 to 5).map(fib(_))