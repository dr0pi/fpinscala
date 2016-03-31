import scala.annotation.tailrec

def myTail[A](list: List[A]): List[A] = {
  list match {
    case x::xs => xs
    case _ => Nil
  }
}

val l1 = List("s","u","p","e","r")
myTail(l1)

def setHead[A](list: List[A], newHead:A): List[A] = {
  newHead::myTail(list)
}

setHead(l1, "P")

def drop[A](list: List[A], n:Int): List[A] = {
  (list, n) match {
    case (xs, 0) => xs
    case (x::xs, _) => drop(xs, n-1)
    case _ => Nil
  }
}

drop(l1, 3)

def dropWhile[A](list: List[A], f: A => Boolean): List[A] = {
  list match {
    case x::xs if f(x) => dropWhile(xs, f)
    case x::xs => x::xs
    case _ => Nil
  }
}

dropWhile(l1, (x: String) => x == "s")

def init[A](list: List[A]): List[A] = {
  @tailrec
  def go[A](acc: List[A], list: List[A]): List[A] = {
    (acc, list) match {
      case(acc, x::y::Nil) => (x::acc).reverse
      case(acc, x::xs) => go(x::acc,xs)
      case _ => Nil
    }
  }
  go(Nil, list)
}
init(l1)
