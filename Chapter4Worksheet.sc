import scala.{Option => _, _}

trait Option[+A] {
  def get: A

  def map[B](f: A => B): Option[B] = this match {
    case Some(_) => Some(f(this.get))
    case None => None
  }
  def flatMap[B](f: A => Option[B]): Option[B] = ???
  def getOrElse[B >: A](default: => B): B = ???
  def orElse[B >: A](ob: => Option[B]): Option[B] = ???
  def filter(f: A => Boolean): Option[A] = ???
}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing] {
  def get = throw new Exception("nothing to see here")
}

val s = Some("B")
s.map(_ => "A")