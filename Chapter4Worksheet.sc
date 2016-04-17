import scala.{Option => _, _}

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing] {
  def get = throw new Exception("nothing to see here")
}

trait Option[+A] {
  def get: A

  def map[B](f: A => B): Option[B] = this match {
    case Some(_) => Some(f(this.get))
    case None => None
  }
  def flatMap[B](f: A => Option[B]): Option[B] = this match {
    case Some(_) => f(this.get)
    case None => None
  }
  def getOrElse[B >: A](default: => B): B = this match {
    case Some(_) => this.get
    case None => default
  }
  def orElse[B >: A](ob: => Option[B]): Option[B] = this match {
    case Some(_) => this
    case None => ob
  }
  def filter(f: A => Boolean): Option[A] = this match {
    case Some(_) if f(this.get) => this
    case _ => None
  }
}

val s = Some("B")
s.map(_ => "A")
s.filter(_ == "B")

def mean(xs: Seq[Double]): Option[Double] = xs match {
  case Nil => None
  case _ => Some(xs.sum / xs.length)
}

def variance(xs: Seq[Double]): Option[Double] = {
  mean(xs) flatMap (m => mean(xs.map(x => math.pow(x - m, 2))))
}

val v = Seq(1.0,2.0,3.0,4.0)
variance(v)

def lift[A,B](f: A => B): Option[A] => Option[B] = _ map f

def map2[A,B,C](optA: Option[A], optB: Option[B])(f: (A,B) => C): Option[C] = {
  optA.flatMap(realA => optB.map(realB => f(realA,realB)))
}

def Try[A](a: => A): Option[A] =
  try Some(a)
  catch { case e: Exception => None }

def sequence[A](a: List[Option[A]]): Option[List[A]] = {
  try Some(a.map(x => x.get))
  catch { case e: Exception => None }
}

val ss = List(Some(1), Some(2), Some(3), None)
sequence(ss)

def traverse[A,B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
  try Some(a.map(x => f(x).get))
  catch { case e: Exception => None }
}


