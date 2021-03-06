package fpinscala.errorhandling

//hide std library `Option2` and `Either`, since we are writing our own in this chapter
import scala.annotation.tailrec
import scala.{Either => _, Option => _, _}

sealed trait Option2[+A] {
  def map[B](f: A => B): Option2[B] = this match {
    case None => None
    case Some(a) => Some(f(a))
  }

  def getOrElse[B>:A](default: => B): B = this match {
    case None => default
    case Some(a) => a
  }

  def flatMap[B](f: A => Option2[B]): Option2[B] =
    map(f) getOrElse None

  /*
  Of course, we can also implement `flatMap` with explicit pattern matching.
  */
  def flatMap_1[B](f: A => Option2[B]): Option2[B] = this match {
    case None => None
    case Some(a) => f(a)
  }

  def orElse[B>:A](ob: => Option2[B]): Option2[B] =
    this map (Some(_)) getOrElse ob

  /*
  Again, we can implement this with explicit pattern matching.
  */
  def orElse_1[B>:A](ob: => Option2[B]): Option2[B] = this match {
    case None => ob
    case _ => this
  }

  def filter(f: A => Boolean): Option2[A] = this match {
    case Some(a) if f(a) => this
    case _ => None
  }
  /*
  This can also be defined in terms of `flatMap`.
  */
  def filter_1(f: A => Boolean): Option2[A] =
    flatMap(a => if (f(a)) Some(a) else None)
}
case class Some[+A](get: A) extends Option2[A]
case object None extends Option2[Nothing]

object Option2 {
  def failingFn(i: Int): Int = {
    // `val y: Int = ...` declares `y` as having type `Int`, and sets it equal to the right hand side of the `=`.
    val y: Int = throw new Exception("fail!")
    try {
      val x = 42 + 5
      x + y
    }
    // A `catch` block is just a pattern matching block like the ones we've seen. `case e: Exception` is a pattern
    // that matches any `Exception`, and it binds this value to the identifier `e`. The match returns the value 43.
    catch { case e: Exception => 43 }
  }

  def failingFn2(i: Int): Int = {
    try {
      val x = 42 + 5
      // A thrown Exception can be given any type; here we're annotating it with the type `Int`
      x + ((throw new Exception("fail!")): Int)
    }
    catch { case e: Exception => 43 }
  }

  def mean(xs: Seq[Double]): Option2[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option2[Double] =
    mean(xs) flatMap (m => mean(xs.map(x => math.pow(x - m, 2))))

  // a bit later in the chapter we'll learn nicer syntax for
  // writing functions like this
  def map2[A,B,C](a: Option2[A], b: Option2[B])(f: (A, B) => C): Option2[C] =
    a flatMap (aa => (b map (bb => f(aa, bb))))

  /*
  Here's an explicit recursive version:
  */
  def sequence[A](a: List[Option2[A]]): Option2[List[A]] =
    a match {
      case Nil => Some(Nil)
      case h :: t => h flatMap (hh => sequence(t) map (hh :: _))
    }
  /*
  It can also be implemented using `foldRight` and `map2`. The type annotation on `foldRight` is needed here; otherwise
  Scala wrongly infers the result type of the fold as `Some[Nil.type]` and reports a type error (try it!). This is an
  unfortunate consequence of Scala using subtyping to encode algebraic data types.
  */
  def sequence_1[A](a: List[Option2[A]]): Option2[List[A]] =
    a.foldRight[Option2[List[A]]](Some(Nil))((x,y) => map2(x,y)(_ :: _))

  def traverse[A, B](a: List[A])(f: A => Option2[B]): Option2[List[B]] =
    a match {
      case Nil => Some(Nil)
      case h::t => map2(f(h), traverse(t)(f))(_ :: _)
    }

  def traverse_1[A, B](a: List[A])(f: A => Option2[B]): Option2[List[B]] =
    a.foldRight[Option2[List[B]]](Some(Nil))((h,t) => map2(f(h),t)(_ :: _))

  def sequenceViaTraverse[A](a: List[Option2[A]]): Option2[List[A]] =
    traverse(a)(x => x)
}
