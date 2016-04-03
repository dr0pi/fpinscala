import fpinscala.datastructures.{List => FPList, Nil => FPNil, Cons, Tree, Branch, Leaf}

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

def dropWhile[A](list: List[A])(f: A => Boolean): List[A] = {
  list match {
    case x::xs if f(x) => dropWhile(xs)(f)
    case x::xs => x::xs
    case _ => Nil
  }
}

dropWhile(l1)(x => x == "s")

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

val x = FPList(1,2,3,4,5) match {
  case Cons(x, Cons(2, Cons(4, _))) => x
  case FPNil => 42
  case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
  case Cons(h, t) => h+1
  case _ => 101
}

val l2: FPList[Int] = FPList(1,2,3,4,5)

// 3.09
def length[A](as: FPList[A]): Int = {
  FPList.foldRight(as, 0)((as, z) => z+1)
}

length(l2)

// 3.10
@tailrec
def foldLeft[A,B](as: FPList[A], z: B)(f: (B,A) => B): B = {
  as match {
    case FPNil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }
}

// 3.11
def length2[A](as: FPList[A]): Int = {
  foldLeft(as, 0)((z, _) => z+1)
}

length2(l2)
def sum2(as: FPList[Int]): Int = {
  foldLeft(as, 0)((acc,a) => acc+a)
}
def prod2(as: FPList[Int]): Int = {
  foldLeft(as, 1)((acc, a) => acc*a)
}

sum2(l2)
prod2(l2)

val test: FPList[Int] = Cons(1,FPNil)

// 3.12
def reverse[A](as: FPList[A]): FPList[A] = {
foldLeft(as, FPList[A]())(
    (acc:FPList[A], a:A) => Cons(a, acc)
  )
}
reverse(l2)

// 3.13
def foldRightWithLeft[A,B](as: FPList[A], z: B)(f: (A,B) => B): B = {
  foldLeft(reverse(as), z)((x: B, y: A) => f(y, x))
}

// 3.14
def append[A](l1: FPList[A], l2: FPList[A]): FPList[A] = {
  foldLeft(reverse(l1), l2)((xs: FPList[A], y: A) => Cons(y, xs))
}
val l3 = FPList(11,12,14,15)
append(l2, l3)

// 3.15
def concat[A](lists: FPList[FPList[A]]): FPList[A] = {
  foldLeft(lists, FPNil: FPList[A])((acc, l) => append(acc,l))
}
val lists = FPList(l2,l3)
concat(lists)

// 3.16
def addPlusOneToList(l: FPList[Int]): FPList[Int] = {
  foldRightWithLeft(l, FPList[Int]())(
    (x, xs) => Cons((x+1), xs)
  )
}
addPlusOneToList(l2)

// 3.17
def listToStr(l: FPList[Int]): FPList[String] = {
  foldRightWithLeft(l, FPList[String]())(
    (x, xs) => Cons(x.toString, xs)
  )
}
listToStr(l2)

// 3.18
def map[A,B](as: FPList[A])(f: A => B): FPList[B] = {
  foldRightWithLeft(as, FPNil:FPList[B])(
    (x, xs) => Cons(f(x), xs)
  )
}

// 3.19
def filter[A](as: FPList[A])(f: A => Boolean): FPList[A] = {
  foldRightWithLeft(as, FPNil:FPList[A])(
    (x, xs) => if(f(x)){
      Cons(x,xs)
    } else {
      xs
  })
}
filter(l2)(_%2==0)

// 3.20
def flatMap[A,B](as: FPList[A])(f: A => FPList[B]): FPList[B] = {
  concat(map(as)(f))
}
flatMap(FPList(1,2,3))(i => FPList(i,i))

// 3.21
def filterWithFlatMap[A](as: FPList[A])(f: A => Boolean): FPList[A] = {
  flatMap(as)(
    a => if (f(a)){
      FPList(a)
    } else {
      FPNil
  })
}
filterWithFlatMap(l2)(_%2==0)

// 3.22
def addLists(l1: FPList[Int], l2: FPList[Int]): FPList[Int] =
  (l1,l2) match {
    case (FPNil, ys) => ys
    case (xs, FPNil) => xs
    case (Cons(x, xs), Cons(y, ys)) => Cons(x+y, addLists(xs,ys))
}

// 3.23
def zipWith[A,B,C](l1: FPList[A], l2: FPList[B])
                  (f: (A, B) => C): FPList[C] =
  (l1,l2) match {
    case (FPNil, _) => FPNil
    case (_, FPNil) => FPNil
    case (Cons(x, xs), Cons(y, ys)) => Cons(f(x,y), zipWith(xs,ys)(f))
  }
zipWith(l2,l2)(_+_)

val list = List(1,2,3)
list.scanRight(0)(_+_)
list.scanLeft(0)(_+_)

// 3.24
@tailrec
def hasStart[A](sup: FPList[A], start: FPList[A]): Boolean =
  (sup, start) match {
    case (_, FPNil) => true
    case (Cons(x, xs), Cons(y, ys)) if (x == y) => hasStart(xs, ys)
    case _ => false
  }
@tailrec
def hasSubsequence[A](sup: FPList[A], sub: FPList[A]): Boolean =
  (sup, sub) match {
    case (FPNil, Cons(y, ys)) => false
    case (_, FPNil) => true
    case _ if hasStart(`sup`,`sub`) => true
    case (Cons(x, xs), _) => hasSubsequence(xs, `sub`)
}
val l4 = FPList(1,2,1,2,3,4,5)
val sub = FPList(1,2,3,4,5)
hasSubsequence(l4, sub)

// 3.25
val t1: Tree[Int] = Branch(Branch(Leaf(1), Leaf(6)), Leaf(3))
def size[A](tree: Tree[A]): Int = tree match {
  case Branch(l,r) => 2 + size(l) + size(r)
  case Leaf(_) => 1
}
size(t1)

// 3.26
def maxTree(tree: Tree[Int]): Int = tree match {
  case Leaf(n) => n
  case Branch(l,r) => maxTree(l) max maxTree(r)
}
maxTree(t1)

// 3.27
def depth[A](tree: Tree[A]): Int = tree match {
  case Leaf(_) => 1
  case Branch(l, r) => (depth(l) max depth(r))+1
}
depth(t1)

// 3.28
def mapTree[A,B](tree: Tree[A])(f: A => B): Tree[B] =
  tree match {
    case Leaf(a) => Leaf(f(a))
    case Branch(l, r) => Branch(mapTree(l)(f), mapTree(r)(f))
}
mapTree(t1)(_+1)

// 3.29

