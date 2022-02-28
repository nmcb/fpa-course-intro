package fps
package cassandra

import cats.instances.set
import scala.{List => sList}
import cats.instances.list
import scala.runtime.IntRef


sealed trait List[+A]:
  def foldRight[B](z:B)(f: (A,B) => B): B =
    this match{
      case Nil => z
      case Cons(h,t) => f(h, t.foldRight(z)(f))
    }
  def foldLeft[B](z: B)(f: (B,A) => B): B =
    this match{
      case Nil => z
      case Cons(h,t) => t.foldLeft(f(z,h))(f)
    }

  def sum: Int =  this.asInstanceOf[List[Int]].foldLeft(0)(_+_)
  def project: Int =  this.asInstanceOf[List[Int]].foldLeft(1)(_*_)

  def length: Int =
    this match{
      case Nil => 0
      case Cons(_,_) => foldLeft(0)((acc,_)=> acc+1)
    }
  def append[A1 >: A](a: A1): List[A1] =
    this.foldRight(Cons(a,Nil))((t,acc) => Cons(t,acc))

  def append[A1 >: A](as:List[A1]): List[A1] =
    this.foldRight(as)((t,acc)=> Cons(t,acc)) 

  def reverse: List[A] = foldLeft(List.empty)((acc, elm) => (Cons(elm,acc)))
  
  def map[B](f:(A => B)): List[B] = 
    this match{
      case Nil => Nil
      case Cons(h,t) => Cons(f(h), t.map(f))
    }
  
  def filter[B](f:(A => Boolean)): List[A] =
    this match{
      case Nil => Nil
      case Cons(h,t) if f(h) => Cons(h, t.filter(f))
      case Cons(_,t) => t.filter(f)
    }

  def flatMap[B](f: A=> List[B]):List[B] =
    concat(map(f))
  
  def zipWith[B,C](bs:List[B])(f:(A,B)=> C):List[C] =
    bs match{
        case Nil => Nil
        case Cons(bh,bt) => this match{
          case Nil => Nil
          case Cons(h,t) => Cons(f(h,bh),t.zipWith(bt)(f))
        }
    }
  def hasSubsequence[B](sub: List[B]): Boolean =
    sub match{
      case Nil => true
      case Cons(sh,st) => this match{
      case Nil => false
      case Cons(h,t) if(h==sh) && t.hasStrictSubsequence(st) => true
      case Cons(h,t) => t.hasSubsequence(sub)
    }}
  
  def hasStrictSubsequence[B](sub: List[B]): Boolean =
    sub match{
      case Nil => true
      case Cons(sh,st) => this match{
        case Nil=> false
        case Cons(h,t) if(h==sh) && t.hasStrictSubsequence(st) => true
        case Cons(_,_) => false
      }
    }
    
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List:
  def apply[A](as: A*):List[A]=
    as.foldRight(List.empty)((t,acc)=> Cons(t,acc))
  def empty[A]: List[A] = Nil

def concat[A](a: List[List[A]]):List[A] =
  a.foldRight(List.empty)((elm, acc) => acc.append(elm))

def tail[A](ls: List[A]): List[A] = 
  ls match {
    case Nil => sys.error("Empty List")
    case Cons(_ , t) => t
  }
    
def set_head[A](h:A, ls: List[A]): List[A] =
  ls match {
    case Nil => sys.error("Empty List")
    case Cons(_,t) => Cons(h,t)
  }

def drop[A](h:Int, ls: List[A]): List[A] =
  if(h>0) then ls match {
                    case Nil => sys.error("Empty List")
                    
                    case Cons(_,t) => drop(h-1, t)
                  }
  else ls


def dropWhile[A](ls: List[A], f:A => Boolean): List[A] =
  ls match{
    case Nil => sys.error("Empty List")
    case Cons(h,t) => if(f(h)) then dropWhile(t,f)
                      else Cons(h,t)
  }

def init[A](ls:List[A]):List[A] =
  ls match{
    case Nil => sys.error("Empty List")
    case Cons(_, Nil) => Nil
    case Cons(h,t) => Cons(h,init(t))
  }
  

def isSorted[A](as:Array[A], ordered:(A,A)=> Boolean): Boolean =
  def loop(n: Int, n1: Int): Boolean =
    if (n >= as.length) true
    else if (ordered(as(n),as(n1))) loop(n+1,n1+1)
    else false
  loop(0,1)
        

def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B) : B =
        as match
            case Nil => z
            case Cons(x, xs) => f(x, foldRight(xs, z)(f))

def length [A] (as: List[A]):Int=
  foldRight(as,0)((_,y:Int) => y+1)


sealed trait Tree[+A]:
  def size: Int =
    this match{
      case Leaf(x) => 1
      case Branch(l,r) => 1+ r.size + l.size 
    }
  def maxi: Int =
    this.asInstanceOf[Tree[Int]] match{
      case Leaf(x) => x
      case Branch(l,r) => l.maxi.max(r.maxi)
    }
  def depth: Int =
    this.recdept(0)

  def recdept(d:Int):Int =
    this match{
      case Leaf(x) => d + 1
      case Branch(l,r) => l.recdept(d+1).max(r.recdept(d+1))
    }

  def map[B](f: A => B): Tree[B]=
    this match{
      case Leaf(x) => Leaf(f(x))
      case Branch(l,r) => Branch(l.map(f),r.map(f))
    }
  
  def fold[B](b:A => B)(f:(B,B) => B): B =
    this match{
      case Leaf(x) => b(x)
      case Branch(l,r) => f(l.fold(b)(f),r.fold(b)(f))
    }
 
case class Leaf[A](value:A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
val tree = Branch(Branch(Leaf(1),Branch(Leaf(2),Branch(Leaf(3),Leaf(4)))),Leaf(5))



object Main extends App:
  val list1 = List(4.0,.5,.6)
  val list2 = List(1,2,1,2,3)
  val list3 = List(7,8,9)
  val comlist = List(list1,list2,list3)
  println(list1.flatMap(i =>if(i>2) List(i) else Nil))
  println(list2.zipWith(list3)(_+_))
  println(list2.hasSubsequence(List(2,3)))
  println(tree.fold(x=>x)((a,b)=> a.max(b)))

  