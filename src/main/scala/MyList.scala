package com.basuykdaria.lab1

import scala.annotation.tailrec
import MyList.*

enum MyList[+A]:
  case MyNil
  case MyCons(hd: A, tl: MyList[A])

  override def toString: String =
    @scala.annotation.tailrec
    def go(sb: StringBuilder, as: MyList[A]): String = {
      as match {
        case MyNil =>
          sb.result
        case MyCons(h, t) =>
          go(
            sb
              .append(h)
              .append(if t == MyNil then "]" else ", "),
            t
          )
      }
    }
    go(new StringBuilder("["), this)

object MyList:
  def apply[A](xs: A*): MyList[A] = of(xs*)
  def of[A](xs: A*): MyList[A] =
    xs.foldRight(MyNil: MyList[A]) { case (x, acc) => MyCons(x, acc) }


def foldRight[A, B](xs: MyList[A], z: B)(f: (A, B) => B): B =
  xs match
    case MyNil => z
    case MyCons(hd, tl) => f(hd, foldRight(tl, z)(f))


def mapWithIndex[A,B](xs: MyList[A], f: (Int, A) => B): MyList[B] =
  def go(xs: MyList[A], f: (Int, A) => B, i: Int): MyList[B] =
    xs match
      case MyNil => MyNil
      case MyCons(hd, tl) => MyCons(f(i, hd), go(tl, f, i + 1))
  go(xs, f, 0)


def duplicate[A](xs: MyList[A]): MyList[(A, A)] =
  foldRight(xs, MyNil: MyList[(A, A)])((a, b) => MyCons((a, a), b))


def intersperse[A](xs: MyList[A], a: A): MyList[A] =
  xs match
    case MyNil => MyNil
    case MyCons(hd, tl) =>
      MyCons(
        hd,
        foldRight(tl, MyNil: MyList[A])(
          (q, w) => MyCons(a, MyCons(q, w))
    )
  )

def fromString(s: String): MyList[Char] =
  s.foldRight(MyNil: MyList[Char])((ch, xs) => MyCons(ch, xs))


def toString(s: MyList[Char]): String =
  @tailrec
  def go(sb: StringBuilder, s: MyList[Char]): String =
    s match
      case MyNil => sb.result()
      case MyCons(hd, tl) => go(sb.append(hd), tl)
  go(new StringBuilder(""), s)

def clean[A](xs: MyList[A]): MyList[A] =
  def go(xs: MyList[A], prev: A): MyList[A] =
    xs match
      case MyNil => MyNil
      case MyCons(hd, tl) =>
        if (hd != prev)
          MyCons(hd, go(tl, hd))
        else
          go(tl, prev)
  xs match
    case MyNil => MyNil
    case MyCons(hd, tl) => MyCons(hd, go(tl, hd))


def getN[A](xs: MyList[A], n: Int): Option[A] =
  foldRight(xs, (1, None): (Int, Option[A]))((a, b) => {
    if (b._2.isEmpty)
      val i = b._1
      if (i == n)
        (n, Some(a))
      else
        (i + 1, None)
    else
      b
  })._2

@main def run() =
  println("Hello")
