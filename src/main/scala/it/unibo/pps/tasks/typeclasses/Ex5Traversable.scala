package it.unibo.pps.tasks.typeclasses

import it.unibo.pps.u03.Sequences.Sequence
import Sequence.*
import it.unibo.pps.u03.extensionmethods.Optionals.Optional

/*  Exercise 5: 
 *  - Generalise by ad-hoc polymorphism logAll, such that:
 *  -- it can be called on Sequences but also on Optional, or others... 
 *  -- it does not necessarily call log, but any function with analogous type
 *  - Hint: introduce a type class Traversable[T[_]]], capturing the ability of calling a
 *    "consumer function" on all elements (with type A) of a datastructure T[A] 
 *    Note Traversable is a 2-kinded trait (similar to Filterable, or Monad)
 *  - Write givens for Traversable[Optional] and Traversable[Sequence]
 *  - Show you can use the generalisation of logAll to:
 *  -- log all elements of an Optional, or of a Traversable
 *  -- println(_) all elements of an Optional, or of a Traversable
 */

object Ex5Traversable:

  def log[A](a: A): Unit = println("The next element is: "+a)

  def logAll[A](seq: Sequence[A]): Unit = seq match
    case Cons(h, t) => log(h); logAll(t)
    case _ => ()


  trait Traversable[T[_]]:
    def generalisation[A](t: T[A])(f: A => Unit): Unit

  given Traversable[Sequence] with
    def generalisation[A](t: Sequence[A])(f: A => Unit): Unit = t match {
      case Cons(h, t) => f(h); generalisation(t)(f)
      case _ => ()
    }

  given Traversable[Optional] with
    def generalisation[A](t: Optional[A])(f: A => Unit): Unit = t match {
      case Optional.Just(a) => f(a)
      case _ => ()
    }

  def logAll[T[_]: Traversable, A](element: T[A]): Unit =
    val traversable = summon[Traversable[T]]
    traversable.generalisation(element)(log)




  
