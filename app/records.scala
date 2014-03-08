package records

import scala.language.postfixOps
import shapeless._, syntax.singleton._, record._
import shapeless.ops.record._
import play.api.data.mapping._

@annotation.implicitNotFound(msg = "No field ${K} in record ${L}")
trait Select[L, K] {
  outer =>
  type Out
  def apply(l: L): Out
  def >>(k: Witness)(implicit select: Select[outer.Out, k.T]) = new Select[L, k.T] {
    type Out = select.Out
    def apply(l: L): Out = select(outer(l))
  }
}

object Select {
  def apply[T](implicit gen: LabelledGeneric[T]) = new Select[T, Nothing] {
    type Out = gen.Repr
    def apply(t: T): Out = gen.to(t)
  }

  implicit def fromS[L <: HList, K](implicit s: Selector[L, K]) = new Select[L, K] {
    type Out = s.Out
    def apply(l: L): Out = s(l)
  }

  implicit def fromGen[T, K](implicit gen: LabelledGeneric[T], s: Select[gen.Repr, K]) = new Select[T, K] {
    type Out = s.Out
    def apply(t: T): Out = s(gen.to(t))
  }
}

object Rules {

  case class Book(author: String, title: String, id: Int, price: Double)
  case class User(name: String, book: Book)

  val bo = Book("Benjamin Pierce", "Types and Programming Languages", 262162091, 44.11)
  val us = User("jto", bo)

  val s0 = Select[User] >> 'book >> 'price
}