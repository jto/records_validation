package records

import scala.language.postfixOps
import shapeless._, syntax.singleton._, record._
import shapeless.ops.record._
// import play.api.data.mapping._

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
  type Aux[T, Out0] = LabelledGeneric[T]{ type Repr = Out0 }

  def apply[T](implicit gen: LabelledGeneric[T]) = new Select[T, Nothing] {
    type Out = gen.Repr
    def apply(t: T): Out = gen.to(t)
  }

  implicit def fromGen[T, K, Out0 <: HList : ({type L[O] = Aux[T, O]})#L](implicit s: Selector[Out0, K]) =
    new Select[T, K] {
      type Out = s.Out
      val gen = implicitly[Aux[T, Out0]]
      def apply(t: T): Out = s(gen.to(t))
    }
}

object Rules {

  case class Book(author: String, title: String, id: Int, price: Double)
  case class User(name: String, book: Book)

  val bo = Book("Benjamin Pierce", "Types and Programming Languages", 262162091, 44.11)
  val us = User("jto", bo)

  val s0 = Select[User] >> 'book >> 'price
  s0(us) // 44.11
}