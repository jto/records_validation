package records

  import scala.language.postfixOps
import shapeless._, syntax.singleton._, record._
import shapeless.ops.record._
import play.api.data.mapping._

object Rules {

  case class Book(author: String, title: String, id: Int, price: Double)
  case class User(name: String, book: Book)

  val bookGen = LabelledGeneric[Book]
  val userGen = LabelledGeneric[User]

  val tapl = Book("Benjamin Pierce", "Types and Programming Languages", 262162091, 44.11)
  val us = User("jto", tapl)

  val uRec = userGen.to(us)
  // Read price field
  val b = uRec('book)
  val bRec = bookGen.to(b)

  trait Select[L <: HList, K] extends Selector[L, K] {
    outer =>
    type Out <: HList
    def >>(k: Witness)(implicit selector: Selector[outer.Out, k.T]) = new Selector[L, k.T] {
      type Out = selector.Out
      def apply(l: L): Out = selector(outer(l))
    }
  }

  object Select {
    // def toN[L <: HList, K, Out <: HList](aux: Selector.Aux[L, K, Out]) = new Select[L, K] {
    //   type Out = aux.Out
    //   def apply(l: L) = aux.apply(l)
    // }
  }

  val k = Witness('book)
  val s = implicitly[Selector.Aux[userGen.Repr, k.T, Book]]
  val book = s(uRec)
  // implicitly[LabelledGeneric[s.Out =:= Book]]
  implicitly[LabelledGeneric[s.Out]]
  // val s2 = SelectorN.toN(s) >> 'price
  // s2(uRec)
}