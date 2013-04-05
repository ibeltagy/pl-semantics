package dhg.depparse

import scala.collection.generic.CanBuildFrom
import scala.collection.GenTraversableLike
import scala.collection.GenTraversable

trait ErrorBox[+T] {
  def map[R](f: T => Option[R], newError: String): ErrorBox[R] = this match {
    case ErrorBoxValue(value) => f(value).map(ErrorBoxValue(_)).getOrElse(ErrorBoxError(newError))
    case ErrorBoxError(error) => ErrorBoxError(error)
  }
  def map[R](f: T => R): ErrorBox[R] = this match {
    case ErrorBoxValue(value) => ErrorBoxValue(f(value))
    case ErrorBoxError(error) => ErrorBoxError(error)
  }
  def flatMap[R](f: T => ErrorBox[R]): ErrorBox[R] = this match {
    case ErrorBoxValue(value) => f(value)
    case ErrorBoxError(error) => ErrorBoxError(error)
  }
  def forError(f: String => Unit)
  def get: Option[T]
  def isDefined: Boolean
  def isError: Boolean
}

case class ErrorBoxValue[+T](value: T) extends ErrorBox[T] {
  override def forError(f: String => Unit) {}
  override def get = Some(value)
  override def isDefined = true
  override def isError = false
}

case class ErrorBoxError(error: String) extends ErrorBox[Nothing] {
  override def forError(f: String => Unit) { f(error) }
  override def get = None
  override def isDefined = false
  override def isError = true
}

case class BooleanAsErrorBox(b: Boolean) {
    def errorBox(error: String): ErrorBox[Unit] = if (b) ErrorBoxValue() else ErrorBoxError(error)
  }


case  class OptionAsErrorBox[T](o: Option[T]) {
    def errorBox(error: String): ErrorBox[T] = o.map(ErrorBoxValue(_)).getOrElse(ErrorBoxError(error))
  }

case class Enriched_flattenOverErrorBox_GenTraversableLike[A, Repr <: GenTraversable[ErrorBox[A]]](self: GenTraversableLike[ErrorBox[A], Repr]) {
    def flattenOverErrorBox[That](implicit bf: CanBuildFrom[Repr, A, That]): ErrorBox[That] = {
      val b = bf(self.asInstanceOf[Repr])
      b.sizeHint(self.size)
      self.foreach {
        case ErrorBoxValue(v) => b += v
        case ErrorBoxError(e) => return ErrorBoxError(e)
      }
      return ErrorBoxValue(b.result)
    }
  }


object ErrorBox {
/*  class BooleanAsErrorBox(b: Boolean) {
    def errorBox(error: String): ErrorBox[Unit] = if (b) ErrorBoxValue() else ErrorBoxError(error)
  }
*/

  

}
