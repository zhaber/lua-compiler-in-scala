package ca.mcmaster.common

object Exceptionable {

  sealed abstract class Exceptionable[+A, +E] {

    def isSuccess = this match {
      case Success(_) => true
      case Failure(_) => false
    }

    def isFailure = !isSuccess

    def success = this match {
      case Success(a) => Some(a)
      case Failure(_) => None
    }

    def failure = this match {
      case Success(_) => None
      case Failure(e) => Some(e)
    }

  }

  implicit def resultToSuccess[A](result: A) = Success(result)
  
  final case class Success[A, E](a: A) extends Exceptionable[A, E]
  final case class Failure[A, E](e: E) extends Exceptionable[A, E]

}


