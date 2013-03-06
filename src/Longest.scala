import scala.annotation.tailrec

object Longest {
  case class Candidate(lower: Int, upper: Int, contents: String) {
    val (smaller, bigger) = (lower - 1, upper + 1)
    def next(string: String): Option[Candidate] =
      if (smaller >= 0 && bigger <= string.length - 1 && string(smaller) == string(bigger)) {
        Some(copy(lower = lower - 1, upper = upper + 1, contents = string.substring(smaller, bigger + 1)))
      }
      else {
        None
      }
  }

  def main(args: Array[String]) = {
    val string = args(0)
    val sideLength = (string.length + 1 / 2) + 1
    val candidates = (string.zipWithIndex map {
      case (char, index) => Candidate(index, index, char.toString)
    }).toList
    println(search(string, candidates, candidates(0).contents))
  }

  @tailrec
  def search(string: String, candidates: List[Candidate], best: String): String = {
    candidates match {
      case Nil => best
      case head  :: tail => {
        val next = head.next(string)
        next match {
          case Some(candidate) => search(string, tail :+ candidate, candidate.contents)
          case None => search(string, tail, best)
        }
      }
    }
  }
}
