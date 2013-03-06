import scala.annotation.tailrec

object Longest {
  case class Candidate(lower: Int, upper: Int, chars: List[Char]) {
    val (smaller, bigger) = (lower - 1, upper + 1)

    def next(string: String): Option[Candidate] =
      if (smaller >= 0 && bigger <= string.length - 1 && string(smaller) == string(bigger)) {
        Some(copy(lower = smaller, upper = bigger, chars = string(smaller) :: chars))
      }
      else {
        None
      }

    def stringify: String = "%s%s".format(chars.mkString, chars.init.reverse.mkString)
  }

  def main(args: Array[String]) = {
    val string = args(0)
    val candidates = (string.zipWithIndex map {
      case (char, index) => Candidate(index, index, char :: Nil)
    }).toList
    println(search(string, candidates, candidates.head))
    println(parSearch(string, candidates))
  }

  @tailrec
  def search(string: String, candidates: List[Candidate], best: Candidate): String = {
    candidates match {
      case Nil => best.stringify
      case head :: tail => head.next(string) match {
        case Some(candidate) => search(string, tail :+ candidate, candidate)
        case None => search(string, tail, best)
      }
    }
  }

  def parSearch(string: String, candidates: List[Candidate]): String = {
    @tailrec
    def consecutiveSearch(candidate: Candidate): String = candidate.next(string) match {
      case Some(next) => consecutiveSearch(next)
      case None => candidate.stringify
    }
    candidates.par.map(consecutiveSearch(_)).maxBy(_.length)
  }

}
