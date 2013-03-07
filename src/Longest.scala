import scala.annotation.tailrec

object Longest {
  case class Candidate(lower: Int, upper: Int, chars: List[Char]) {
    val (smaller, bigger) = (lower - 1, upper + 1)

    def next(string: String): Option[Candidate] =
      if (smaller >= 0 && bigger <= string.length && string(smaller) == string(bigger - 1)) {
        Some(copy(lower = smaller, upper = bigger, chars = string(smaller) :: chars))
      }
      else {
        None
      }

    def stringify: String = chars match {
      case Nil => ""
      case _ => "%s%s".format(chars.mkString, chars.init.reverse.mkString)
    }
  }

  def main(args: Array[String]) = {
    val string = args(0)
    val (ones, zeros) = (string.zipWithIndex map {
      case (char, index) => (Candidate(index, index + 1, char :: Nil), Candidate(index, index, Nil))
    }).toList.unzip
    val candidates = zeros ++ ones
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
