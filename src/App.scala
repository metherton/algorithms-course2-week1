import scala.annotation.tailrec
import scala.collection.mutable.Stack

/**
  * Created by martin on 08/05/17.
  */
object App {

  sealed abstract class Bracket {
    val position: Int
  }
  case class LeftSquareBracket(position: Int) extends Bracket
  case class LeftRoundBracket(position: Int) extends Bracket
  case class LeftCurlyBracket(position: Int) extends Bracket
//  case class RightSquareBracket(position: Int) extends Bracket
//  case class RightRoundBracket(position: Int) extends Bracket
//  case class RightCurlyBracket(position: Int) extends Bracket

  def main(args: Array[String]): Unit = {
    val input = (for {n <- 1 to 1; line = Console.readLine()} yield line)

    var badPosition = 0

    def isBalanced(s: String): Boolean = {
      @tailrec
      def loop(s: List[Char], balancedStack: Stack[Bracket], position: Int): Boolean = s match {
        case Nil => {
          if (balancedStack.isEmpty) true
          else {
            val p = balancedStack.pop
            badPosition = p.position
            false
          }
        }
        case '[' :: (tl @ tail) => loop(tl, balancedStack.push(LeftSquareBracket(position)), position + 1)
        case '{' :: (tl @ tail) => loop(tl, balancedStack.push(LeftCurlyBracket(position)), position + 1)
        case '(' :: (tl @ tail) => loop(tl, balancedStack.push(LeftRoundBracket(position)), position + 1)
        case (h @ head) :: (tl @ tail) =>
          if ( h == ']' || h == ')' || h == '}') {
            if (balancedStack.isEmpty) {
              badPosition = position
              false
            }
            else {
              val t = balancedStack.pop
              t match {
                case LeftSquareBracket(_) if (h == ']') => loop(tl, balancedStack, position + 1)
                case LeftRoundBracket(_) if (h == ')') => loop(tl, balancedStack, position + 1)
                case LeftCurlyBracket(_) if (h == '}') => loop(tl, balancedStack, position + 1)
                case _ => {
                  badPosition = position
                  false
                }
              }
            }
          } else {
            loop(tail, balancedStack, position + 1)
          }
        case _ => {
          println("true")
          true
        }
      }
      loop(s.toList, Stack(), 1)
    }

    val success = isBalanced(input(0))

    if (success == true) println("Success") else println(badPosition)

  }


}
