package srcambilng

import Examples._

object Scrambling extends App {
  def scrambleWord(word: String): String = {
    if (word.length <= 3) {
      word
    } else {
      val middle = word.substring(1, word.length - 1)

      val middleScrambled = (0 until middle.length).foldLeft(middle) {
        case (acc, cur) =>
          val pos = (math.random * middle.length).toInt

          acc.updated(cur, acc(pos)).updated(pos, acc(cur))
      }

      word.patch(1, middleScrambled, middleScrambled.length)
    }
  }
  
  def scrambleWord(word: List[Char]): List[Char] = scrambleWord(word.mkString).toList
  
  def scramble(text: String): String = {
    def scramble(text: List[Char], word: List[Char], processed: List[Char]): List[Char] = {
      text match {
        case Nil => processed ::: scrambleWord(word)
        case c :: tail => {
          if (c.isLetter) {
            scramble(tail, word :+ c, processed)
          } else {
            scramble(tail, Nil, processed ::: (scrambleWord(word) :+ c))
          }
        }
      }
    }
    
    scramble(text.toList, Nil, Nil).mkString
  }
  
  println(scramble(paragraph))
}