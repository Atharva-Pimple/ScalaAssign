package CoreDigit

import scala.annotation.tailrec
import scala.io.Source

object CoreDigitProblem extends App {
  val source=Source.fromFile("C:/Users/a.pimple/Downloads/CoreData.txt","UTF-8")
  val lines=source.getLines().toList
  val numbers=lines.head.split("\\s").toList
  source.close()
  val base=numbers.head
  val times=numbers.tail.head.toInt

  @tailrec
  def coreDigit(number: String): String={
    @tailrec
    def calculateSum(n: String, sum: Int): Int={
      if(n.isEmpty || n.length<=0) sum
      else{
        val num= n.charAt(0).asDigit
        calculateSum(n.substring(1),sum+num)
      }
    }

    if(number.length==1) number
    else coreDigit(calculateSum(number,0).toString)
  }

  val digitSum = base.map(_.asDigit).sum
  val total = digitSum * times
//  val result=coreDigit(total.toString)
  val result = if (total == 0) 0 else 1 + (total - 1) % 9
  println(result)

}
