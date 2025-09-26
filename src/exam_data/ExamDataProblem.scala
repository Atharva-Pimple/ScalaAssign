package exam_data

import scala.io.Source

object ExamDataProblem extends App {
  val source=Source.fromFile("C:/Users/a.pimple/Downloads/exam_data.txt","UTF-8")
  val lines=source.getLines().toList

  source.close()
  println(lines.head)

  def canPrepare(line: String): String={
    val data=line.split(",").map(_.trim.toInt)
    if(data.length != 3) "Invalid"
    else{
      val k=data(0)
      val l=data(1)
      val m=data(2)

      if(k*l <= m) "Yes"
      else "No"
    }
  }

  val ans=lines.filter(line=> line.nonEmpty).map(line=> canPrepare(line)).toList
  ans.foreach(println)


}