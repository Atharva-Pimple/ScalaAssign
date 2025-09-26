package thief_data

import scala.io.Source

object ThiefDataProblem extends App {
  val source=Source.fromFile("C:/Users/a.pimple/Downloads/thief_data.txt","UTF-8")
  val lines=source.getLines().toList
  source.close()

  def remoteUsed(path: String, key: Char='1', idx: Int=0, count: Int=0): Int = {
    if(idx==path.length) count
    else if(path.charAt(idx) == key) remoteUsed(path,key,idx+1,count)
    else{
      val newKey= if(key=='0') '1' else '0'
      remoteUsed(path,newKey,idx+1,count+1)
    }
  }

  val ans=lines.filter(_.nonEmpty).map(line=>remoteUsed(line)).toList
  ans.foreach(println)
}
