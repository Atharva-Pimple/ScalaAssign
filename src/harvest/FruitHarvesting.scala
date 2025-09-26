package harvest

import scala.io.Source

object FruitHarvesting extends App {
  val fruitPriceMap: Map[(String, String), Double] = {
    val lines = Source.fromFile("C:/Users/a.pimple/Downloads/harvest/prices.csv").getLines().drop(1)
    lines.map { line =>
      val Array(fruit, date, price) = line.split(",").map(_.trim)
      ((fruit, date), price.toDouble)
    }.toMap
  }


  val harvestMap: Map[(String,String,String),Double]={
    val lines= Source.fromFile("C:/Users/a.pimple/Downloads/harvest/harvest.csv").getLines().drop(1)
    lines.map { line =>
      val Array(gatherer,date,fruit,amount)=line.split(",").map(_.trim)
      val fruitPrice: Double = fruitPriceMap.getOrElse((fruit, date), 1.0)
      val totalAmount=amount.toDouble * fruitPrice
      ((gatherer,fruit,date),totalAmount)
    }.toMap
  }

//  harvestMap.foreach(println)

  def extractMonth(date: String): String= date.substring(0,7)
  val incomeFruitPerMonth: Map[(String,String), Double]={
    harvestMap.toSeq
      .map{
        case ( (_,fruit,date),amount) => ( (fruit,extractMonth(date)),amount )
      }
      .groupBy(_._1)
      .mapValues(_.map(_._2).sum)
  }

  // Group by month, then find fruit with max income
  val topFruitPerMonth: Map[String, (String, Double)] ={
    incomeFruitPerMonth
      .groupBy { case ((fruit, month), _) => month }
      .mapValues { fruitMap =>
        fruitMap.toSeq
          .map { case ((fruit, _), amount) => (fruit, amount) }
          .maxBy(_._2)
      }
  }

  println("Best Earning fruit by month:")
  topFruitPerMonth.foreach(data=>println(f"${data._1}: ${data._2._1} = ${data._2._2}%.2f"))

  val bestEarningFruit=harvestMap.groupBy(_._1._2).mapValues(_.map(_._2).sum).maxBy(_._2)
  println(f"Best Earning overall fruit is: ${bestEarningFruit._1} with totalAmount: ${bestEarningFruit._2}%.2f")

  val bestGatherer=harvestMap.groupBy(_._1._1).mapValues(_.values.sum).maxBy(_._2)
  println(f"Best Gatherer is: ${bestGatherer._1} with totalIncome: ${bestGatherer._2}%.2f")

}
