package harvest.oop_sln

import java.time.LocalDate

class FarmAnalyzer(harvests: List[HarvestEntry], prices: Map[(String, LocalDate), Double]) {
  private def getMonthKey(date: LocalDate): String = date.getYear + "-" + f"${date.getMonthValue}%02d"


  def totalFruitByGathererPerMonth(): Map[String, String] = {
    harvests.groupBy(h => (h.gatherer, getMonthKey(h.date)))
      .mapValues(_.map(_.amount).sum)
      .groupBy(_._1._2)
      .mapValues(_.maxBy(_._2)._1._1)
  }


  def gathererFruitSpeciality(): Map[String,String]={
    harvests.groupBy(_.gatherer)
      .mapValues(entries=> entries.groupBy(_.fruit).mapValues(_.map(_.amount).sum))
      .mapValues(fruitTotals=> fruitTotals.maxBy(_._2)._1)
  }

  def fruitEarningByMonth(): Map[String,Map[String, Double]]={
    harvests.flatMap{h=>
      prices.get((h.fruit,h.date)).map(price=> (getMonthKey(h.date), h.fruit, h.amount * price))
    }
      .groupBy{case (month,fruit,_) => (month,fruit)}
      .mapValues(_.map(_._3).sum)
      .groupBy(_._1._1)
      .mapValues(_.map {case ((_,fruit), amount) => (fruit,amount)}.toMap)
  }

  def bestWorstFruitByMonth(): Map[String, (String, String)]={
    fruitEarningByMonth().mapValues{fruitMap=>
      val sorted=fruitMap.toList.sortBy(_._2)
      val worst=sorted.head._1
      val best=sorted.last._1
      (best,worst)
    }
  }

  def gathererIncome(): Map[String,Double]={
    harvests.groupBy(_.gatherer).mapValues(entries=>
      entries.flatMap{ h=>
        prices.get((h.fruit, h.date)).map(price=> h.amount * price)
      }.sum
    )
  }

  def gathererIncomeByMonth(): Map[String, Map[String,Double]]= {
    harvests.groupBy(h=>(h.gatherer, getMonthKey(h.date)))
      .mapValues(_.flatMap{ h=>
        prices.get((h.fruit,h.date)).map(price=> h.amount * price)
      }.sum)
      .groupBy(_._1._1)
      .mapValues(_.map{ case ((_,month),income) => month->income}.toMap)
  }


  def topEarnerByMonth():Map[String,String] ={
    harvests.groupBy(h=>getMonthKey(h.date))
      .mapValues(_.groupBy(_.gatherer)
        .mapValues(_.flatMap{ h=>
          prices.get((h.fruit,h.date)).map(price=> h.amount * price)
        }.sum)
      .maxBy(_._2)._1)
  }




}
