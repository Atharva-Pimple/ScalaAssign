package harvest.oop_sln


object FruitHarvestingMain extends App {
  val dataLoader=new DataLoader(
    "C:/Users/a.pimple/Downloads/harvest/harvest.csv",
    "C:/Users/a.pimple/Downloads/harvest/prices.csv"
  )

  val harvestData=dataLoader.loadHarvestData()
  val pricesData=dataLoader.loadPriceData()

  val analyzer=new FarmAnalyzer(harvestData,pricesData)

  println("best gatherer in terms of the amounts of fruits gathered every month")
  analyzer.totalFruitByGathererPerMonth().foreach{case (month,gatherer)=>
    println(s"$month : $gatherer")
  }

  println("\nBest Gatherer by Fruit:")
  analyzer.gathererFruitSpeciality().foreach { case (gatherer, fruit) =>
    println(s"$gatherer specializes in $fruit")
  }

  println("\nGatherer Income (Overall):")
  analyzer.gathererIncome().toList.sortBy(-_._2).foreach {
    case (gatherer, income) => println(f"$gatherer: $$${income}%.2f")
  }

  println("\nFruit Earnings by Month:")
  analyzer.fruitEarningByMonth().foreach { case (month, fruitMap) =>
    println(s"Month: $month")
    fruitMap.toList.sortBy(-_._2).foreach {
      case (fruit, earning) => println(f"  $fruit: $$${earning}%.2f")
    }
  }
  println("\nBest/Worst Fruit by Month:")
  analyzer.bestWorstFruitByMonth().foreach {
    case (month, (best, worst)) => println(s"$month => Best: $best, Worst: $worst")
  }

  println("\nTop Earning Gatherer by Month:")
  analyzer.topEarnerByMonth().foreach {
    case (month, gatherer) => println(s"$month => $gatherer")
  }

  val bestGatherer=analyzer.gathererIncome().maxBy(_._2)
  println(f"\nBest Gatherer is: ${bestGatherer._1} with totalIncome: ${bestGatherer._2}%.2f")

//  println()
//  analyzer.gathererIncomeByMonth().foreach(println)


}
