package harvest.oop_sln

import java.time.LocalDate
import scala.io.Source

class DataLoader(harvestFile: String, pricesFile: String){
  def loadHarvestData(): List[HarvestEntry] ={
    val lines= Source.fromFile(harvestFile).getLines().drop(1)
    lines.map { lines=>
      val Array(gatherer, dateStr, fruit, amountStr)=lines.split(",").map(_.trim)
      HarvestEntry(gatherer, CsvUtils.parseDate(dateStr), fruit, amountStr.toDouble)
    }.toList
  }

  def loadPriceData(): Map[(String, LocalDate), Double]={
    val lines=Source.fromFile(pricesFile).getLines().drop(1)
    lines.map { line =>
      val Array(fruit, dateStr, priceStr) = line.split(",").map(_.trim)
      ((fruit, CsvUtils.parseDate(dateStr)) -> priceStr.toDouble)
    }.toMap
  }


}
