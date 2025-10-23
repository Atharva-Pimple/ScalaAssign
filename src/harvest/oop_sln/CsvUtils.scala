package harvest.oop_sln

import java.time.LocalDate
import java.time.format.DateTimeFormatter

object CsvUtils {
  val formatter: DateTimeFormatter = DateTimeFormatter.ofPattern("yyyy-MM-dd")

  def parseDate(dateStr: String): LocalDate = LocalDate.parse(dateStr, formatter)
}
