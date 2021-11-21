
import scala.concurrent.duration.DurationInt
import scala.io.StdIn.readLine
import scala.util.matching.Regex

object Demo extends App {

  val csvParser: Regex = "(?<text>[\\S\\s]+?),(?<language>\\w+)".r
  val resource = "dataset.csv"
  Language.loadFromResource(csvParser, resource, 0.67)
    .printValidationSummary()
    .validationBarChart(timeout = 10.minutes)


  while(true) Language
    .classifyLanguage(readLine("Ready: "))
    .printScoreOfWinner()
    .plotBarChart(timeout = 10.minutes)
}

