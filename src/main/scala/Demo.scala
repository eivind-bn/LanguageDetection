
import scala.concurrent.duration.DurationInt
import scala.io.Source
import scala.io.StdIn.readLine
import scala.util.matching.Regex

object Demo extends App {

  val csvParser: Regex = "(?<text>[\\S\\s]+?),(?<language>\\w+)".r
  val path = "dataset.csv"
  Language.loadFromResource(csvParser, path)

  while(true) Language
    .classifyLanguage(readLine("Ready: "))
    .printScoreOfAll()
    .plotBarChart(timeout = 60.seconds)
}

