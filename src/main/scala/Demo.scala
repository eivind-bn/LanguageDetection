
import scala.io.StdIn.readLine
import scala.util.matching.Regex

object Demo extends App {

  val csvParser: Regex = "(?<text>[\\S\\s]+?),(?<language>\\w+)".r
  val path = "/home/eivind/Nedlastinger/dataset.csv"
  Language.loadFromFile(csvParser, path)

  while(true) Language
    .classifyLanguage(readLine("Ready: "))
    .printScoreOfAll()
    .plotBarChart()
}

