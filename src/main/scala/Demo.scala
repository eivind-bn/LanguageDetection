
import scala.io.StdIn.readLine
import scala.util.matching.Regex

object Demo extends App {

  val csvParser: Regex = "(?<text>[\\S\\s]+?),(?<language>\\w+)".r
  val path = "/home/eivind/Nedlastinger/dataset.csv"
  Language.loadFromFile(csvParser, path)

  Russian.vocabulary.foreach(println)

  while(true){
    val input = readLine("Ready: ")
    Language.classifyLanguage(input)
  }
}

