
import scala.annotation.tailrec
import scala.concurrent.duration.DurationInt
import scala.io.StdIn.readLine
import scala.util.{Failure, Success, Try}
import scala.util.matching.Regex

object Demo extends App {

  val csvParser: Regex = "(?<text>[\\S\\s]+?),(?<language>\\w+)".r
  val resource = "dataset.csv"

  @tailrec
  def initiateModel(): Unit = Try(readLine("Specify percent allocated to validation: ").toDouble) match {

    case Success(value) if value == 0.0  => Language.loadFromResource(csvParser, resource)

    case Success(value) if value > 0.0 && value < 100.0 => Language
      .loadFromResource(csvParser, resource, value / 100)
      .printValidationSummary()
      .validationBarChart(timeout = 10.minutes)

    case Success(_) =>
      println("Value must be bounded as follows: 0.0 <= p < 100.0. Please try again.")
      initiateModel()

    case Failure(exception) =>
      println("Could not be interpreted as double. Please try again.")
      initiateModel()
  }

  initiateModel()

  while(true) Language
    .classifyLanguage(readLine("Ready: "))
    .printScoreOfWinner()
    .plotBarChart(timeout = 10.minutes)
}

