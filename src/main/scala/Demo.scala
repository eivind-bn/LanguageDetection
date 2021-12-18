
import scala.annotation.tailrec
import scala.concurrent.duration.DurationInt
import scala.io.StdIn.readLine
import scala.util.matching.Regex
import scala.util.{Failure, Success, Try}

object Demo extends App {

  val csvParser: Regex = "(?<text>[\\S\\s]+?),(?<language>\\S+)".r
  val resource = "dataset-modded.csv"

  @tailrec
  def initiateModel(): Unit = Try(readLine("Specify percent allocated to axioms: ").toDouble) match {

    // Digest the data-set as train-data only. The complete startup-phase is conducted with guided learning.
    case Success(value) if value == 100.0  => Language.loadAxiomResource(csvParser, resource)

    // Data is partitioned into train and validation chunks.
    // Both guided learning and continuous learning occurs through the startup-phase.
    case Success(value) if value > 0.0 && value < 100.0 => Language
      .loadFromResource(csvParser, resource, value / 100)
      .printTrainingSummary()
      .validationBarChart(timeout = 10.minutes)
      .plotAxiomDistribution(timeout = 10.minutes)

    case Success(_) =>
      println("Value must be bounded as follows: 0.0 < p <= 100.0. Please try again.")
      initiateModel()

    case Failure(exception) =>
      println("Could not be interpreted as double. Please try again.")
      initiateModel()
  }

  println(Console.RED + "Note: Python3, numpy and matplotlib required. " +
    "Ignore this if dependencies is properly installed" + Console.RESET)


  initiateModel()

  while(true) Language
    .classifyLanguage(readLine("Ready: "))
    .printScoreOfWinner()
    .plotBarChart(timeout = 10.minutes)
}

