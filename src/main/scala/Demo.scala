
import scala.annotation.tailrec
import scala.concurrent.duration.DurationInt
import scala.io.StdIn.readLine
import scala.util.{Failure, Success, Try}
import scala.util.matching.Regex

object Demo extends App {

  val csvParser: Regex = "(?<text>[\\S\\s]+?),(?<language>\\S+)".r
  val resource = "dataset-modded.csv"

  /**
   * Applies the data-set to the language-detection model.
   * The detection model will digest the data-set in 2 different ways.
   *
   * 1. Guided learning, i.e. supervised training.
   *    The detection model receives sample-texts along with the correct language.
   *    Words sources via guided learning is assumed axiomatically correct,
   *    and weights associated with these words are never adjusted.
   *    The direct and indirect presence of these words lay the foundation of continuous learning
   *    throughout the lifetime of the model.
   *
   * 2. Continuous learning, i.e. unsupervised learning.
   *    The model detects new words in the presence of both axiomatically known words, or other reasoned words.
   *    A sum of weights is calculated from existing words in every language, and the winning language
   *    will receive the unknown words into its vocabulary. Weights associated with words for this particular language
   *    is then re-adjusted.
   *
   * When the model initiates, user may choose to partition the data into train-data, and validation-data.
   * If 0% validation is specified, a 100% guided learning phase is executed.
   *
   * If more than 0% validation is requested, the data-set undergoes shuffling to eliminate patterns
   * present in the data set, and reduce risk of consistent startup-behaviour. The data is then partitioned
   * into train and validation-data. The train-data is initially applied to then model through guided learning.
   * The remaining validation-data is deducted by the model itself through continuous learning.
   *
   * The key difference is:
   *   - With train-data, the model has access to the solution, through which it can create axioms.
   *   - With validation-data, the model is required to deduce the solution, either in presence of axioms,
   *     or recursively in presence of other deduced words.
   *
   *
   * Side-note: validation-data is treated equivalent to test-data, except that test-data is applied continuously
   *            in smaller chunks, and therefore requires no shuffling.
   */

  @tailrec
  def initiateModel(): Unit = Try(readLine("Specify percent allocated to validation: ").toDouble) match {

    // Digest the data-set as train-data only. The complete startup-phase is conducted with guided learning.
    case Success(value) if value == 0.0  => Language.loadFromResource(csvParser, resource)

    // Data is partitioned into train and validation chunks.
    // Both guided learning and continuous learning occurs through the startup-phase.
    case Success(value) if value > 0.0 && value < 100.0 => Language
      .loadFromResource(csvParser, resource, value / 100)
      .printValidationSummary()
      .validationBarChart(timeout = 10.minutes)
      .plotAxiomDistribution(timeout = 10.minutes)

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

