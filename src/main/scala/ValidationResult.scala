import java.io.IOException
import java.util.{Timer, TimerTask}
import scala.concurrent.duration.FiniteDuration
import scala.util.{Failure, Success, Try}

/**
 * Class used to analyse results of validations
 * @param data Necessary data derived from classification.
 */

class ValidationResult(data: Seq[(Language, TestResult)]) {

  case class Observation(correctLanguage: Language, testResult: TestResult){

    /**
     * Tests if classification is correct.
     * @return True if correct.
     */

    def isCorrect: Boolean = testResult.findWinner.forall(_.language == correctLanguage)

    /**
     * Tests if classification is incorrect.
     * @return False if correct.
     */

    def isIncorrect: Boolean = !isCorrect
  }

  val observations: Seq[Observation] = data
    .map{ case (language, result) => Observation(language, result) }

  /**
   * Simply a convenience method eliminating the necessity of writing 'this' as the last statement after performing
   * stateful work.
   *
   * @param runnable The code to execute. For multi-statements, switch from brackets '()' to curly-brackets '{}'
   * @return this instance.
   */

  private def execute(runnable: Unit): this.type = this

  /**
   * Prints short summary of validation.
   * @return This instance.
   */

  def printValidationSummary(): this.type = execute{
    val (rights, wrongs) = observations.partition(_.isCorrect)
    println(
      s"""
         |Validation summary:
         |Correct guesses: '${rights.size}'
         |Wrong guesses: '${wrongs.size}'
         |ratio: '${wrongs.size.toDouble / rights.size}'
         |""".stripMargin
    )
  }

  /**
   * Plots python bar-chart visualizing the the distribution of axioms vs deductions
   * The y-axis is languages, and the absolute value of the x-axis
   * is the number of axioms and deductions respectively.
   * @param timeout Time until plot is forced closed.
   * @return this instance.
   */

  def plotAxiomDistribution(timeout: FiniteDuration): this.type = execute{

    val (language, axioms, deductions) = Language
      .dictionary
      .map{ case (language, words) => language -> words.partition(_.score == 1.0) }
      .map{ case (language, (axioms, deductions)) => (language, axioms, deductions) }
      .unzip3

    val axiomCounts = axioms
      .map(-_.size)

    val deductionCounts = deductions
      .map(words => words.filterNot(_.score == 0.0))
      .map(_.size)

    def pythonBarChart: String = {
      s"""
         |import numpy as np
         |import matplotlib.pyplot as plt
         |
         |
         |ind = np.arange(${language.size})
         |width = 0.6
         |
         |fig = plt.figure("Continuous learning summary")
         |
         |${axiomCounts.mkString("plt.barh(ind, [", ",", "], width, zorder=3)")}
         |${deductionCounts.mkString("plt.barh(ind, [", ",", "], width, zorder=3)")}
         |
         |plt.title('Visualization of axiom/deduction distribution')
         |plt.xlabel('Axiom/Deduction')
         |plt.yticks(ind, ${language.map(language => s"'$language'").mkString("[", ",", "]")})
         |plt.grid(True, linestyle = "dashed", zorder=0)
         |
         |plt.show()
         |""".stripMargin
    }

    def runPython(): Try[Process] = Try(new ProcessBuilder("python3", "-c", pythonBarChart).inheritIO().start())

    if(observations.nonEmpty) runPython() match {
      case Failure(_: IOException) => println(Console.RED + "Error: Could not visualize axiom/deduction distribution. " +
        "Ensure python3, numpy, and matplotlib is installed." + Console.RESET)

      case Failure(exception) => throw exception

      case Success(process) =>
        val forceCloser = new Timer()
        forceCloser.schedule(new TimerTask { override def run(): Unit = process.destroyForcibly() }, timeout.toMillis)
    }
  }

  /**
   * Plots python bar-chart visualizing the data of this validation-result.
   * The y-axis is languages, and the x-axis is the number of right and wrong guesses.
   * @param timeout Time until plot is forced closed.
   * @return this instance.
   */

  def validationBarChart(timeout: FiniteDuration): this.type = execute{

    val truePositive = observations
      .filter(_.isCorrect)
      .flatMap(_.testResult.findWinner)
      .groupBy(_.language)
      .map{ case (language, winner) => language -> winner.length }
      .toSeq

    val temp = observations
      .filter(_.isIncorrect)
      .flatMap(_.testResult.findWinner)
      .groupBy(_.language)
      .map{ case (language, winner) => language -> -winner.length }
      .toIndexedSeq

    val falsePositive = Seq.tabulate(truePositive.length)(index => temp.unapply(index).map(_._2).getOrElse(0))

    def pythonBarChart: String = {
      s"""
         |import numpy as np
         |import matplotlib.pyplot as plt
         |
         |
         |ind = np.arange(${truePositive.length})
         |width = 0.6
         |
         |fig = plt.figure("Validation summary")
         |
         |${truePositive.map(_._2).mkString("plt.barh(ind, [", ",", "], width, zorder=3)")}
         |${falsePositive.mkString("plt.barh(ind, [", ",", "], width, zorder=3)")}
         |
         |plt.title('Comparison of false/true classifications')
         |plt.xlabel('False/True')
         |plt.yticks(ind, ${truePositive.map{ case (language, _) => s"'$language'"}.mkString("[", ",", "]")})
         |plt.grid(True, linestyle = "dashed", zorder=0)
         |
         |plt.show()
         |""".stripMargin
    }

    def runPython(): Try[Process] = Try(new ProcessBuilder("python3", "-c", pythonBarChart).inheritIO().start())

    if(observations.nonEmpty) runPython() match {
      case Failure(_: IOException) => println(Console.RED + "Error: Could not visualize validation. " +
        "Ensure python3, numpy, and matplotlib is installed." + Console.RESET)

      case Failure(exception) => throw exception

      case Success(process) =>
        val forceCloser = new Timer()
        forceCloser.schedule(new TimerTask { override def run(): Unit = process.destroyForcibly() }, timeout.toMillis)
    }
  }
}
