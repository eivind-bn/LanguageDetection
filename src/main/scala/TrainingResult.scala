import scala.concurrent.duration.FiniteDuration
import scala.util.{Failure, Success}

/**
 * Class used to analyse results from semi-supervised training.
 * @param data Necessary data derived from classification.
 */

class TrainingResult(data: Seq[(Language, TestResult)]) {

  case class Observation(correctLanguage: Language, testResult: TestResult){

    /**
     * Tests if classification is correct.
     * @return True if correct.
     */

    def isCorrect: Boolean = testResult.findWinner.exists(_.language == correctLanguage)

    /**
     * Tests if classification is incorrect.
     * @return False if correct.
     */

    def isIncorrect: Boolean = testResult.findWinner.exists(_.language != correctLanguage)
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
   * Prints short summary from the semi-supervised training phase.
   * @return This instance.
   */

  def printTrainingSummary(): this.type = execute{
    val rights = observations.filter(_.isCorrect)
    val wrongs = observations.filter(_.isIncorrect)
    val nulls = observations.filterNot(obs => obs.isCorrect || obs.isIncorrect)
    println(
      s"""
         |Training summary:${Console.GREEN}
         |Correct guesses: '${rights.size}'
         |Success-rate: '${rights.size.toDouble / (rights.size + wrongs.size + nulls.size)}'${Console.RED}
         |Wrong guesses: '${wrongs.size}'
         |Fail-rate: '${wrongs.size.toDouble / (rights.size + wrongs.size + nulls.size)}'${Console.YELLOW}
         |Null guesses: '${nulls.size}'
         |Null-rate: '${nulls.size.toDouble / (rights.size + wrongs.size + nulls.size)}'${Console.RESET}
         |""".stripMargin
    )
  }

  /**
   * Plots python bar-chart visualizing the the distribution of axiom-count vs induction-count.
   * The y-axis represents languages, and the absolute value of the x-axis
   * is the number of axioms and inductions respectively.
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

    val inductionCounts = deductions
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
         |fig = plt.figure('Semi-supervised training summary')
         |
         |${axiomCounts.mkString("plt.barh(ind, [", ",", "], width, zorder=3)")}
         |${inductionCounts.mkString("plt.barh(ind, [", ",", "], width, zorder=3)")}
         |
         |plt.title('Visualization of axiom-count vs induction-count')
         |plt.xlabel('Axiom-count vs induction-count')
         |plt.yticks(ind, ${language.map(language => s"'$language'").mkString("[", ",", "]")})
         |plt.grid(True, linestyle = 'dashed', zorder=0)
         |
         |plt.show()
         |""".stripMargin
    }

    Python.execute(pythonBarChart) match {
      case Failure(exception) => System.err.println(s"Error plotting axiom/induction barchart: ${exception.getMessage}")
      case Success(value) =>
    }
  }

  /**
   * Plots python bar-chart visualizing wrong-guesses and right-guesses for each language respectively.
   * The y-axis represents languages, and the x-axis is the number of right and wrong guesses.
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
         |fig = plt.figure('Semi-supervised training summary')
         |
         |${truePositive.map(_._2).mkString("plt.barh(ind, [", ",", "], width, zorder=3)")}
         |${falsePositive.mkString("plt.barh(ind, [", ",", "], width, zorder=3)")}
         |
         |plt.title('Comparison of false/true classifications')
         |plt.xlabel('False-classifications vs true-classifications')
         |plt.yticks(ind, ${truePositive.map{ case (language, _) => s"'$language'"}.mkString("[", ",", "]")})
         |plt.grid(True, linestyle = 'dashed', zorder=0)
         |
         |plt.show()
         |""".stripMargin
    }

    Python.execute(pythonBarChart) match {
      case Failure(exception) => System.err.println(s"Error plotting true/false barchart: ${exception.getMessage}")
      case Success(value) =>
    }
  }

  def scatterViabilityTrend(timeout: FiniteDuration): this.type = execute {

    val temp = observations
      .scanLeft(0)((x, y) => y match {
        case y if y.isCorrect => x + 1
        case y if y.isIncorrect => x - 1
        case _ => x
      }).zipWithIndex

    val points = (temp.size.toDouble / 13000).round match {
      case 0 => temp
      case n => temp.filter{ case (y, index) => index % n == 0 }
    }

    def pythonViabilityTrend: String = {
      s"""
         |import numpy as np
         |import matplotlib.pyplot as plt
         |
         |
         |ind = ${points.map{ case (y, index) => index }.mkString("[",",","]")}
         |
         |fig = plt.figure('Semi-supervised training summary')
         |
         |${points.map{ case (y, index) => y }.mkString("plt.scatter(ind, [", ",", "])")}
         |
         |plt.title('success/failure trend (positive-slope=success, negative-slope=failure)')
         |plt.xlabel('Classification number')
         |plt.ylabel('successes minus failures')
         |plt.grid(True, linestyle = 'dashed')
         |
         |ax = plt.gca()
         |${points.lastOption.map{ case (y, index) => s"ax.set_ylim([${-index}, $index])" }.getOrElse("")}
         |
         |plt.show()
         |""".stripMargin
    }

    Python.execute(pythonViabilityTrend) match {
      case Failure(exception) => System.err.println(s"Error scattering train-history: ${exception.getMessage}")
      case Success(value) =>
    }
  }
}
