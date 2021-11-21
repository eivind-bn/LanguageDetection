import java.io.IOException
import scala.concurrent.duration.FiniteDuration
import scala.util.{Failure, Success, Try}

class ValidationResult(data: Seq[(Language, TestResult)]) {

  case class Observation(correctLanguage: Language, testResult: TestResult){
    def isCorrect: Boolean = testResult.findWinner.forall(_.language == correctLanguage)
    def isIncorrect: Boolean = !isCorrect
  }

  val observations: Seq[Observation] = data
    .map{ case (language, result) => Observation(language, result) }

  def execute(runnable: Unit): this.type = this

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
        process.waitFor(timeout.length, timeout.unit)
        process.destroyForcibly()

    }
  }
}
