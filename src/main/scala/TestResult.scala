import scala.concurrent.duration.{DurationInt, FiniteDuration}
import scala.util.{Failure, Success}

/**
 * Class used to analyse results of classifications.
 * @param data Necessary data derived from classification.
 */

class TestResult(data: Seq[(Language, Seq[Language#Word])]){

  case class Observation(language: Language, words: Seq[Language#Word]){
    val score: Double = words.map(_.score).sum
  }

  val observations: Seq[Observation] = data
    .map{ case (language, value) => Observation(language, value) }

  /**
   * Simply a convenience method eliminating the necessity of writing 'this' as the last statement after performing
   * stateful work.
   *
   * @param runnable The code to execute. For multi-statements, switch from brackets '()' to curly-brackets '{}'
   * @return this instance.
   */

  protected def execute(runnable: Unit): this.type = this

  /**
   * Finds the winner of this test-result.
   * @return The winner if any.
   */

  def findWinner: Option[Observation] = observations
    .maxByOption(_.score)
    .filter(_.score > 0.0001)

  /**
   * Prints winner with score.
   * @return This instance.
   */

  def printScoreOfWinner(): this.type = execute{
    for(winner <- findWinner){
      println(s"${winner.language}: ${winner.score}")
    }
  }

  /**
   * Prints score of all contenders in descending order.
   * @return This instance.
   */

  def printScoreOfAll(): this.type = execute{
    val contenders = observations.sortBy(-_.score)
    for(contender <- contenders){
      println(s"${contender.language}: ${contender.score}")
    }
  }

  /**
   * Plots python bar-chart visualizing the data of this test-result.
   * The y-axis is languages, and the x-axis is total score.
   * The individual segments on the bars is the different words of the classification,
   * and the width of segment is its attribution to the total score.
   * Colours of bars holds no significance other than making it easier to distinguish the segments.
   * @param timeout Time until plot is forced closed.
   * @return this instance.
   */

  def plotBarChart(timeout: FiniteDuration = 30.seconds): this.type = execute{

    val buffer = observations.toIndexedSeq //Need to buffer collection due to multiple traversals.
    val segmentCount = buffer.map(_.words.length).maxOption.getOrElse(0) //Find max word-count by each language. Bar-chart requires at least that many bars.
    def matrixElement(row: Int, column: Int): Option[Double] = buffer.unapply(row).flatMap(_.words.unapply(column)).map(_.score)
    val bars = Seq.tabulate(observations.size, segmentCount)((row,column) => matrixElement(row,column).getOrElse(0.0)) //Generate matrix. Rows length must match language count.
      .map(_.scan(0.0)(_ + _)) // Succeeding bar segments must be stacked. Need to adjust score to be sum of current score plus preceding score.
      .transpose // Because pyplot requires it.
      .map(_.mkString("plt.barh(ind, [", ",", "], width, zorder=3)")) // Convert rows to python list surrounded by plt.barh call.
      .reverse // Necessary to prevent segment overlapping.

    def pythonBarChart: String = {
      s"""
         |import numpy as np
         |import matplotlib.pyplot as plt
         |
         |
         |ind = np.arange(${observations.size})
         |width = 0.6
         |
         |fig = plt.figure('Language classification')
         |
         |${bars.mkString("\n")}
         |
         |plt.title('Score by language visualization')
         |plt.xlabel('Total score')
         |plt.yticks(ind, ${observations.map(result => s"'${result.language}'").mkString("[", ",", "]")})
         |plt.grid(True, linestyle = 'dashed', zorder=0)
         |
         |plt.show()
         |""".stripMargin
    }

    Python.execute(pythonBarChart) match {
      case Failure(exception) => exception
      case Success(value) =>
    }
  }
}
