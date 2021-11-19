import scala.concurrent.duration.{DurationInt, FiniteDuration}

case class Result(language: Language, score: Double, words: Seq[Language#Word])
class ResultSet(data: Iterable[Result]){

  def this(data: Seq[(Language, Double, Seq[Language#Word])]) =
    this(data.map{ case (language, score, word) => Result(language, score, word) })

  /**
   * Simply a convenience method levitating the necessity of writing 'this' as the last statement after performing
   * stateful work.
   * @param runnable The code to execute. For multi-statements, switch from brackets '()' to curly-brackets '{}'
   * @return this instance.
   */
  private def execute(runnable: Unit): this.type = this

  private def findWinner: Option[Result] = data
    .maxByOption(_.score)
    .filter(_.score > 0.0001)

  def printScoreOfWinner(): this.type = execute{
    for(winner <- findWinner){
      println(s"${winner.language}: ${winner.score}")
    }
  }

  def printScoreOfAll(): this.type = execute{
    val contenders = data.toSeq.sortBy(-_.score)
    for(contender <- contenders){
      println(s"${contender.language}: ${contender.score}")
    }
  }

  def plotBarChart(timeout: FiniteDuration = 30.seconds): this.type = execute{

    val buffer = data.toIndexedSeq //Need to buffer collection due to multiple traversals.
    val segmentCount = buffer.map(_.words.length).maxOption.getOrElse(0) //Find max word-count by each language. Bar-chart requires at least that many bars.
    val bars = Seq.tabulate(22, segmentCount)((x,y) => buffer.unapply(x).flatMap(_.words.unapply(y)).map(_.score).getOrElse(0.0)) //Generate matrix. Rows length must match language count.
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
         |ind = np.arange(${data.size})
         |width = 0.6
         |
         |fig = plt.figure("Language classification")
         |
         |${bars.mkString("\n")}
         |
         |plt.xlabel('Total score')
         |plt.title('Language classification')
         |plt.yticks(ind, ${data.map(result => s"'${result.language}'").mkString("[", ",", "]")})
         |plt.grid(True, linestyle = "dashed", zorder=0)
         |
         |plt.show()
         |""".stripMargin
    }

    for(_ <- findWinner){ //Run only if any winner
      val process = new ProcessBuilder("python3", "-c", pythonBarChart)
        .inheritIO()
        .start()
      process.waitFor(timeout.length, timeout.unit)
      process.destroyForcibly()
    }
  }
}
