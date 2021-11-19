import scala.concurrent.duration.{DurationInt, FiniteDuration}

case class Result(language: Language, score: Double, words: Seq[Language#Word])
class ResultSet(data: Iterable[Result]){

  def this(data: Seq[(Language, Double, Seq[Language#Word])]) =
    this(data.map{ case (language, score, word) => Result(language, score, word) })

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
    for(winner <- findWinner){
      val process = new ProcessBuilder("python3", "-c", pythonBarChart)
        .inheritIO()
        .start()
//      process.waitFor(timeout.length, timeout.unit)
//      process.destroyForcibly()
    }

    def pythonBarChart: String = {
      s"""
         |import numpy as np
         |import matplotlib.pyplot as plt
         |
         |
         |N = 5
         |menMeans = [60, 35, 30, 35, 27]
         |womenMeans = [110, 32, 34, 20, 25]
         |menStd = [2, 3, 4, 1, 2]
         |womenStd = [3, 5, 2, 3, 3]
         |ind = np.arange(N)    # the x locations for the groups
         |width = 0.35       # the width of the bars: can also be len(x) sequence
         |
         |p1 = plt.bar(ind, menMeans, width, yerr=menStd)
         |p2 = plt.bar(ind, womenMeans, width, bottom=menMeans, yerr=womenStd)
         |p3 = plt.bar(ind, womenMeans, width, bottom=menMeans, yerr=womenStd)
         |
         |plt.ylabel('Scores')
         |plt.title('Language classification')
         |plt.xticks(ind, ['Thai', 'G2', 'G3', 'G4', 'G5'])
         |plt.legend(['Men', 'Women'])
         |
         |plt.show()
         |""".stripMargin

    }
  }
}
