import scala.collection.mutable
import scala.collection.mutable.ListBuffer
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
    val l = data.map(_.words.length).maxOption.getOrElse(0)

    val bars: ListBuffer[Array[Double]] = ListBuffer()
    for(i <- 0 to l){
      val arr = new Array[Double](22)
      data.map(_.words.unapply(i).map(_.score).getOrElse(0.0)).zipWithIndex.foreach(g => arr.update(g._2, g._1))
      bars.addOne(arr)
    }
    for(i <- 1 to l){
      for{
        a <- bars.unapply(i)
        b <- bars.unapply(i + 1)
      } b.zipWithIndex.foreach{ case (d, i) => b(i) = d + a(i) }
    }

    for(winner <- findWinner){
      val process = new ProcessBuilder("python3", "-c", pythonBarChart)
        .inheritIO()
        .start()
      process.waitFor(timeout.length, timeout.unit)
      process.destroyForcibly()
    }

    def pythonBarChart: String = {
      s"""
         |import numpy as np
         |import matplotlib.pyplot as plt
         |
         |
         |ind = np.arange(${Language.languages.size})
         |width = 0.5
         |
         |${bars.reverse
        .map(_.mkString("[",",","]"))
        .zipWithIndex
        .map{ case (list, i) => (list, if(i % 2 == 0) "'white'" else "'lightgrey'") }
        .map{ case (list, color) => s"plt.barh(ind, $list, width, color=$color, edgecolor='skyblue', zorder=3)" }
        .mkString("\n")}
         |
         |plt.xlabel('Total score')
         |plt.title('Language classification')
         |plt.yticks(ind, ${data.map(result => s"'${result.language}'").mkString("[", ",", "]")})
         |plt.grid(True, linestyle = "dashed", zorder=0)
         |
         |plt.show()
         |""".stripMargin

    }
  }
}
