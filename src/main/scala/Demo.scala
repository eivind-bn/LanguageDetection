
import java.lang.Character.UnicodeBlock
import scala.io.StdIn.readLine
import scala.util.matching.Regex

/**
 * Personal notes:
 * 1. Languages are statically known and finite in number.
 *    Machine-learning are bounded to the languages present in the dataframe.
 *
 * 2. Words from data-set are assumed correct, and therefore immutable.
 * 3.
 *
 *
 * Data-set:
 *
 * word | language
 * ---------------------------
 * name | english
 * is   | english, norwegian
 * ---------------------------
 *
 * Sample:
 *
 * My name is John Doe.
 *
 * p_english = 2/5
 * p_norwegian = 1/5
 *
 */

object Demo extends App {

//  println('\u0628'.toLower)
//  println(Portugese.letters.toSeq.sortBy(_.toInt).map(c => c.toHexString -> c))

//  val csvParser: Regex = "(?<text>[\\S\\s]+?),(?<language>\\w+)".r
//  val path = "/home/eivind/Nedlastinger/dataset.csv"
//  val dictionary = Dictionary.readFromFile(csvParser, path).get
//
////  while (true) dictionary.classifyLanguage(readLine("Ready: "))
//  while (true) println(dictionary.findOrigins(readLine("Ready: ")).mkString)

//  val j = Japanese.loadWords("』の諸本は系統に分類される。すなわち、ヶ条（条ともみなされる）からなり、「簡素本」と呼ばれる写本群と、このヶ条を修正し且つ新た" +
//    "な条項を追加した、ヶ条からなる「拡大本」と呼ばれる写本群の系統である。さらに、「拡大本」と他の史")
//  val c = Chinese.parseWords("』の諸本は系統に分類される。すなわち、ヶ条（条ともみなされる）からなり、「簡素本」と呼ばれる写本群と、このヶ条を修正し且つ新た" +
//    "な条項を追加した、ヶ条からなる「拡大本」と呼ばれる写本群の系統である。さらに、「拡大本」と他の史")
//  val s = Spanish.loadWords("los primeros habitantes se establecieron cerca de  siendo un grupo de mixtecas " +
//    "zapotecas otomíes y olmecas tras la conquista de méxico la población fue encargada a sebastián y luisa narvaes " +
//    "se estableció como patrono a san andrés a quién se le creó una escultura en el siglo xvi el  de febrero de  " +
//    "el virrey luis de velasco y ruiz de alarcón otorgó un título en reconocimiento de las tierras " +
//    "comunales de los grupos originarios")

//  println(j)
//  println(c)
//  println(s)


  val csvParser: Regex = "(?<text>[\\S\\s]+?),(?<language>\\w+)".r
  val path = "/home/eivind/Nedlastinger/dataset.csv"
  Language.loadFromFile(csvParser, path)

  while(true){
    val input = readLine("Ready: ")
    Language.classifyLanguage(input)
  }

//  def yieldWeight(p0: Double, p: Double, n: Double) = {
//    (p0 + ((p + p0*n) / (n + p0*n))) / 2
//  }

//  def yieldWeight(p0: BigDecimal, p:Double, n: Int) =
//    (p0.pow(2)*n + 2*p0*n + p)/(2*n + 2*p0*n)
////
//  println(yieldWeight(0, 2, 5))
//
//  Iterator.iterate(yieldWeight(0, 2, 5))(next => yieldWeight(next, 2, 5))
//    .take(150)
//    .foreach(println)

}

