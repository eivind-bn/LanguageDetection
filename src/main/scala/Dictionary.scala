import scala.collection.mutable
import scala.io.Source
import scala.util.Try
import scala.util.Using.Manager
import scala.util.matching.Regex


class Dictionary(regex: Regex, rawData: String) extends Iterable[(String, Word)] with Map[String, Word] {

  protected val entries: collection.mutable.HashMap[String, Word] = regex.findAllMatchIn(rawData)
    .drop(1)
    .map(`match` => `match`.group("language") -> `match`.group("text"))
    .flatMap{ case (lang,text) => Word.parse(Set.from(Language.forName(lang)), text)}
    .toSeq
    .groupBy(_.text)
    .flatMap{ case (text, words) => Word.parse(words.flatMap(_.primeWeights).map(_.language).toSet, text)}
    .map(word => word.text -> word)
    .to(mutable.HashMap)

  override def removed(key: String): Map[String, Word] = entries.toMap.removed(key)

  override def updated[V1 >: Word](key: String, value: V1): Map[String, V1] = entries.toMap.updated(key, value)

  override def get(key: String): Option[Word] = entries.lift.apply(key)

  override def iterator: Iterator[(String, Word)] = entries.iterator

  def findOrigins(substring: String): List[String] = rawData.linesIterator
    .filter(_.contains(substring))
    .toList

  def classifyLanguage(sample: String): Unit = {
    val sampleWords = Word.parse(sample).toSeq
    val result = sampleWords
      .map(word => entries.getOrElseUpdate(word.text, word))
      .flatMap(_.weights)
      .groupBy(_.language)
      .map{ case (language, weights) => language -> weights.map(_.probability).sum }
      .tapEach { case (language, d) => println(language, d) }
      .maxBy(_._2)
      ._1

    println(s"Sample is most likely: $result")
  }
}
object Dictionary{

  def readFromFile[T](regex: Regex, path: String): Try[Dictionary] = Manager(_(Source.fromFile(path)).mkString)
    .map(new Dictionary(regex, _))
}
