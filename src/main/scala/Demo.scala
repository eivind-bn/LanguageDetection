
import scala.collection.mutable
import scala.io.Source
import scala.reflect.ClassTag
import scala.util.Using.Manager
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

  val dictionary: mutable.Set[Word] = {
    val csvParser: Regex = "(?<text>[\\S\\s]+?),(?<language>\\w+)".r
    val rawData = Manager(_(Source.fromFile("/home/eivind/Nedlastinger/dataset.csv")).mkString).get
    val formattedData = csvParser.findAllMatchIn(rawData)
      .drop(1)
      .map(`match` => `match`.group("language") -> `match`.group("text"))
      .flatMap{ case (lang,text) => Word.parse(Set(Language.forName(lang).get), text)}
      .toList

    formattedData
      .groupBy(_.text)
      .values
      .flatten
      .to(mutable.HashSet)
  }

  dictionary.iterator.filter(_.definedIn(Thai)).map(_.intersections).foreach(println)
  for{
    word <- dictionary
    weight <- word.weights
  } if(weight.isInstanceOf[ImmutableWeight]) println(weight.language, word)

  Thread.sleep(10000)

}
