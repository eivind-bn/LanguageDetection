
import scala.io.Source
import scala.io.StdIn.readLine
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

  val csvParser: Regex = "(?<text>[\\S\\s]+?),(?<language>\\w+)".r
  val path = "/home/eivind/Nedlastinger/dataset.csv"
  val dictionary = Dictionary.readFromFile(csvParser, path).get

  while (true) dictionary.classifyLanguage(readLine("Ready: "))




}

