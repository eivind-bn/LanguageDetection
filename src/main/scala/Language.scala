



import java.lang.Character.UnicodeBlock._
import java.lang.Character.UnicodeScript.{HIRAGANA => Hiragana, KATAKANA => Katakana, _}
import java.lang.Character.{UnicodeBlock, UnicodeScript}
import scala.collection.mutable
import scala.io.Source
import scala.util.Try
import scala.util.Using.Manager
import scala.util.matching.Regex

/**
 * All languages are statically known. The machine cannot model new languages, only deduce origin of words
 * in existing ones. Words on the other hand is not statically known. New words can be deduced by context,
 * i.e. being surrounded by existing ones from known languages. Weights associated with heuristic words should
 * also change as the model progresses.
 *
 */

sealed trait Language { lang:Product =>

  //Mutable object. Limit scope as much as possible.
  private val entries: mutable.HashMap[String,Word] = mutable.HashMap()

  //Returns the immutable word-set of this language. NOTE: The origin of particular word dictates whether they are
  //mutable or not. Words from dataset are immutable. Words from untrusted data are mutable. This collection may
  //be comprised of both, and side-effect inducing methods on words will be undefined.
  def vocabulary: Set[Word] = entries.values.toSet

  def loadTrainData(text: String): Seq[Word] = splitWords(text)
    .flatMap(Word.parseTrain(_).toOption)
    .toSeq

  def loadSampleData(text: String): Seq[Word] = splitWords(text)
    .flatMap(Word.parseSample(_).toOption)
    .toSeq

  protected[this] def mayContain(chars: Char*): Boolean
  protected[this] def splitWords(text: String): Array[String] = text
    .strip()
    .toLowerCase
    .filter(char => char.isLetter || char.isWhitespace || char == '\'')
    .split("[\\s-]+")
    .filter(word => word.length > 1 && mayContain(word.toSeq:_*))


  sealed trait Word extends CharSequence {
    val text: String

    def score: Double
    def meanAdjust(totalScore: Double, numberOfWords: Int): Unit
    override def length(): Int = text.length
    override def charAt(index: Int): Char = text.charAt(index)
    override def subSequence(start: Int, end: Int): CharSequence = text.subSequence(start, end)
    override def equals(obj: Any): Boolean = obj match {
      case other: Word => this.text.equalsIgnoreCase(other.text)
      case other: String => this.text.equalsIgnoreCase(other)
      case _ => false
    }
  }
  private[this] object Word{

    def parseTrain(_text: String): Try[Word] = Try { new Word {
      override val text: String = _text
      // Insert this word disregarding prior entries. All words sourced from dataset are immutable, and behave the same,
      // so no if-checking required. If prior entry is sourced from normal input, desired behaviour is to invalidate it.
      entries.update(text, this)
      override def meanAdjust(totalScore: Double, numberOfWords: Int): Unit = {/*NOOP*/}
      override def score: Double = 1.0
      override def toString: String = s"${lang.productPrefix}.Word($text)"
    }}

    def parseSample(_text: String): Try[Word] = Try{ new Word {
      override val text: String = _text
      var _score: Double = entries.get(text).map(_.score).getOrElse(0.0)
      override def score: Double = _score
      def percent: Int = (score * 100).toInt

      // Inserts this instance if no equal entry exist. If it does, it could have been sampled from a dataset
      // which claims higher priority. This instance should then relay method calls to that instance instead.
      private val entry: Word = entries.getOrElseUpdate(text, this)

      override def meanAdjust(totalScore: Double, numberOfWords: Int): Unit = entry match {
        case entry if entry eq this => _score = (_score + (totalScore / numberOfWords))/2
        case entry => entry.meanAdjust(totalScore, numberOfWords)
      }

      override def toString: String = entry match {
        case entry if entry eq this => s"${lang.productPrefix}.Word($text p=$percent%)"
        case entry => entry.toString
      }
    }}
  }
}

case object Thai extends Language.Spanning(Language.Letters.thai)
case object Korean extends Language.Scripted(HANGUL, HAN) with Language.WhitespaceIgnored
case object Indonesian extends Language.Blocked(BASIC_LATIN)
case object Spanish extends Language.Spanning(Language.Letters.spanish)
case object Russian extends Language.Scripted(UnicodeScript.CYRILLIC)
case object Arabic extends Language.Scripted(UnicodeScript.ARABIC)
case object Latin extends Language.Blocked(BASIC_LATIN)
case object Estonian extends Language.Spanning(Language.Letters.estonian)
case object Dutch extends Language.Spanning(Language.Letters.dutch)
case object Portugese extends Language.Spanning(Language.Letters.portuguese)
case object Persian extends Language.Scripted(UnicodeScript.ARABIC)
case object Japanese extends Language.Scripted(Hiragana, Katakana, HAN) with Language.WhitespaceIgnored
case object Chinese extends Language.Scripted(HAN) with Language.WhitespaceIgnored
case object Hindi extends Language.Spanning(Language.Letters.hindi)
case object French extends Language.Spanning(Language.Letters.french)
case object Turkish extends Language.Spanning(Language.Letters.turkish)
case object English extends Language.Spanning(Language.Letters.english)
case object Tamil extends Language.Spanning(Language.Letters.tamil)
case object Romanian extends Language.Spanning(Language.Letters.romanian)
case object Pushto extends Language.Spanning(Language.Letters.pushto)
case object Swedish extends Language.Spanning(Language.Letters.swedish)
case object Urdu extends Language.Spanning(Language.Letters.urdu)

object Language{

  def dictionary: Map[String,Seq[Language#Word]] = languages.toSeq.flatMap(_.entries).groupMap(_._1)(_._2)

  def loadFromFile(regex: Regex, path: String): Unit = for{
    data <- Manager(manager => manager(Source.fromFile(path)).mkString)
    regex <- regex.findAllMatchIn(data)
    (lang,text) = regex.group("language") -> regex.group("text")
  } Language.forName(lang).foreach(_.loadTrainData(text))

  def classifyLanguage(sample: String): Unit = languages
    .map(lang => (lang, lang.loadSampleData(sample))) // Parse input text
    .map{ case (language, words) => (language, words, words.map(_.score).sum) } // Calculate score by language
    .maxByOption{ case (language, words, score) => score } // Pick highest score
    .tapEach{ case (language, words, score) => words.foreach(_.meanAdjust(score, words.length)) } // Adjust winner weights.
    .map{ case (language, words, score) => s"$language: ${words.mkString("\t")}" } // Generate result.
    .foreach(println) // Printing result. Will change in near commit.

  sealed abstract class Spanning(letters: Iterable[Char]*) extends Language { this:Product =>
    override def mayContain(chars: Char*): Boolean = chars
      .forall(letters.flatten.contains)
  }
  sealed abstract class Scripted(unicodeScripts: UnicodeScript*) extends Language { this:Product =>
    override def mayContain(chars: Char*): Boolean = chars
      .map(char => UnicodeScript.of(char.toInt))
      .forall(unicodeScripts.contains)
  }
  sealed abstract class Blocked(unicodeBlocks: UnicodeBlock*) extends Language { this:Product =>
    override def mayContain(chars: Char*): Boolean = chars
      .map(char => UnicodeScript.of(char.toInt))
      .forall(unicodeBlocks.contains)
  }

  trait WhitespaceIgnored{ this:Language =>
    override protected def splitWords(text: String): Array[String] = text
      .filter(char => char.isLetter && mayContain(char))
      .map(_.toLower)
      .map(_.toString)
      .toArray
  }

  object Letters{
    lazy val thai: Set[Char] = Set.range('\u0e00', '\u0e4f')
    lazy val spanish: Set[Char] = Set.range('a', 'z') ++ Set('ñ','á', 'é', 'í', 'ó', 'ú','ü')
    lazy val estonian: Set[Char] = Set('a','b','d','e','g','h','i','j','k','l','m','n', 'o','p','r','s','t','u','v','õ','ä','ö','ü')
    lazy val dutch: Set[Char] = Set.range('a', 'z') ++ Set('á','é','í','ó','ú','à','è','ë','ï','ö','ü','ĳ')
    lazy val portuguese: Set[Char] = Set.range('a', 'z') ++ Set('á','é','í','ó','ú','ç','â','ê','ô','ã', 'õ','à', 'è', 'ì', 'ò', 'ù')
    lazy val hindi: Set[Char] = Set.range('\u0900','\u097f') ++ Set.range('\uA8E0','\uA8FF') ++ Set.range('\u1CD0','\u1CFF')
    lazy val french: Set[Char] = Set.range('a', 'z') ++ Set('ç','é','â','ê','î','ô','û','à','è','ì','ò','ù','ë','ï','ü')
    lazy val turkish: Set[Char] = Set.range('a','z') ++ Set('ç','ğ','i','ö','ş','ü')
    lazy val english: Set[Char] = Set.range('a', 'z')
    lazy val tamil: Set[Char] = Set.range('\u0B80', '\u0BFF') ++ Set.range(0x11FC0.toChar, 0x11FFF.toChar)
    lazy val romanian: Set[Char] = Set.range('a', 'z') ++ Set('ă','â','î','ș','ț')
    lazy val swedish: Set[Char] = Set.range('a', 'z') ++ Set('å', 'ä', 'ö', 'é')
    lazy val urdu: Set[Char] = Set.range('\u0627', '\u06D2')
    lazy val pushto = Set('\u0627', '\u0622', '\u0628', '\u067E', '\u062A', '\u067C', '\u062B', '\u062C',
      '\u0686', '\u062D', '\u062E', '\u0685', '\u0681', '\u062F', '\u0689', '\u0630', '\u0631', '\u0693',
      '\u0632', '\u0698', '\u0696', '\u0633', '\u0634', '\u069A', '\u0635', '\u0636', '\u0637', '\u0638',
      '\u0639', '\u063A', '\u0641', '\u0642', '\u06A9', '\u06AB', '\u0644', '\u0645', '\u0646', '\u06BC',
      '\u06BA', '\u0648', '\u0647', '\u06C0', '\u064A', '\u06D0', '\u06CC', '\u06D2', '\u06CD', '\u0626')
  }

  def forName(name: String): Option[Language] = name.strip().toLowerCase match {
    case "thai" => Some(Thai)
    case "indonesian" => Some(Indonesian)
    case "spanish" => Some(Spanish)
    case "estonian" => Some(Estonian)
    case "russian" => Some(Russian)
    case "pushto" => Some(Pushto)
    case "arabic" => Some(Arabic)
    case "latin" => Some(Latin)
    case "persian" => Some(Persian)
    case "chinese" => Some(Chinese)
    case "japanese" => Some(Japanese)
    case "korean" => Some(Korean)
    case "hindi" => Some(Hindi)
    case "french" => Some(French)
    case "turkish" => Some(Turkish)
    case "english" => Some(English)
    case "tamil" => Some(Tamil)
    case "romanian" => Some(Romanian)
    case "dutch" => Some(Dutch)
    case "portugese" => Some(Portugese)
    case "swedish" => Some(Swedish)
    case "urdu" => Some(Urdu)
    case _ => None
  }

  def languages: Set[Language] = Set(
    Thai,
    Indonesian,
    Spanish,
    Estonian,
    Russian,
    Arabic,
    Latin,
    Persian,
    Chinese,
    Japanese,
    Korean,
    Hindi,
    French,
    Turkish,
    English,
    Tamil,
    Romanian,
    Dutch,
    Portugese,
    Pushto,
    Swedish,
    Urdu,
  )
}
