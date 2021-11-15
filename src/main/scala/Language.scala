



import java.lang.Character.UnicodeBlock._
import java.lang.Character.UnicodeScript.{HIRAGANA => Hiragana, KATAKANA => Katakana, _}
import java.lang.Character.{UnicodeBlock, UnicodeScript}
import java.math.{MathContext, RoundingMode}
import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.Source
import scala.util.Using.Manager
import scala.util.matching.Regex

/**
 * All languages are statically known. The machine cannot model new languages, only deduce origin of words
 * in existing ones. Words on the other hand is not statically known. New words can be deduced by context,
 * i.e. being surrounded by existing ones from known languages. Weights associated with heuristic words should
 * also change as the model progresses.
 * These wrapper objects offers language inspections which is statically known, such as the alphabet.
 * Words which contains thai letters,
 * may trivially be discarded from english model, because english and thai alphabet have no mutual letters.
 * Words which is comprised of letters from intersecting alphabets, must be deduced by machine learning.
 *
 * NOTE: might disable alphabetic filtering to make the model more heuristic.
 */


/*
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
 * My name är John Doe.
 *
 * p_english = 2/5
 * p_norwegian = 1/5
 *
 */

sealed trait Language { lang:Product =>

  private val entries: mutable.HashMap[String,Word] = mutable.HashMap()

  sealed trait Word extends CharSequence {

    def score: Double
    val text: String

    entries.updateWith(text) {
      case Some(word: PrimeWord) => Some(word)
      case Some(_) => Some(this)
      case None => Some(this)
    }

    override def length(): Int = text.length
    override def charAt(index: Int): Char = text.charAt(index)
    override def subSequence(start: Int, end: Int): CharSequence = text.subSequence(start, end)
    override def equals(obj: Any): Boolean = obj match {
      case other: Word => this.text.equalsIgnoreCase(other.text)
      case other: String => this.text.equalsIgnoreCase(other)
      case _ => false
    }
  }
  case class PrimeWord(text: String) extends Word{
    override val score: Double = 1.0
    override def toString: String = s"${lang.productPrefix}.Word($text)"
  }
  case class HeuristicWord(text: String) extends Word{
    var score: Double = entries.get(text).map(_.score).getOrElse(0.0)
    def percent: Int = (BigDecimal(score).round(new MathContext(2, RoundingMode.HALF_UP))*100).toInt
    override def toString: String = s"${lang.productPrefix}.Word($text p=$percent%)"
  }

  def loadData(text: String): Seq[PrimeWord] = splitWords(text).map(PrimeWord).toSeq
  def mayContain(chars: Char*): Boolean
  protected def splitWords(text: String): Array[String] = text
    .strip()
    .toLowerCase
    .filter(char => char.isLetter || char.isWhitespace || char == '\'')
    .split("[\\s-]+")
    .filter(word => word.length > 1 && mayContain(word.toSeq:_*))
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

  def dictionary: Map[String,Seq[Language#Word]] = values.toSeq.flatMap(_.entries).groupMap(_._1)(_._2)

  def loadFromFile(regex: Regex, path: String): Unit = for{
    data <- Manager(manager => manager(Source.fromFile(path)).mkString)
    regex <- regex.findAllMatchIn(data)
    (lang,text) = regex.group("language") -> regex.group("text")
  } Language.forName(lang).foreach(_.loadData(text))

  def classifyLanguage(sample: String): Unit = values.map{lang =>
    val (primes, heuristic) = lang
      .splitWords(sample)
      .map(text => lang.entries.getOrElse(text, lang.HeuristicWord(text)))
      .partitionMap {
        case self@lang.PrimeWord(text) => Left(self)
        case self@lang.HeuristicWord(text) => Right(self)
      }

    val score = primes.map(_.score).sum + heuristic.map(_.score).sum

    (lang, score, primes, heuristic)
  }
    .filterNot(_._2 == 0.0)
    .maxByOption(_._2)
    .foreach{ case (language, score, primes, heuristic) =>

//      def yieldWeight(p0: BigDecimal, n: Int, n_total: Int) =
//        (p0.pow(2)*n + 2*p0*n + ((n_total.toDouble - n)/n_total))/(2*n + 2*p0*n)
      val p = (primes.map(_.score).sum + heuristic.map(_.score).sum) / (primes.length + heuristic.length)

      def yieldWeight(p0: BigDecimal, p:Double, n: Int) =
        (p0.pow(2)*n + 2*p0*n + p)/(2*n + 2*p0*n)

      for(hw <- heuristic) {
        hw.score = yieldWeight(hw.score, p, heuristic.length).toDouble
      }

      println(s"$language: ${primes.mkString("\t")} ${heuristic.mkString("\t")}")
    }


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
    case "korean" => Some(Korean)
    case "indonesian" => Some(Indonesian)
    case "spanish" => Some(Spanish)
    case "estonian" => Some(Estonian)
    case "russian" => Some(Russian)
    case "pushto" => Some(Pushto)
    case "arabic" => Some(Arabic)
    case "latin" => Some(Latin)
    case "persian" => Some(Persian)
    case "japanese" => Some(Japanese)
    case "chinese" => Some(Chinese)
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

  def values: Set[Language] = Set(
    Thai,
    Korean,
    Indonesian,
    Spanish,
    Estonian,
    Russian,
    Arabic,
    Latin,
    Persian,
    Japanese,
    Chinese,
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
