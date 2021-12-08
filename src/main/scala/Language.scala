
import Language.WhitespaceIgnored

import java.lang.Character.UnicodeScript
import java.lang.Character.UnicodeScript.{HAN, HANGUL, THAI, HIRAGANA => Hiragana, KATAKANA => Katakana}
import scala.collection.mutable
import scala.io.Source
import scala.util.Random
import scala.util.Using.Manager
import scala.util.matching.Regex

/**
 * All languages are statically known. New languages cannot be deduced by machine-learning, but only words.
 *
 * For a language to be incorporated in the detection-model, it must be a subtype of this trait.
 * This ensures all declared languages contain their own vocabulary, and are able to grow dynamically.
 * This model has the capability of continuous learning, which inherently requires mutation.
 * To cope with mutation in the safest manner, all methods which causes side-effects should be declared in
 * the scope of this superclass.
 */

sealed trait Language { lang:Product =>

  //Mutable object. Limit scope as much as possible.
  private[this] val entries: mutable.HashMap[String,Word] = mutable.HashMap()

  def vocabulary: Set[Word] = entries
    .values
    .filterNot(_.score == 0.0)
    .map(_.toImmutable)
    .toSet

  def loadAxioms(text: String): Seq[Word] = splitWords(text)
    .map(Axiom)
    .tapEach(word => entries.update(word.text, word))
    .toSeq

  def loadInductions(text: String): Seq[Word] = splitWords(text) match {
    case text if text.exists(entries.contains) => text
      .map(text => entries.getOrElseUpdate(text, Induction(text)))
      .toSeq

    case _ => Seq.empty
  }

  protected[this] def splitWords(text: String): Array[String] = text
    .strip()
    .toLowerCase
    .filter(char => char.isLetter || char.isWhitespace || char == '\'')
    .split("[\\s-]+")
    .filter(word => word.nonEmpty)


  sealed trait Word {
    val text: String

    def score: Double

    protected[Language] def meanAdjust(meanTotal: Double): Unit

    def confidence: Int = (score * 100).toInt

    private[Language] def invalidate(): Unit = entries.remove(text)

    def language: Language = lang

    def toImmutable: Word

    override def equals(obj: Any): Boolean = obj match {
      case other: Word => this.text.equalsIgnoreCase(other.text)
      case _ => false
    }
  }

  private[this] case class Axiom(override val text: String) extends Word {

    override def score: Double = 1.0

    override protected[Language] def meanAdjust(meanTotal: Double): Unit = {/*NOOP*/}

    override def toImmutable: Axiom = this

    override def toString: String = s"${lang.productPrefix}.Word($text)"
  }

  private[this] case class Induction(override val text: String) extends Word{ self =>

    protected[this] var _score: Double = entries.get(text).map(_.score).getOrElse(0.0)

    override def score: Double = _score

    override protected[Language] def meanAdjust(meanTotal: Double): Unit = _score = (_score + meanTotal)/2

    override def toImmutable: Induction = new Induction(text){
      override val score: Double = self.score
      override protected[Language] def meanAdjust(meanTotal: Double): Unit = {/*NOOP*/}
    }

    override def toString: String = s"${lang.productPrefix}.Word($text w=$confidence%)"
  }
}

/**
 * Declared languages supported by this model.
 */

case object Thai extends Language with WhitespaceIgnored {
  override def isValidChar(char: Char): Boolean = UnicodeScript.of(char) == THAI
}
case object Korean extends Language with WhitespaceIgnored{
  override def isValidChar(char: Char): Boolean = UnicodeScript.of(char) match {
    case script if script == HANGUL || script == HAN => true
    case _ => false
  }
}
case object Indonesian extends Language
case object Spanish extends Language
case object Russian extends Language
case object Arabic extends Language
case object Latin extends Language
case object Estonian extends Language
case object Dutch extends Language
case object Portugese extends Language
case object Persian extends Language
case object Japanese extends Language with WhitespaceIgnored{
  override def isValidChar(char: Char): Boolean = UnicodeScript.of(char) match {
    case script if script == Hiragana || script == Katakana || script == HAN => true
    case _ => false
  }
}
case object Chinese extends Language with WhitespaceIgnored{
  override def isValidChar(char: Char): Boolean = UnicodeScript.of(char) == HAN
}
case object Hindi extends Language
case object French extends Language
case object Turkish extends Language
case object English extends Language
case object Tamil extends Language
case object Romanian extends Language
case object Pushto extends Language
case object Swedish extends Language
case object Urdu extends Language
case object Bokmål extends Language
case object Nynorsk extends Language

object Language{

  /**
   * A global dictionary containing all words contained in all languages.
   * @return The dictionary.
   */

  def dictionary: Map[Language,Seq[Language#Word]] = values.toSeq
    .flatMap(_.vocabulary)
    .groupMap(_.language)(word => word)

  /**
   * Loads the data-set from resources.
   * @param regex regex to parse the csv.
   * @param name name of the resource.
   * @return A unsorted list of the tuples with languages and train-data.
   */

  private def readData(regex: Regex, name: String): Seq[(Language, String)] = for{
    data <- Manager(manager => manager(Source.fromResource(name)).mkString).toOption.toSeq
    regex <- regex.findAllMatchIn(data)
    (lang,text) = Language.forName(regex.group("language")) -> regex.group("text")
    result <- lang.zip(Some(text))
  } yield result

  /**
   * Performs 100% supervised-learning.
   * @param regex the regex to parse the csv.
   * @param name the name of the file.
   */

  def loadAxiomResource(regex: Regex, name: String): Unit = readData(regex, name)
    .foreach{ case (language, text) => language.loadAxioms(text) }

  def loadSampleResource(regex: Regex, name: String): TrainingResult = new TrainingResult(
    Random.shuffle(readData(regex, name)).map { case (language, text) => (language, classifyLanguage(text)) }
  )

  /**
   * Loads dataset from resources. Then performs semi-supervised training according to the ratio given.
   * The data is always shuffled before it's forwarded to the model.
   * @param regex csvParser to read the data.
   * @param name name of the file.
   * @param axiomAlloc A double in the range <0,1>. This percent-factor determines how much of
   *                        the dataset is stripped out to be used for unsupervised learning instead.
   * @return Training result. Allows further examining of the data.
   */

  def loadFromResource(regex: Regex, name: String, axiomAlloc: Double): TrainingResult = {
    val data = Random.shuffle(readData(regex, name))

    val (axioms, samples) = data.splitAt((data.length * axiomAlloc).toInt)

    for((language, text) <- axioms) language.loadAxioms(text)

    val result = samples.map{ case (language, text) => (language, classifyLanguage(text)) }

    new TrainingResult(result)
  }

  /**
   * Classifies the provided test-data, then returns Test Result-object which
   * can be used to analyse the result.
   * @param sample Sample of arbitrary many words.
   * @return TestResult which may be used to analyse result.
   */

  def classifyLanguage(sample: String): TestResult = {
    val temp = values
      .map(lang => (lang, lang.loadInductions(sample))) // Parse input text
      .map{ case (language, words) => (language, words.map(_.score).sum, words) } // Calculate score by language

    val result = temp
      .map{ case (language, score, words) => (language, words.map(_.toImmutable)) } // Make immutable copies to save current score.
      .toSeq

    for{
      (language, score, words) <- temp.maxByOption{ case (language, score, words) => score }
      meanTotal = score/words.length
      word <- words
    } word.meanAdjust(meanTotal) //Adjust words in classified language.

    new TestResult(result)
  }


  /**
   * Some languages may not distinguish words by whitespace. This mixin trait can be applied to such languages
   * if such languages doesnt have a known policy for splitting words. If this trait is applied,
   * every character will be interpreted as a unique word instead.
   *
   * NOTE!!
   * Languages which incorporate this trait must be bounded by a limited charset, and must never use
   * codepoints likely to occur in other languages unless that language also subtypes this trait.
   * Failure to comply will cause a major bias in the model, and non-subtypes will likely never be classified.
   */

  trait WhitespaceIgnored{ this:Language =>
    def isValidChar(char: Char): Boolean

    override protected def splitWords(text: String): Array[String] = text
      .filter(char => char.isLetter)
      .map(_.toLower)
      .filter(isValidChar)
      .map(_.toString)
      .toArray
  }

  /**
   * Accepts a language name, and return the corresponding language object if it exists.
   * @param name Name of the language. String is lower cased and stripped.
   * @return The language if any match.
   */

  def forName(name: String): Option[Language] = name.strip().toLowerCase match {
    case "indonesian" => Some(Indonesian)
    case "spanish" => Some(Spanish)
    case "estonian" => Some(Estonian)
    case "russian" => Some(Russian)
    case "pushto" => Some(Pushto)
    case "arabic" => Some(Arabic)
    case "latin" => Some(Latin)
    case "persian" => Some(Persian)
    case "thai" => Some(Thai)
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
    case "bokmål" => Some(Bokmål)
    case "nynorsk" => Some(Nynorsk)
    case _ => None
  }

  val values: Set[Language] = Set(
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
    Bokmål,
    Nynorsk
  )
}
