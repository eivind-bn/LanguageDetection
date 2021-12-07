
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

  /**
   * Returns a immutable collection of every word contained in this language.
   * Every word is also copied to an immutable instance to ensure the score is stable.
   *
   * @return Every word presently contained in this language.
   */
  def vocabulary: Set[Word] = entries.values.map(_.copy).toSet

  /**
   * Assumes data is 100% correct with respect to this language, and inserts immutable instances of words into
   * its vocabulary. Since data is assumed correct, and instances are immutable, weights associated with
   * this words will never change.
   *
   * @param text Can be a string of arbitrary number of words.
   *             The string is split into words, and each respective word is inserted into the vocabulary if
   *             they satisfy this language letter encoding. Non-letter are simply ignored.
   * @return The inserted words.
   */

  def loadLabeledData(text: String): Seq[Word] = splitWords(text)
    .map(Word.makeAxiom)
    .tapEach(word => entries.update(word.text, word))
    .toSeq

  /**
   * Loads words into this language if at least one word exist already. These words are mutable instances
   * with adjustable weights. Weights are always initially 0, but may be adjusted after creation.
   *
   * @param text Can be a string of arbitrary number of words.
   *             The string is split into words, and each respective word is inserted into the vocabulary if
   *             they satisfy this language letter encoding. Non-letter are simply ignored.
   * @return The words already present in vocabulary, and the newly discovered ones if any.
   */

  private[Language] def loadNonLabeledData(text: String): Seq[Word] = splitWords(text) match {
    // Insert testdata in this language only if at least one word exist in it already.
    case text if text.exists(entries.contains) => text.map(text => entries.getOrElseUpdate(text, Word.makeInduction(text))).toSeq
    case _ => Seq.empty
  }


  /**
   * Splits a string of arbitrary words into its respective words. Every invalid codepoint should also be filtered out.
   * This method defaults to splitting word by whitespace or dashes. Any language which distinguish word by
   * other policies need to override this method.
   *
   * @param text text of arbitrary word length to split.
   * @return Every legal word for this language.
   */

  protected[this] def splitWords(text: String): Array[String] = text
    .strip()
    .toLowerCase
    .filter(char => char.isLetter || char.isWhitespace || char == '\'')
    .split("[\\s-]+")
    .filter(word => word.nonEmpty)

  /**
   * Trait for every word of this instance. May or may not be mutable.
   */

  sealed trait Word extends CharSequence { self =>

    /**
     * Unwrapped version of this word.
     */

    val text: String

    /**
     * The current score, i.e. weight of this word. The number must be in the range: 0.0 <= w <= 1.0
     *
     * @return The current score.
     */

    def score: Double

    /**
     * Discards this word from its language.
     */

    private[Language] def invalidate(): Unit = entries.remove(text)

    /**
     * Returns the outer instance of this particular word.
     *
     * @return The language that owns this instance.
     */

    def language: Language = lang

    /**
     * May or may not actually mutate the word depending on the subtype definition. If this word is desired
     * as immutable, then the subtype may simply override this method without any statements (NOOP).
     *
     * @param meanTotal Average score of all words in sample text.
     */

    protected[Language] def meanAdjust(meanTotal: Double): Unit
    override def length(): Int = text.length
    override def charAt(index: Int): Char = text.charAt(index)
    override def subSequence(start: Int, end: Int): CharSequence = text.subSequence(start, end)

    /**
     * Returns a safe immutable copy of this object with score eagerly computed. This is necessary if
     * score is too be preserved for statistical analysis.
     * @return A anonymous instance with stable and eagerly computed attributes.
     */

    def copy: Word = new Word {
      override val text: String = self.text
      override val score: Double = self.score
      override protected[Language] def meanAdjust(meanTotal: Double): Unit = {/*NOOP*/}

    }
    override def equals(obj: Any): Boolean = obj match {
      case other: Word => this.text.equalsIgnoreCase(other.text)
      case other: String => this.text.equalsIgnoreCase(other)
      case _ => false
    }
  }
  private[this] object Word{

    /**
     * Produces a anonymous word instance with immutable fields. Weight of this word is always 1.0 (the max-value),
     * and will never be influenced may adjustments.
     * @param _text The properly formatted textual representation of this word (assumed to be correct).
     * @return The new word.
     */

    def makeAxiom(_text: String): Word = new Word {
      override val text: String = _text
      override protected[Language] def meanAdjust(meanTotal: Double): Unit = {/*NOOP*/}
      override def score: Double = 1.0
      override def toString: String = s"${lang.productPrefix}.Word($text)"
    }

    /**
     * Produces a anonymous word instance with mutable fields. Weight of this word is initially the same
     * as other equal entries if any, or else 0.0 (the min-value).
     *
     * @param _text The properly formatted textual representation of this word (assumed to be correct).
     * @return The new word.
     */

    def makeInduction(_text: String): Word = new Word {
      override val text: String = _text

      /**
       * Current weight state of this word.
       */

      private[this] var _score: Double = entries.get(text).map(_.score).getOrElse(0.0)
      override def score: Double = _score

      /**
       * Yields score represented in percent. Note that this number is just a weight, not actually the
       * probability of being a authentic word. The machine cannot predict this.
       *
       * @return The weight in percent.
       */

      def percent: Int = (score * 100).toInt

      /**
       *
       * Method which performs the actual weight adjustment. For adjustment to happen, the minimum-word
       * threshold of this word must be surpassed. The math:
       *
       * 1. Calculate the average weight of all words.
       * 2. Calculate average of previously mentioned average, and the current weight in this word.
       *
       * Step 1 ensures the new score considers the whole sentence.
       * Step 2 ensures the new score is not applied in too strong magnitude too fast.
       *
       * For illustrative purposes, say the model encounters these 2 words:
       *
       * English.Word("hello", w=0.25), English.Word("world", w=0.75)
       *
       * Model has concluded the sample is english. Now it needs to make weight adjustments.
       *
       * After => English.Word("hello", w=0.375), English.Word("world", w=0.625)
       *
       * The idea is that the model is uncertain that English.Word("hello", w=0.25) is actually english.
       * However, it's more more certain that English.Word("world", w=0.75) is english judging by the current weight.
       * After adjustment, the model gained confidence that English.Word("hello", w=0.375) is english,
       * But lost some confidence in English.Word("world", w=0.625). After infinitely many iterations, the score will
       * converge at the midpoint.
       *
       * 0. English.Word("hello", w=0.25), English.Word("world", w=0.75)
       * 1. English.Word("hello", w=0.375), English.Word("world", w=0.625)
       * 2. English.Word("hello", w=0.4375), English.Word("world", w=0.5625)
       * 3. English.Word("hello", w=0.46875), English.Word("world", w=0.53125)
       * 4. English.Word("hello", w=0.484375), English.Word("world", w=0.51576)
       *
       *
       * Now lets illustrate the same case, but let 1 word be the immutable counterpart instead.
       *
       * 0. English.Word("hello", w=0.25), English.ImmutableWord("world", w=1.00)
       * 1. English.Word("hello", w=0.4375), English.ImmutableWord("world", w=1.00)
       * 2. English.Word("hello", w=0.578125), English.ImmutableWord("world", w=1.00)
       * 3. English.Word("hello", w=0.68359), English.ImmutableWord("world", w=1.00)
       * 4. English.Word("hello", w=0.76269), English.ImmutableWord("world", w=1.00)
       *
       * In this scenario the model encountered a train-data word which can be though of as an axiom.
       * The model became more and more confident that English.Word("hello", w=0.25) is english,
       * whilst loosing no confidence in English.ImmutableWord("world", w=1.00) since it's sourced from the
       * train-data and therefore must be correct. After infinitely many iterations, the model will
       * have 100% confidence in "hello" as well.
       *
       * @param meanTotal Average score of all words in sample text.
       */

      override protected[Language] def meanAdjust(meanTotal: Double): Unit = _score = (_score + meanTotal)/2

      override def toString: String = s"${lang.productPrefix}.Word($text p=$percent%)"
    }
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

  def loadFromResource(regex: Regex, name: String): Unit = readData(regex, name)
    .foreach{ case (language, text) => language.loadLabeledData(text) }

  /**
   * Loads dataset from resources. Then performs semi-supervised training according to the ratio given.
   * The data is always shuffled before it's forwarded to the model.
   * @param regex csvParser to read the data.
   * @param name name of the file.
   * @param unlabeledRatio A double in the range <0,1>. This percent-factor determines how much of
   *                        the dataset is stripped out to be used for unsupervised learning instead.
   * @return Training result. Allows further examining of the data.
   */

  def loadFromResource(regex: Regex, name: String, unlabeledRatio: Double): TrainingResult = {
    val data = Random.shuffle(readData(regex, name))

    val (unlabeledData, labeledData) = data.splitAt((data.length * unlabeledRatio).toInt)

    for((language, text) <- labeledData) language.loadLabeledData(text)

    val result = unlabeledData
      .map{ case (language, text) => (language, classifyLanguage(text)) }

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
      .map(lang => (lang, lang.loadNonLabeledData(sample))) // Parse input text
      .map{ case (language, words) => (language, words.map(_.score).sum, words) } // Calculate score by language

    val result = temp
      .map{ case (language, score, words) => (language, words.map(_.copy)) } // Make immutable copies to save current score.
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
   *
   * Languages which incorporate this trait must be bounded by a limited charset, and must never intersect
   * codepoints likely to occur in other languages unless that language also subtypes this trait.
   * Failure to comply cause a major bias in the model, and non-subtypes will likely never be classified.
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

  /**
   * List of all language objects incorporated in the detection-model.
   * @return The list of languages.
   */

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
