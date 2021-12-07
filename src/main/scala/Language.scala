
import java.lang.Character.UnicodeBlock._
import java.lang.Character.UnicodeScript.{HIRAGANA => Hiragana, KATAKANA => Katakana, _}
import java.lang.Character.{UnicodeBlock, UnicodeScript}
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
   * Different languages have different letter encodings. This method should return true i all chars are valid
   * in this particular language, or else false.
   *
   * @param chars The codepoints to validate.
   * @return True if ALL valid, or else false.
   */

  protected[this] def mayContain(chars: Char*): Boolean

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
    .filter(word => word.nonEmpty && mayContain(word.toSeq:_*))

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
     * @param totalScore The accounted sum of weights before adjustment has begun.
     * @param numberOfWords Number of words accounted for.
     */

    protected[Language] def meanAdjust(totalScore: Double, numberOfWords: Int): Unit
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
      override protected[Language] def meanAdjust(totalScore: Double, numberOfWords: Int): Unit = {/*NOOP*/}

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
      override protected[Language] def meanAdjust(totalScore: Double, numberOfWords: Int): Unit = {/*NOOP*/}
      override def score: Double = 1.0
      override def toString: String = s"${lang.productPrefix}.Word($text)"
    }

    /**
     * Produces a anonymous word instance with mutable fields. Weight of this word is initially the same
     * as other equal entries if any, or else 0.0 (the min-value).
     *
     * @param _text The properly formatted textual representation of this word (assumed to be correct).
     * @param adjustThreshold Threshold to surpass if weight adjustments is allowed to occur.
     *                        This threshold corresponds to amount of words present when classifying.
     *                        The default threshold is 6 words. Sentences below 6 words may yield unstable
     *                        classifications.
     * @return The new word.
     */

    def makeInduction(_text: String, adjustThreshold: Int = 6): Word = new Word {
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
       * @param totalScore The accounted sum of weights before adjustment has begun.
       * @param numberOfWords Number of words accounted for.
       */

      override protected[Language] def meanAdjust(totalScore: Double, numberOfWords: Int): Unit =
        if(numberOfWords > adjustThreshold) _score = (_score + (totalScore / numberOfWords))/2

      override def toString: String = s"${lang.productPrefix}.Word($text p=$percent%)"
    }
  }
}

/**
 * Declared languages supported by this model.
 */

case object Thai extends Language.Explicit(Language.Letters.thai) with Language.WhitespaceIgnored
case object Korean extends Language.Scripted(HANGUL, HAN) with Language.WhitespaceIgnored
case object Indonesian extends Language.Blocked(BASIC_LATIN)
case object Spanish extends Language.Explicit(Language.Letters.spanish)
case object Russian extends Language.Scripted(UnicodeScript.CYRILLIC)
case object Arabic extends Language.Scripted(UnicodeScript.ARABIC)
case object Latin extends Language.Blocked(BASIC_LATIN)
case object Estonian extends Language.Explicit(Language.Letters.estonian)
case object Dutch extends Language.Explicit(Language.Letters.dutch)
case object Portugese extends Language.Explicit(Language.Letters.portuguese)
case object Persian extends Language.Scripted(UnicodeScript.ARABIC)
case object Japanese extends Language.Scripted(Hiragana, Katakana, HAN) with Language.WhitespaceIgnored
case object Chinese extends Language.Scripted(HAN) with Language.WhitespaceIgnored
case object Hindi extends Language.Explicit(Language.Letters.hindi)
case object French extends Language.Explicit(Language.Letters.french)
case object Turkish extends Language.Explicit(Language.Letters.turkish)
case object English extends Language.Explicit(Language.Letters.english)
case object Tamil extends Language.Explicit(Language.Letters.tamil)
case object Romanian extends Language.Explicit(Language.Letters.romanian)
case object Pushto extends Language.Explicit(Language.Letters.pushto)
case object Swedish extends Language.Explicit(Language.Letters.swedish)
case object Urdu extends Language.Explicit(Language.Letters.urdu)
case object Bokmål extends Language.Explicit(Language.Letters.norwegian)
case object Nynorsk extends Language.Explicit(Language.Letters.norwegian)

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

  def loadFromResource(regex: Regex, name: String, unlabeledRatio: Double): TrainingResult =
    Random.shuffle(readData(regex, name)) match {
    case data =>
      val (unlabeledData, labeledData) = data.splitAt((data.length * unlabeledRatio).toInt)
      labeledData.foreach{ case (language, text) => language.loadLabeledData(text) }
      new TrainingResult(unlabeledData.map{ case (language, text) => (language, classifyLanguage(text)) })
  }

  /**
   * Classifies the provided test-data, then returns TestResult-object which
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

    temp
      .maxByOption{ case (language, score, words) => score } // Pick highest score
      .foreach{ case (language, score, words) => words.foreach(_.meanAdjust(score, words.length)) } // Adjust winner weights.

    new TestResult(result)
  }

  /**
   * Languages which inherits this class uses fine-grained filtering to filter out invalid characters.
   * The constructor accepts a iterable of explicitly defined letters which is valid in this language.
   * @param letters Explicitly defined letters.
   */

  sealed abstract class Explicit(letters: Iterable[Char]*) extends Language { this:Product =>
    override def mayContain(chars: Char*): Boolean = chars
      .forall(letters.flatten.contains)
  }

  /**
   * Languages which inherits this class uses defines their valid letter-encoding inaccurately by
   * unicode-scripts. In some languages the valid letters count may be too many too define explicitly.
   * @param unicodeScripts Valid Unicode-scripts for this language.
   */

  sealed abstract class Scripted(unicodeScripts: UnicodeScript*) extends Language { this:Product =>
    override def mayContain(chars: Char*): Boolean = chars
      .map(char => UnicodeScript.of(char.toInt))
      .forall(unicodeScripts.contains)
  }

  /**
   * A hybrid of Scripted and Explicit. Subclasses can inherit this class to define valid letters in smaller
   * unicode-blocks instead.
   * @param unicodeBlocks Valid Unicode-blocks for this language.
   */

  sealed abstract class Blocked(unicodeBlocks: UnicodeBlock*) extends Language { this:Product =>
    override def mayContain(chars: Char*): Boolean = chars
      .map(char => UnicodeScript.of(char.toInt))
      .forall(unicodeBlocks.contains)
  }

  /**
   * Some languages may not distinguish words by whitespace. This mixin trait can be applied to such languages
   * if such languages doesnt have a known policy for splitting words. If this trait is applied,
   * every character will be interpreted as a unique word instead.
   */

  trait WhitespaceIgnored{ this:Language =>
    override protected def splitWords(text: String): Array[String] = text
      .filter(char => char.isLetter && mayContain(char))
      .map(_.toLower)
      .map(_.toString)
      .toArray
  }

  /**
   * Definitions of different letter-schemas for different languages. Note these are explicitly defined.
   * Some languages may accept a broader range if they inherit Scripted or Blocked class instead.
   */

  object Letters{
    lazy val norwegian: Set[Char] = Set.range('a','z') ++ Set('æ','ø','å','é', 'è', 'ê', 'ó', 'ò', 'ô', 'ù', 'ü', 'è', 'à')
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

  /**
   * Accepts a language name, and return the corresponding language object if it exists.
   * @param name Name of the language. String is lower cased and stripped.
   * @return The language if any match.
   */

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
