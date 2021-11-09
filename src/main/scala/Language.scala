import scala.annotation.switch
import scala.collection.immutable.NumericRange

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


sealed class Language(_alphabet: Seq[Char]) {
  def this(other: Language) = this(other.alphabet.toSeq)
  val alphabet: Set[Char] = _alphabet.toSet
  def upperCase: Set[Char] = alphabet.map(_.toUpper)
  def lowerCase: Set[Char] = alphabet.map(_.toLower)
}

case object Thai extends Language('\u0e00' to '\u0e7f')
case object Korean extends Language(Latin)
case object Indonesian extends Language('\u1b00' to '\u1b7f')
case object Spanish extends Language(Latin.alphabet.toSeq ++ Set('á', 'é', 'í', 'ó', 'ú', 'ü'))
case object Estonian extends Language('a' to 'z')
case object Russian extends Language('a' to 'z')
case object Arabic extends Language('\u0600' to '\uFEFF')
case object Latin extends Language('a' to 'z')
case object Persian extends Language('\u0621' to '\u06CC')
case object Japanese extends Language(('\u3000' to '\u309f') ++ ('\u30a0' to '\u30ff') ++ ('\uff00' to '\uffef') ++ ('\u4e00' to '\u9faf'))
case object Chinese extends Language(('\u2e80' to '\u2fd5') ++ ('\u3190' to '\u319f') ++ ('\u3400' to '\u4dbf') ++ ('\u4e00' to '\u9fcc') ++ ('\uf900' to '\ufaad'))
case object Hindi extends Language('\u0900' to '\u097f')
case object French extends Language('a' to 'z')
case object Turkish extends Language(Latin.alphabet.toSeq ++ Seq('Ç', 'Ğ', 'I', 'İ', 'Ö', 'Ş', 'Ü'))
case object English extends Language(Latin)
case object Tamil extends Language('\u0B80' to '\u0BFA')
case object Romanian extends Language('a' to 'z')
case object Dutch extends Language(Latin.alphabet.toSeq :+ '\u0132')
case object Portugese extends Language('a' to 'z')
case object Pushto extends Language('a' to 'z')
case object Swedish extends Language(('a' to 'z') ++ Set('æ', 'ø', 'å'))
case object Urdu extends Language('\u0627' to '\u06D2')
object Language{

  def intersect(language: Language*): Language = language
    .map(_.alphabet)
    .reduceOption(_ intersect _)
    .map(alphabet => new Language(alphabet.toSeq))
    .getOrElse(new Language(Nil))

  def union(language: Language*): Language = language
    .map(_.alphabet)
    .reduceOption(_ union _)
    .map(alphabet => new Language(alphabet.toSeq))
    .getOrElse(new Language(Nil))

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
