

class Word(val text: String,
           val primeWeights: Set[ImmutableWeight],
           val heuristicWeights: Set[MutableWeight]) extends CharSequence with Iterable[Char] {

  def weights: Set[Weight] = primeWeights
    .map(_.asInstanceOf[Weight]) union heuristicWeights.map(_.asInstanceOf[Weight])

  // Returns each language which this word poses as a viable entry. For instance, 'hello' qualifies as english,
  // and could hypothetically be norwegian too due too intersecting alphabets,
  // but not chinese due to letters disjoint in chinese alphabet
  def intersections: Set[Language] = weights.map(_.language)

  def ∈(language: Language): Boolean = isMemberOf(language)
  def isMemberOf(language: Language): Boolean = primeWeights
    .exists(_.language == language)

  def ∉(language: Language): Boolean = nonMemberOf(language)
  def nonMemberOf(language: Language): Boolean = !isMemberOf(language)

  def isMemberOf(language: Language, tolerance: Double): Boolean = weights
    .exists(weight => weight.language == language && weight.probability >= tolerance)

  override def hashCode(): Int = text.hashCode
  override def equals(obj: Any): Boolean = obj match {
    case other: Word => other.text == this.text
    case other: String => other == text
    case _ => false
  }

  override def length(): Int = text.length
  override def charAt(index: Int): Char = text.charAt(index)
  override def subSequence(start: Int, end: Int): CharSequence = text.subSequence(start, end)
  override def isEmpty: Boolean = text.isEmpty
  override def iterator: Iterator[Char] = Iterator.tabulate(text.length)(text.charAt)

  override def toString: String = text
}
object Word{

  def intersects(text: String, language: Language*): Boolean = {
    val alphabet = language
      .map(_.alphabet)
      .reduceOption(_ union _)
      .getOrElse(Set.empty)

    text.forall(alphabet.contains)
  }

  private def normalize(text: String): Array[String] = text
    .strip()
    .toLowerCase
    .filter(char => char.isLetter || char.isWhitespace || char == '\'')
    .split("[\\s-]+")
    .filterNot(word => word.length <= 1)

  def parse(languages: Set[Language], text: String): Set[Word] = normalize(text)
    .map{ text =>
      val candidates = Language.values.filter(lang => text.forall(lang.alphabet.contains))
      val primeWeights = candidates
        .intersect(languages)
        .map(lang => ImmutableWeight(lang))
      val heuristicWeights = candidates
        .diff(languages)
        .map(lang => MutableWeight(lang, 0.0))
      new Word(text, primeWeights, heuristicWeights)
    }.toSet

  def parse(text: String): Set[Word] = normalize(text)
    .map{ text =>
      val primeWeights: Set[ImmutableWeight] = Set.empty
      val heuristicWeights: Set[MutableWeight] = Language
        .values
        .filter(lang => text.forall(lang.alphabet.contains))
        .map(lang => MutableWeight(lang, 0.0))
      new Word(text, primeWeights, heuristicWeights)
    }.toSet

  def unapply(arg: Word): Option[(String, Seq[Weight])] = Some(arg.text -> arg.weights.toSeq)
}



