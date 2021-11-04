

class Word private(val text: String, _languages: Language*) extends CharSequence with Iterable[Char] {

  val (primeWeights, heuristicWeights) = {
    val candidates = Language.values.filter(_.isSetOf(text))
    //Weights from data-set are never modified. Therefore immutable.
    val primeWeights = candidates
      .intersect(_languages.toSet)
      .map(lang => ImmutableWeight(lang))
    // Machine-learning requires mutation. Only permitted on data absent from data-set.
    val heuristicWeights = candidates
      .diff(_languages.toSet)
      .map(lang => MutableWeight(lang, 0.0))

    primeWeights -> heuristicWeights
  }

  def weights: Set[Weight] = primeWeights
    .map(_.asInstanceOf[Weight]) union heuristicWeights.map(_.asInstanceOf[Weight])

  // Returns each language which this word poses as a viable entry. For instance, 'hello' qualifies as english,
  // and could hypothetically be norwegian too due too intersecting alphabets,
  // but not chinese due to letters disjoint in chinese alphabet
  def intersections: Set[Language] = weights.map(_.language)

  def definedIn(language: Language): Boolean = primeWeights
    .exists(_.language == language)

  def definedIn(language: Language, tolerance: Double): Boolean = weights
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

  def parse(languages: Set[Language], text: String): Set[Word] = text
    .strip()
    .toLowerCase
    .filter(char => char.isLetter || char.isWhitespace || char == '\'')
    .split("[\\s-]+")
    .filterNot(word => word.length <= 1)
    .map(text => new Word(text, languages.toSeq:_*))
    .toSet

  def unapply(arg: Word): Option[(String, Seq[Weight])] = Some(arg.text -> arg.weights.toSeq)
}



