sealed trait Weight{
  def language: Language
  def probability: Double
}
case class MutableWeight(language: Language, var probability: Double) extends Weight{

  def sigmoidAdjust(/* not implemented yet */): Unit = ???

  def binomialAdjust(/* not implemented yet */): Unit = ???

}
case class ImmutableWeight(language: Language) extends Weight{
  override def probability: Double = 1.0
}