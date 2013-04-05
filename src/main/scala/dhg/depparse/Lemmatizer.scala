package dhg.depparse

import edu.stanford.nlp.process.Morphology

object Lemmatize {

  private val prep = Set("abroad", "across", "after", "ahead", "along", "aside", "away", "around", "back", "down", "forward", "in", "off", "on", "over", "out", "round", "together", "through", "up")
  private val particles = prep

  private val morphology = new Morphology()

  def apply(taggedSentences: Seq[Seq[(String, String)]]): Seq[Seq[String]] = {
    for (sentence <- taggedSentences) yield {
      for ((word, tag) <- sentence) yield {
        apply(word, tag)
      }
    }
  }

  def apply(word: String, tag: String): String = {
    if (tag == null || tag.isEmpty)
      morphology.stem(word)
    else
      phrasalVerb(word, tag).getOrElse(morphology.lemma(word, tag))
  }

  /**
   * If a token is a phrasal verb with an underscore between a verb and a
   *  particle, return the phrasal verb lemmatized.
   */
  private[this] def phrasalVerb(word: String, tag: String): Option[String] = {
    if (tag.startsWith("VB"))
      word.split("_") match {
        case Array(base, particle) if particles(particle) => Some(morphology.lemma(base, tag) + '_' + particle)
        case _ => None
      }
    else
      None
  }

}
