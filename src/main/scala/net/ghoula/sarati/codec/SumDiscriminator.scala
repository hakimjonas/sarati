package net.ghoula.sarati.codec

/** Names the tag property written to and read from the AST for sum-type derivation.
  *
  * `Encoder.derived`/`Decoder.derived` tag product cases of enums and sealed hierarchies with this
  * property (by default `"_type"` holding the case label, with an `"_ordinal"` integer fallback on
  * decode). Provide a custom given in the derivation scope to rename the tag property — for example
  * to match an external contract's discriminator field:
  *
  * {{{
  * given SumDiscriminator = new SumDiscriminator:
  *   def propertyName: String = "kind"
  * given Encoder[Shape, JsonValue] = Encoder.derived
  * }}}
  *
  * Simple parameterless enums encode as bare strings and never carry the tag.
  */
trait SumDiscriminator {
  def propertyName: String
}

object SumDiscriminator {

  /** The derivation default when no transformer is given explicitly. */
  given default: SumDiscriminator = DefaultSumDiscriminator
}

/** The default discriminator property: `"_type"`. */
object DefaultSumDiscriminator extends SumDiscriminator {
  def propertyName: String = "_type"
}
