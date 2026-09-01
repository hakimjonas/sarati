package net.ghoula.sarati.codec

/** Renames or excludes case-class fields at the AST boundary during derivation.
  *
  * `transformFieldName` maps the Scala field name to the key written to or read from the AST;
  * `shouldIncludeField` drops a field entirely — on encode the key is not written, on decode the
  * field is treated exactly like a missing key (`None` for an `Option` field, a
  * [[DecodeError.MissingField]] failure for a required one). Passed to
  * `Decoder.derived`/`Encoder.derived`; the default ([[FieldTransformer.default]]) leaves every
  * field as-is.
  */
trait FieldTransformer {
  def transformFieldName(fieldName: String): String
  def shouldIncludeField(fieldName: String): Boolean
}

object FieldTransformer {

  /** The derivation default when no transformer is given explicitly. */
  given default: FieldTransformer = IdentityFieldTransformer
}

/** The identity transformer: names pass through, no field is excluded. */
object IdentityFieldTransformer extends FieldTransformer {
  def transformFieldName(fieldName: String): String = fieldName
  def shouldIncludeField(fieldName: String): Boolean = true
}

/** Ready-made naming conventions. */
object FieldTransformers {

  /** `userId` → `user_id`. */
  object SnakeCase extends FieldTransformer {
    def transformFieldName(fieldName: String): String =
      fieldName.replaceAll("([A-Z])", "_$1").toLowerCase.stripPrefix("_")

    def shouldIncludeField(fieldName: String): Boolean = true
  }

  /** `userId` → `user-id`. */
  object KebabCase extends FieldTransformer {
    def transformFieldName(fieldName: String): String =
      fieldName.replaceAll("([A-Z])", "-$1").toLowerCase.stripPrefix("-")

    def shouldIncludeField(fieldName: String): Boolean = true
  }

  /** `userId` → `USER_ID`. */
  object ScreamingSnakeCase extends FieldTransformer {
    def transformFieldName(fieldName: String): String =
      fieldName.replaceAll("([A-Z])", "_$1").toUpperCase.stripPrefix("_")

    def shouldIncludeField(fieldName: String): Boolean = true
  }
}
