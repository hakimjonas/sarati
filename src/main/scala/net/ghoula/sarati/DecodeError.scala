package net.ghoula.sarati

/** Structural decode failures: a required key absent ([[MissingField]]), an AST node of the wrong
  * shape ([[TypeMismatch]] — expected and actual type names), a value outside the target type's
  * domain ([[InvalidValue]]), or a decoder-specific condition ([[Custom]]).
  */
enum DecodeError {
  case MissingField(field: String, location: Location)
  case TypeMismatch(expected: String, actual: String, location: Location)
  case InvalidValue(message: String, location: Location)
  case Custom(message: String, location: Location)
}
