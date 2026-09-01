package net.ghoula.sarati

/** Position of a decode or parse failure: `line` and `column` are 1-based, `offset` is 0-based from
  * the start of the input. Sarati's own structural decoders report the constant `(1, 1, 0)` — they
  * decode ASTs, not text — while text parsers fill in real positions.
  */
type Location = (line: Int, column: Int, offset: Int)
