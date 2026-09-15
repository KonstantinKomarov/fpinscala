package fpinscala.exercises.parsing

enum JSON:
  case JNull
  case JNumber(get: Double)
  case JString(get: String)
  case JBool(get: Boolean)
  case JArray(get: IndexedSeq[JSON])
  case JObject(get: Map[String, JSON])

object JSON:
  def jsonParser[Parser[+_]](P: Parsers[Parser]): Parser[JSON] = 
    import P.*

    def token(s: String) = string(s).token

    def array: Parser[JSON] = (
      token("[") *> value.sep(token(",")).map(vs => JArray(vs.toIndexedSeq)) <* token("]")
    ).scope("array")

    def obj: Parser[JSON] = ( 
      token("{") *> keyval.sep(token(",")).map(kvs => JObject(kvs.toMap)) <* token("}")
    ).scope("object")

    def keyval: Parser[(String, JSON)] = escapedQuoted ** (token(":") *> value)

    def lit: Parser[JSON] = (
      token("null").as(JNull) | 
      double.map(JNumber(_)) |
      escapedQuoted.map(JString(_))
    ).scope("literal")

    def value: Parser[JSON] = lit | obj | array

    (ws *> (obj | array)).root