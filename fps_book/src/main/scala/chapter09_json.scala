import MyJSONParser.JSON.jnumber

object MyJSONParser:

  import Combinator.P.*
  import Combinator.*

  val example: String =
    """
    {
      "embedded" : {"pizza" : {"mario" : "daisy"}, "peach" : "apple"},
      "company name": "Microsoft Corporation",
      "ticker" : "MSFT",
      "active" : true,
      "price"  : 30.66,
      "shares outstanding" : 8.38e9,
      "related companies"  : [ "HPQ", "IBM", @"YHOO", "DELL", "GOOG" ]
    }
    """


  enum JSON:
    case JNull
    case JNumber(get: Double)
    case JString(get: String)
    case JBool(get: Boolean)
    case JArray(get: IndexedSeq[JSON])
    case JObject(get: Map[String, JSON])

  object JSON:

    val jnull: Parser[JSON] = string("null").map((s: String) => JNull)
    val jnumber: Parser[JNumber] = (scientific | decimal | number ).map((s: String) => JNumber(s.toDouble))
    val jstring: Parser[JString] = quotedString.map((v: String) => JString(v))
    val jbool: Parser[JBool] = bool.map((b: Boolean) => JBool(b))
    val jarray: Parser[JArray] = for {
      _ <- char('[')
      items <- skipWhiteSpace(sequence(char(','), jsonParser))
      _ <- char(']')
    } yield JArray(items.toVector)
    val pair: Parser[(String, JSON)] = {
      for {
        k <- skipWhiteSpace(quotedString)
        _ <- char(':')
        v <- skipWhiteSpace(jsonParser)
      } yield (k, v)}
    val jobject: Parser[JObject] = for {
      _ <- char('{')
      pairs <- sequence(char(','), pair)
      _ <- char('}')
    } yield JObject(pairs.toMap)
    val jsonParser: Parser[JSON] = skipWhiteSpace(jnull | jbool | jnumber | jstring | jarray | jobject)



@main def run_chapter09_json: Unit =
  import MyJSONParser.JSON.*
  import Combinator.P.*

  // TESTS
//  println(jnull.run("null"))
//  println(jnumber.run("444.44E4"))
//  println(jstring.run(""""hel9403""""))
//  println(jbool.run("false"))
//  val test2 = sequence(char(','), jstring)
//  println(test2.run(""""aaa","aaa","aaa","aaa""""))
//  println(jarray.run("""[4,4,4,4]"""))
//  println(jarray.run("""[null,null]"""))
//  println(jarray.run("""[true,false]"""))
//  println(jarray.run("""["joost","bernard"]"""))
//  println(pair.run("""    "foobar"   :  4   """))
//  println(pair.run("""    "kl"   :  "dsf349208ds AAA"   """))
//  println(pair.run("""    "kl"   :  true   """))
//  println(pair.run("""    "kl"   :  [   "1",  3   ]      """))
  println(jsonParser.run(MyJSONParser.example))

