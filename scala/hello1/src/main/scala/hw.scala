package kakadu

import fastparsers.parsers.Parser

object Hw {

//  object JSonImpl1 {
//    import fastparsers.framework.implementations.FastParsers._
//    val jsonparser = FastParser{
//      def value:Parser[Any] = whitespaces ~> (obj | arr | stringLit | decimalNumber | "null" | "true" | "false")
//      def obj:Parser[Any] = '{' ~> repsep(member,",") <~ "}"
//      def arr:Parser[Any] = '[' ~> repsep(value,",") <~ "]"
//      def member:Parser[Any] = stringLit ~> ":" ~> value
//    }
//  }

  object KParser1 {
    val nullValue = "null".toCharArray
    //import fastparsers.framework.implementations.FastParsersCharArray._
    import fastparsers.framework.implementations.FastParsersCharArrayDefaultErrors._
    val kparser = FastParsersCharArray {
      def value: Parser[Any] = whitespaces ~> stringLit ~> nullValue
    }
  }


  //  val bigFileName = "" + "json.big1"
//  val bigFile = scala.io.Source.fromFile(bigFileName).getLines mkString "\n"
//  val bigFileArray = bigFile.toCharArray
//  val bigFileSeq = new FastCharSequence(bigFileArray)


  def main(args: Array[String]) = println("Hi!")
}
