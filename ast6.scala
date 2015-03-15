  object JSonImpl6 {
    import fastparsers.framework.implementations.FastParsersCharArrayNoInline._
    import fastparsers.input.InputWindow.InputWindow
    val jsonparser = FastParsersCharArray{
      def value:Parser[Any] = whitespaces ~> (obj | arr | stringLit ^^ (_.toString) | decimalNumber | nullValue | trueValue | falseValue)
      def obj:Parser[Any] = '{' ~> repsep(member,comma) <~ closeBracket
      def arr:Parser[Any] = '[' ~> repsep(value,comma) <~ closeSBracket
      def member:Parser[Any] = stringLit ~ (lit(points) ~> value) ^^ {case (a, b) => (a.toString, b)}
    }
  }
