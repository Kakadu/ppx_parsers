object JSonImpl6 {
    import FastParsersCharArrayNoInline._
    import InputWindow.InputWindow
    val jsonparser = FastParsersCharArray{
      def value:Parser[Any] = whitespaces ~> (obj | arr | stringLit ^^ (_.toString) | decimalNumber | nullValue | trueValue | falseValue)
      def obj:Parser[Any] = '{' ~> repsep(member,comma) <~ closeBracket
      def arr:Parser[Any] = '[' ~> repsep(value,comma) <~ closeSBracket
      def member:Parser[Any] = stringLit ~ (lit(points) ~> value) ^^ {case (a, b) => (a.toString, b)}
    }
  }

performing macro expansion FastParsersCharArrayNoInline.FastParsersCharArray({
  def value: fastparsers.parsers.Parser[Any] = FastParsersCharArrayNoInline.baseParsers[InputWindow.InputWindow[Array[Char]]](FastParsersCharArrayNoInline.whitespaces).~>[Any](FastParsersCharArrayNoInline.baseParsers[Any](FastParsersCharArrayNoInline.baseParsers[Any](FastParsersCharArrayNoInline.baseParsers[Any](FastParsersCharArrayNoInline.baseParsers[Any](FastParsersCharArrayNoInline.baseParsers[Any](FastParsersCharArrayNoInline.baseParsers[Any](obj).|[Any](arr)).|[Any](FastParsersCharArrayNoInline.baseParsers[InputWindow.InputWindow[Array[Char]]](FastParsersCharArrayNoInline.stringLit).^^[String](((x$1: InputWindow.InputWindow[Array[Char]]) => x$1.toString())))).|[Any](FastParsersCharArrayNoInline.decimalNumber)).|[Any](FastParsersCharArrayNoInline.lit(Hw.this.nullValue))).|[Any](FastParsersCharArrayNoInline.lit(Hw.this.trueValue))).|[Any](FastParsersCharArrayNoInline.lit(Hw.this.falseValue)));
  def obj: fastparsers.parsers.Parser[Any] = FastParsersCharArrayNoInline.baseParsers[List[Any]](FastParsersCharArrayNoInline.elemParser('{').~>[List[Any]](FastParsersCharArrayNoInline.repsep[Any, Array[Char]](member, FastParsersCharArrayNoInline.lit(Hw.this.comma)))).<~[Array[Char]](FastParsersCharArrayNoInline.lit(Hw.this.closeBracket));
  def arr: fastparsers.parsers.Parser[Any] = FastParsersCharArrayNoInline.baseParsers[List[Any]](FastParsersCharArrayNoInline.elemParser('[').~>[List[Any]](FastParsersCharArrayNoInline.repsep[Any, Array[Char]](value, FastParsersCharArrayNoInline.lit(Hw.this.comma)))).<~[Array[Char]](FastParsersCharArrayNoInline.lit(Hw.this.closeSBracket));
  def member: fastparsers.parsers.Parser[Any] = FastParsersCharArrayNoInline.baseParsers[(InputWindow.InputWindow[Array[Char]], Any)](FastParsersCharArrayNoInline.baseParsers[InputWindow.InputWindow[Array[Char]]](FastParsersCharArrayNoInline.stringLit).~[Any](FastParsersCharArrayNoInline.baseParsers[Array[Char]](FastParsersCharArrayNoInline.lit(Hw.this.points)).~>[Any](value))).^^[Any](((x0$1: (InputWindow.InputWindow[Array[Char]], Any)) => x0$1 match {
    case (_1: InputWindow.InputWindow[Array[Char]], _2: Any)(InputWindow.InputWindow[Array[Char]], Any)((a @ _), (b @ _)) => scala.Tuple2.apply[String, Any](a.toString(), b)
  }));
  ()
}) at source-/home/kakadu/ppx_parsers/scala/hello1/src/main/scala/hw.scala,line-40,offset=1292
{
  class fresh$macro$102 extends FinalFastParserImpl {
    def <init>() = {
      super.<init>();
      ()
    };
    import scala.collection.mutable.ListBuffer;
    import scala.reflect.runtime.universe._;
    @new scala.annotation.compileTimeOnly("can\'t be used outside of FastParser") def member: fastparsers.parsers.Parser[Any] = $qmark$qmark$qmark;
    @new scala.annotation.compileTimeOnly("can\'t be used outside of FastParser") def arr: fastparsers.parsers.Parser[Any] = $qmark$qmark$qmark;
    @new scala.annotation.compileTimeOnly("can\'t be used outside of FastParser") def value: fastparsers.parsers.Parser[Any] = $qmark$qmark$qmark;
    @new scala.annotation.compileTimeOnly("can\'t be used outside of FastParser") def obj: fastparsers.parsers.Parser[Any] = $qmark$qmark$qmark;
    //       def member:Parser[Any] = stringLit ~ (lit(points) ~> value) ^^ {case (a, b) => (a.toString, b)}

    def member(input$macro$1: Array[Char], fresh$macro$6: Int = 0): ParseResult[Any, String] @fastparsers.framework.saveAST(FastParsersCharArrayNoInline.baseParsers[(InputWindow.InputWindow[Array[Char]], Any)](FastParsersCharArrayNoInline.baseParsers[InputWindow.InputWindow[Array[Char]]](FastParsersCharArrayNoInline.stringLit).$tilde[Any](FastParsersCharArrayNoInline.baseParsers[Array[Char]](FastParsersCharArrayNoInline.lit(Hw.this.points)).$tilde$greater[Any](FastParsersCharArrayNoInline.call[Any]("value")))).$up$up[Any](((x0$1: (InputWindow.InputWindow[Array[Char]], Any)) => x0$1 match {
      case (_1: InputWindow.InputWindow[Array[Char]], _2: Any)(InputWindow.InputWindow[Array[Char]], Any)((a @ _), (b @ _)) => scala.Tuple2.apply[String, Any](a.toString(), b)
    }))) = {
      var error = " ";
      {
        var inputpos$macro$2 = fresh$macro$6;
        val inputsize$macro$3 = input$macro$1.size;
        {
          val inputpositioned$macro$4 = new fastparsers.tools.ToPosition.IndexedCharSeqToPosition(input$macro$1);
          {
            var success$macro$5 = false;
            var fresh$macro$10: InputWindow.CharArrayStruct = null;
            var fresh$macro$15: Array[Char] = null;
            var fresh$macro$17: Any = null;
            var fresh$macro$18: scala.Tuple2[InputWindow.CharArrayStruct, Any] = null;
            var fresh$macro$19: Any = null;
            {
              val fresh$macro$7 = ((x0$1: (InputWindow.InputWindow[Array[Char]], Any)) => x0$1 match {
                case scala.Tuple2((a @ _), (b @ _)) => scala.Tuple2.apply[String, Any](a.toString(), b)
              });
              {
                {
                  val fresh$macro$9 = inputpos$macro$2;
                  {
                    while$2(){
                      if (inputpos$macro$2 < (inputsize$macro$3) && (input$macro$1(inputpos$macro$2) == (' ') || (input$macro$1(inputpos$macro$2) == ('\t')) || (input$macro$1(inputpos$macro$2) == ('\n')) || (input$macro$1(inputpos$macro$2) == ('\r'))))
                        {
                          inputpos$macro$2 = inputpos$macro$2.$plus(1);
                          while$2()
                        }
                      else
                        ()
                    };
                    val fresh$macro$8 = inputpos$macro$2;
                    if (inputpos$macro$2 < (inputsize$macro$3) && (input$macro$1(inputpos$macro$2) == ('\"')))
                      {
                        inputpos$macro$2 = inputpos$macro$2.$plus(1);
                        while$1(){
                          if (inputpos$macro$2 < (inputsize$macro$3) && (input$macro$1(inputpos$macro$2) != ('\"')))
                            {
                              {
                                if (input$macro$1(inputpos$macro$2) == ('\\'))
                                  inputpos$macro$2 = inputpos$macro$2.$plus(1)
                                else
                                  ();
                                inputpos$macro$2 = inputpos$macro$2.$plus(1)
                              };
                              while$1()
                            }
                          else
                            ()
                        };
                        if (inputpos$macro$2 < (inputsize$macro$3))
                          {
                            success$macro$5 = true;
                            inputpos$macro$2 = inputpos$macro$2.$plus(1);
                            fresh$macro$10 = new InputWindow.CharArrayStruct(input$macro$1, fresh$macro$8, inputpos$macro$2)
                          }
                        else
                          {
                            success$macro$5 = false;
                            ();
                            inputpos$macro$2 = fresh$macro$9
                          }
                      }
                    else
                      {
                        success$macro$5 = false;
                        ();
                        inputpos$macro$2 = fresh$macro$9
                      }
                  }
                };
                if (success$macro$5)
                  {
                    {
                      {
                        val fresh$macro$14 = inputpos$macro$2;
                        {
                          var fresh$macro$13 = 0;
                          val fresh$macro$12 = Hw.this.points.length;
                          while$4(){
                            if (inputpos$macro$2 < (inputsize$macro$3) && (input$macro$1(inputpos$macro$2) == (' ') || (input$macro$1(inputpos$macro$2) == ('\t')) || (input$macro$1(inputpos$macro$2) == ('\n')) || (input$macro$1(inputpos$macro$2) == ('\r'))))
                              {
                                inputpos$macro$2 = inputpos$macro$2.$plus(1);
                                while$4()
                              }
                            else
                              ()
                          };
                          while$3(){
                            if (inputpos$macro$2 < (inputsize$macro$3) && (fresh$macro$13 < (fresh$macro$12)) && (input$macro$1(inputpos$macro$2) == (Hw.this.points.charAt(fresh$macro$13))))
                              {
                                {
                                  fresh$macro$13 = fresh$macro$13.$plus(1);
                                  inputpos$macro$2 = inputpos$macro$2.$plus(1)
                                };
                                while$3()
                              }
                            else
                              ()
                          };
                          if (fresh$macro$13 == (fresh$macro$12))
                            {
                              success$macro$5 = true;
                              fresh$macro$15 = Hw.this.points
                            }
                          else
                            {
                              success$macro$5 = false;
                              ();
                              inputpos$macro$2 = fresh$macro$14
                            }
                        }
                      };
                      if (success$macro$5)
                        {
                          val fresh$macro$16 = value(input$macro$1, inputpos$macro$2);
                          success$macro$5 = fresh$macro$16.success;
                          if (success$macro$5)
                            {
                              inputpos$macro$2 = fresh$macro$16.inputPos;
                              fresh$macro$17 = fresh$macro$16.result
                            }
                          else
                            error = fresh$macro$16.error
                        }
                      else
                        ()
                    };
                    fresh$macro$18 = scala.Tuple2(fresh$macro$10, fresh$macro$17)
                  }
                else
                  ()
              };
              if (success$macro$5)
                fresh$macro$19 = fresh$macro$7.apply(fresh$macro$18)
              else
                ()
            };
            ParseResult(success$macro$5, error, if (success$macro$5)
              fresh$macro$19
            else
              null, inputpos$macro$2)
          }
        }
      }
    };
    def arr(input$macro$1: Array[Char], fresh$macro$20: Int = 0): ParseResult[Any, String] @fastparsers.framework.saveAST(FastParsersCharArrayNoInline.baseParsers[List[Any]](FastParsersCharArrayNoInline.elemParser('[').$tilde$greater[List[Any]](FastParsersCharArrayNoInline.repsep[Any, Array[Char]](FastParsersCharArrayNoInline.call[Any]("value"), FastParsersCharArrayNoInline.lit(Hw.this.comma)))) < $tilde[Array[Char]](FastParsersCharArrayNoInline.lit(Hw.this.closeSBracket))) = {
      var error = " ";
      {
        var inputpos$macro$2 = fresh$macro$20;
        val inputsize$macro$3 = input$macro$1.size;
        {
          val inputpositioned$macro$4 = new fastparsers.tools.ToPosition.IndexedCharSeqToPosition(input$macro$1);
          {
            var success$macro$5 = false;
            var fresh$macro$21: Char = ' ';
            var fresh$macro$24: List[Any] = null;
            var fresh$macro$30: Array[Char] = null;
            var fresh$macro$33: Any = null;
            var fresh$macro$38: Array[Char] = null;
            {
              {
                if (inputpos$macro$2 < (inputsize$macro$3) && (input$macro$1(inputpos$macro$2) == ('[')))
                  {
                    fresh$macro$21 = '[';
                    inputpos$macro$2 = inputpos$macro$2.$plus(1);
                    success$macro$5 = true
                  }
                else
                  {
                    success$macro$5 = false;
                    ()
                  };
                if (success$macro$5)
                  {
                    var fresh$macro$22 = true;
                    val fresh$macro$23 = new ListBuffer[Any]();
                    while$7(){
                      if (fresh$macro$22)
                        {
                          {
                            val fresh$macro$31 = inputpos$macro$2;
                            {
                              {
                                val fresh$macro$32 = value(input$macro$1, inputpos$macro$2);
                                success$macro$5 = fresh$macro$32.success;
                                if (success$macro$5)
                                  {
                                    inputpos$macro$2 = fresh$macro$32.inputPos;
                                    fresh$macro$33 = fresh$macro$32.result
                                  }
                                else
                                  error = fresh$macro$32.error
                              };
                              if (success$macro$5)
                                {
                                  fresh$macro$23 += (fresh$macro$33);
                                  {
                                    val fresh$macro$25 = inputpos$macro$2;
                                    {
                                      {
                                        val fresh$macro$29 = inputpos$macro$2;
                                        {
                                          var fresh$macro$28 = 0;
                                          val fresh$macro$27 = Hw.this.comma.length;
                                          while$6(){
                                            if (inputpos$macro$2 < (inputsize$macro$3) && (input$macro$1(inputpos$macro$2) == (' ') || (input$macro$1(inputpos$macro$2) == ('\t')) || (input$macro$1(inputpos$macro$2) == ('\n')) || (input$macro$1(inputpos$macro$2) == ('\r'))))
                                              {
                                                inputpos$macro$2 = inputpos$macro$2.$plus(1);
                                                while$6()
                                              }
                                            else
                                              ()
                                          };
                                          while$5(){
                                            if (inputpos$macro$2 < (inputsize$macro$3) && (fresh$macro$28 < (fresh$macro$27)) && (input$macro$1(inputpos$macro$2) == (Hw.this.comma.charAt(fresh$macro$28))))
                                              {
                                                {
                                                  fresh$macro$28 = fresh$macro$28.$plus(1);
                                                  inputpos$macro$2 = inputpos$macro$2.$plus(1)
                                                };
                                                while$5()
                                              }
                                            else
                                              ()
                                          };
                                          if (fresh$macro$28 == (fresh$macro$27))
                                            {
                                              success$macro$5 = true;
                                              fresh$macro$30 = Hw.this.comma
                                            }
                                          else
                                            {
                                              success$macro$5 = false;
                                              ();
                                              inputpos$macro$2 = fresh$macro$29
                                            }
                                        }
                                      };
                                      if (success$macro$5.unary_$bang)
                                        {
                                          fresh$macro$22 = false;
                                          inputpos$macro$2 = fresh$macro$25
                                        }
                                      else
                                        ()
                                    }
                                  }
                                }
                              else
                                {
                                  fresh$macro$22 = false;
                                  inputpos$macro$2 = fresh$macro$31
                                }
                            }
                          };
                          while$7()
                        }
                      else
                        ()
                    };
                    {
                      fresh$macro$24 = fresh$macro$23.toList;
                      success$macro$5 = true
                    }
                  }
                else
                  ()
              };
              if (success$macro$5)
                {
                  val fresh$macro$37 = inputpos$macro$2;
                  {
                    var fresh$macro$36 = 0;
                    val fresh$macro$35 = Hw.this.closeSBracket.length;
                    while$9(){
                      if (inputpos$macro$2 < (inputsize$macro$3) && (input$macro$1(inputpos$macro$2) == (' ') || (input$macro$1(inputpos$macro$2) == ('\t')) || (input$macro$1(inputpos$macro$2) == ('\n')) || (input$macro$1(inputpos$macro$2) == ('\r'))))
                        {
                          inputpos$macro$2 = inputpos$macro$2.$plus(1);
                          while$9()
                        }
                      else
                        ()
                    };
                    while$8(){
                      if (inputpos$macro$2 < (inputsize$macro$3) && (fresh$macro$36 < (fresh$macro$35)) && (input$macro$1(inputpos$macro$2) == (Hw.this.closeSBracket.charAt(fresh$macro$36))))
                        {
                          {
                            fresh$macro$36 = fresh$macro$36.$plus(1);
                            inputpos$macro$2 = inputpos$macro$2.$plus(1)
                          };
                          while$8()
                        }
                      else
                        ()
                    };
                    if (fresh$macro$36 == (fresh$macro$35))
                      {
                        success$macro$5 = true;
                        fresh$macro$38 = Hw.this.closeSBracket
                      }
                    else
                      {
                        success$macro$5 = false;
                        ();
                        inputpos$macro$2 = fresh$macro$37
                      }
                  }
                }
              else
                ()
            };
            ParseResult(success$macro$5, error, if (success$macro$5)
              fresh$macro$24
            else
              null, inputpos$macro$2)
          }
        }
      }
    };
    //       def value:Parser[Any] = whitespaces ~> (obj | arr | stringLit ^^ (_.toString) | decimalNumber | nullValue | trueValue | falseValue)

    def value(input$macro$1: Array[Char], fresh$macro$39: Int = 0): ParseResult[Any, String] @fastparsers.framework.saveAST(FastParsersCharArrayNoInline.baseParsers[InputWindow.InputWindow[Array[Char]]](FastParsersCharArrayNoInline.whitespaces).$tilde$greater[Any](FastParsersCharArrayNoInline.baseParsers[Any](FastParsersCharArrayNoInline.baseParsers[Any](FastParsersCharArrayNoInline.baseParsers[Any](FastParsersCharArrayNoInline.baseParsers[Any](FastParsersCharArrayNoInline.baseParsers[Any](FastParsersCharArrayNoInline.baseParsers[Any](FastParsersCharArrayNoInline.call[Any]("obj")).$bar[Any](FastParsersCharArrayNoInline.call[Any]("arr"))).$bar[Any](FastParsersCharArrayNoInline.baseParsers[InputWindow.InputWindow[Array[Char]]](FastParsersCharArrayNoInline.stringLit).$up$up[String](((x$1: InputWindow.InputWindow[Array[Char]]) => x$1.toString())))).$bar[Any](FastParsersCharArrayNoInline.decimalNumber)).$bar[Any](FastParsersCharArrayNoInline.lit(Hw.this.nullValue))).$bar[Any](FastParsersCharArrayNoInline.lit(Hw.this.trueValue))).$bar[Any](FastParsersCharArrayNoInline.lit(Hw.this.falseValue)))) = {
      var error = " ";
      {
        var inputpos$macro$2 = fresh$macro$39;
        val inputsize$macro$3 = input$macro$1.size;
        {
          val inputpositioned$macro$4 = new fastparsers.tools.ToPosition.IndexedCharSeqToPosition(input$macro$1);
          {
            var success$macro$5 = false;
            var fresh$macro$42: InputWindow.CharArrayStruct = null;
            var fresh$macro$43: Any = null;
            var fresh$macro$45: Any = null;
            var fresh$macro$47: Any = null;
            var fresh$macro$49: Any = null;
            var fresh$macro$51: Any = null;
            var fresh$macro$53: Any = null;
            var fresh$macro$56: Any = null;
            var fresh$macro$58: Any = null;
            var fresh$macro$62: InputWindow.CharArrayStruct = null;
            var fresh$macro$63: String = "";
            var fresh$macro$66: InputWindow.CharArrayStruct = null;
            var fresh$macro$72: Array[Char] = null;
            var fresh$macro$77: Array[Char] = null;
            var fresh$macro$82: Array[Char] = null;
            {
              {
                val fresh$macro$40 = inputpos$macro$2;
                while$10(){
                  if ((inputpos$macro$2 < inputsize$macro$3) && (input$macro$1(inputpos$macro$2) == (' ') ||
                      (input$macro$1(inputpos$macro$2) == ('\t')) || (input$macro$1(inputpos$macro$2) == ('\n')) ||
                      (input$macro$1(inputpos$macro$2) == ('\r'))) )
                    {
                      inputpos$macro$2 = inputpos$macro$2.$plus(1);
                      while$10()
                    }
                  else
                    ()
                };
                fresh$macro$42 = new InputWindow.CharArrayStruct(input$macro$1, fresh$macro$40, inputpos$macro$2);
                success$macro$5 = true
              };
              if (success$macro$5)
                {
                  val fresh$macro$44 = inputpos$macro$2;
                  {
                    {
                      val fresh$macro$46 = inputpos$macro$2;
                      {
                        {
                          val fresh$macro$48 = inputpos$macro$2;
                          {
                            {
                              val fresh$macro$50 = inputpos$macro$2;
                              {
                                {
                                  val fresh$macro$52 = inputpos$macro$2;
                                  {
                                    {
                                      val fresh$macro$54 = inputpos$macro$2;
                                      {
                                        {
                                          val fresh$macro$55 = obj(input$macro$1, inputpos$macro$2);
                                          success$macro$5 = fresh$macro$55.success;
                                          if (success$macro$5)
                                            {
                                              inputpos$macro$2 = fresh$macro$55.inputPos;
                                              fresh$macro$56 = fresh$macro$55.result
                                            }
                                          else
                                            error = fresh$macro$55.error
                                        };
                                        if (success$macro$5.unary_$bang)
                                          {
                                            inputpos$macro$2 = fresh$macro$54;
                                            {
                                              val fresh$macro$57 = arr(input$macro$1, inputpos$macro$2);
                                              success$macro$5 = fresh$macro$57.success;
                                              if (success$macro$5)
                                                {
                                                  inputpos$macro$2 = fresh$macro$57.inputPos;
                                                  fresh$macro$58 = fresh$macro$57.result
                                                }
                                              else
                                                error = fresh$macro$57.error
                                            };
                                            if (success$macro$5)
                                              fresh$macro$53 = fresh$macro$58
                                            else
                                              ()
                                          }
                                        else
                                          fresh$macro$53 = fresh$macro$56
                                      }
                                    };
                                    if (success$macro$5.unary_$bang)
                                      { // neither obj nor arr
                                        inputpos$macro$2 = fresh$macro$52;
                                        {
                                          val fresh$macro$59 = ((x$1: InputWindow.InputWindow[Array[Char]]) => x$1.toString());
                                          {
                                            val fresh$macro$61 = inputpos$macro$2;
                                            {
                                              while$12(){
                                                if ((inputpos$macro$2 < (inputsize$macro$3) && (input$macro$1(inputpos$macro$2) == (' ') ||
                                                    (input$macro$1(inputpos$macro$2) == ('\t')) || (input$macro$1(inputpos$macro$2) == ('\n')) ||
                                                    (input$macro$1(inputpos$macro$2) == '\r')) )
                                                  {
                                                    inputpos$macro$2 = inputpos$macro$2.$plus(1);
                                                    while$12()
                                                  }
                                                else
                                                  ()
                                              };
                                              val fresh$macro$60 = inputpos$macro$2;
                                              if (inputpos$macro$2 < (inputsize$macro$3) && (input$macro$1(inputpos$macro$2) == ('\"')))
                                                {
                                                  inputpos$macro$2 = inputpos$macro$2.$plus(1);
                                                  while$11(){
                                                    if (inputpos$macro$2 < (inputsize$macro$3) && (input$macro$1(inputpos$macro$2) != ('\"')))
                                                      {
                                                        {
                                                          if (input$macro$1(inputpos$macro$2) == ('\\'))
                                                            inputpos$macro$2 = inputpos$macro$2.$plus(1)
                                                          else
                                                            ();
                                                          inputpos$macro$2 = inputpos$macro$2.$plus(1)
                                                        };
                                                        while$11()
                                                      }
                                                    else
                                                      ()
                                                  };
                                                  if (inputpos$macro$2 < (inputsize$macro$3))
                                                    {
                                                      success$macro$5 = true;
                                                      inputpos$macro$2 = inputpos$macro$2.$plus(1);
                                                      fresh$macro$62 = new InputWindow.CharArrayStruct(input$macro$1, fresh$macro$60, inputpos$macro$2)
                                                    }
                                                  else
                                                    {
                                                      success$macro$5 = false;
                                                      ();
                                                      inputpos$macro$2 = fresh$macro$61
                                                    }
                                                }
                                              else
                                                {
                                                  success$macro$5 = false;
                                                  ();
                                                  inputpos$macro$2 = fresh$macro$61
                                                }
                                            }
                                          };
                                          // we have created a new macro for --> and apply it there
                                          if (success$macro$5)
                                            fresh$macro$63 = fresh$macro$59.apply(fresh$macro$62)
                                          else
                                            ()
                                        };
                                        if (success$macro$5)
                                          fresh$macro$51 = fresh$macro$63
                                        else
                                          ()
                                      }
                                    else
                                      fresh$macro$51 = fresh$macro$53
                                  }
                                };
                                if (success$macro$5.unary_$bang)
                                  { // neither, obj, arr or stringLit
                                    inputpos$macro$2 = fresh$macro$50;
                                    {
                                      val fresh$macro$67 = inputpos$macro$2;
                                      {
                                        while$16(){
                                          if (inputpos$macro$2 < (inputsize$macro$3) && (input$macro$1(inputpos$macro$2) == (' ') || (input$macro$1(inputpos$macro$2) == ('\t')) || (input$macro$1(inputpos$macro$2) == ('\n')) || (input$macro$1(inputpos$macro$2) == ('\r'))))
                                            {
                                              inputpos$macro$2 = inputpos$macro$2.$plus(1);
                                              while$16()
                                            }
                                          else
                                            ()
                                        };
                                        var fresh$macro$64 = false;
                                        val fresh$macro$65 = inputpos$macro$2;
                                        success$macro$5 = false;
                                        if (inputpos$macro$2 < (inputsize$macro$3) && (input$macro$1(inputpos$macro$2) == ('-')))
                                          inputpos$macro$2 = inputpos$macro$2.$plus(1)
                                        else
                                          ();
                                        if ((inputpos$macro$2 < inputsize$macro$3) && (input$macro$1(inputpos$macro$2) >= ('0')) &&
                                            (input$macro$1(inputpos$macro$2) <= ('9')))
                                          {
                                            inputpos$macro$2 = inputpos$macro$2.$plus(1);
                                            while$14(){
                                              if ((inputpos$macro$2 < inputsize$macro$3) && (input$macro$1(inputpos$macro$2) >= ('0')) &&
                                                  (input$macro$1(inputpos$macro$2) <= ('9')))
                                                {
                                                  inputpos$macro$2 = inputpos$macro$2.$plus(1);
                                                  while$14()
                                                }
                                              else
                                                ()
                                            };
                                            if (inputpos$macro$2 < (inputsize$macro$3) && (input$macro$1(inputpos$macro$2) == ('.')))
                                              {
                                                inputpos$macro$2 = inputpos$macro$2.$plus(1);
                                                while$13(){
                                                  if ((inputpos$macro$2 < inputsize$macro$3) && (input$macro$1(inputpos$macro$2) >= ('0')) &&
                                                      (input$macro$1(inputpos$macro$2) <= ('9')))
                                                    {
                                                      inputpos$macro$2 = inputpos$macro$2.$plus(1);
                                                      while$13()
                                                    }
                                                  else
                                                    ()
                                                }
                                              }
                                            else
                                              ();
                                            success$macro$5 = true;
                                            fresh$macro$66 = new InputWindow.CharArrayStruct(input$macro$1, fresh$macro$65, inputpos$macro$2)
                                          }
                                        else
                                          if (inputpos$macro$2 < (inputsize$macro$3) && (input$macro$1(inputpos$macro$2) == ('.')))
                                            {
                                              inputpos$macro$2 = inputpos$macro$2.$plus(1);
                                              if (inputpos$macro$2 < (inputsize$macro$3) && (input$macro$1(inputpos$macro$2) >= ('0')) && (input$macro$1(inputpos$macro$2) <= ('9')))
                                                {
                                                  inputpos$macro$2 = inputpos$macro$2.$plus(1);
                                                  while$15(){
                                                    if (inputpos$macro$2 < (inputsize$macro$3) && (input$macro$1(inputpos$macro$2) >= ('0')) && (input$macro$1(inputpos$macro$2) <= ('9')))
                                                      {
                                                        inputpos$macro$2 = inputpos$macro$2.$plus(1);
                                                        while$15()
                                                      }
                                                    else
                                                      ()
                                                  };
                                                  success$macro$5 = true;
                                                  fresh$macro$66 = new InputWindow.CharArrayStruct(input$macro$1, fresh$macro$65, inputpos$macro$2)
                                                }
                                              else
                                                ()
                                            }
                                          else
                                            ()
                                      }
                                    };
                                    if (success$macro$5)
                                      fresh$macro$49 = fresh$macro$66
                                    else
                                      ()
                                  }
                                else
                                  fresh$macro$49 = fresh$macro$51
                              }
                            };
                            if (success$macro$5.unary_$bang)
                              {
                                inputpos$macro$2 = fresh$macro$48;
                                {
                                  val fresh$macro$71 = inputpos$macro$2;
                                  {
                                    var fresh$macro$70 = 0;
                                    val fresh$macro$69 = Hw.this.nullValue.length;
                                    while$18(){
                                      if (inputpos$macro$2 < (inputsize$macro$3) && (input$macro$1(inputpos$macro$2) == (' ') || (input$macro$1(inputpos$macro$2) == ('\t')) || (input$macro$1(inputpos$macro$2) == ('\n')) || (input$macro$1(inputpos$macro$2) == ('\r'))))
                                        {
                                          inputpos$macro$2 = inputpos$macro$2.$plus(1);
                                          while$18()
                                        }
                                      else
                                        ()
                                    };
                                    while$17(){
                                      if (inputpos$macro$2 < (inputsize$macro$3) && (fresh$macro$70 < (fresh$macro$69)) && (input$macro$1(inputpos$macro$2) == (Hw.this.nullValue.charAt(fresh$macro$70))))
                                        {
                                          {
                                            fresh$macro$70 = fresh$macro$70.$plus(1);
                                            inputpos$macro$2 = inputpos$macro$2.$plus(1)
                                          };
                                          while$17()
                                        }
                                      else
                                        ()
                                    };
                                    if (fresh$macro$70 == (fresh$macro$69))
                                      {
                                        success$macro$5 = true;
                                        fresh$macro$72 = Hw.this.nullValue
                                      }
                                    else
                                      {
                                        success$macro$5 = false;
                                        ();
                                        inputpos$macro$2 = fresh$macro$71
                                      }
                                  }
                                };
                                if (success$macro$5)
                                  fresh$macro$47 = fresh$macro$72
                                else
                                  ()
                              }
                            else
                              fresh$macro$47 = fresh$macro$49
                          }
                        };
                        if (success$macro$5.unary_$bang)
                          {
                            inputpos$macro$2 = fresh$macro$46;
                            {
                              val fresh$macro$76 = inputpos$macro$2;
                              {
                                var fresh$macro$75 = 0;
                                val fresh$macro$74 = Hw.this.trueValue.length;
                                while$20(){
                                  if (inputpos$macro$2 < (inputsize$macro$3) && (input$macro$1(inputpos$macro$2) == (' ') || (input$macro$1(inputpos$macro$2) == ('\t')) || (input$macro$1(inputpos$macro$2) == ('\n')) || (input$macro$1(inputpos$macro$2) == ('\r'))))
                                    {
                                      inputpos$macro$2 = inputpos$macro$2.$plus(1);
                                      while$20()
                                    }
                                  else
                                    ()
                                };
                                while$19(){
                                  if (inputpos$macro$2 < (inputsize$macro$3) && (fresh$macro$75 < (fresh$macro$74)) && (input$macro$1(inputpos$macro$2) == (Hw.this.trueValue.charAt(fresh$macro$75))))
                                    {
                                      {
                                        fresh$macro$75 = fresh$macro$75.$plus(1);
                                        inputpos$macro$2 = inputpos$macro$2.$plus(1)
                                      };
                                      while$19()
                                    }
                                  else
                                    ()
                                };
                                if (fresh$macro$75 == (fresh$macro$74))
                                  {
                                    success$macro$5 = true;
                                    fresh$macro$77 = Hw.this.trueValue
                                  }
                                else
                                  {
                                    success$macro$5 = false;
                                    ();
                                    inputpos$macro$2 = fresh$macro$76
                                  }
                              }
                            };
                            if (success$macro$5)
                              fresh$macro$45 = fresh$macro$77
                            else
                              ()
                          }
                        else
                          fresh$macro$45 = fresh$macro$47
                      }
                    };
                    if (success$macro$5.unary_$bang)
                      {
                        inputpos$macro$2 = fresh$macro$44;
                        {
                          val fresh$macro$81 = inputpos$macro$2;
                          {
                            var fresh$macro$80 = 0;
                            val fresh$macro$79 = Hw.this.falseValue.length;
                            while$22(){
                              if (inputpos$macro$2 < (inputsize$macro$3) && (input$macro$1(inputpos$macro$2) == (' ') || (input$macro$1(inputpos$macro$2) == ('\t')) || (input$macro$1(inputpos$macro$2) == ('\n')) || (input$macro$1(inputpos$macro$2) == ('\r'))))
                                {
                                  inputpos$macro$2 = inputpos$macro$2.$plus(1);
                                  while$22()
                                }
                              else
                                ()
                            };
                            while$21(){
                              if (inputpos$macro$2 < (inputsize$macro$3) && (fresh$macro$80 < (fresh$macro$79)) && (input$macro$1(inputpos$macro$2) == (Hw.this.falseValue.charAt(fresh$macro$80))))
                                {
                                  {
                                    fresh$macro$80 = fresh$macro$80.$plus(1);
                                    inputpos$macro$2 = inputpos$macro$2.$plus(1)
                                  };
                                  while$21()
                                }
                              else
                                ()
                            };
                            if (fresh$macro$80 == (fresh$macro$79))
                              {
                                success$macro$5 = true;
                                fresh$macro$82 = Hw.this.falseValue
                              }
                            else
                              {
                                success$macro$5 = false;
                                ();
                                inputpos$macro$2 = fresh$macro$81
                              }
                          }
                        };
                        if (success$macro$5)
                          fresh$macro$43 = fresh$macro$82
                        else
                          ()
                      }
                    else
                      fresh$macro$43 = fresh$macro$45
                  }
                }
              else
                ()
            };
            ParseResult(success$macro$5, error, if (success$macro$5)
              fresh$macro$43
            else
              null, inputpos$macro$2)
          }
        }
      }
    };
    def obj(input$macro$1: Array[Char], fresh$macro$83: Int = 0): ParseResult[Any, String] @fastparsers.framework.saveAST(FastParsersCharArrayNoInline.baseParsers[List[Any]](FastParsersCharArrayNoInline.elemParser('{').$tilde$greater[List[Any]](FastParsersCharArrayNoInline.repsep[Any, Array[Char]](FastParsersCharArrayNoInline.call[Any]("member"), FastParsersCharArrayNoInline.lit(Hw.this.comma)))) < $tilde[Array[Char]](FastParsersCharArrayNoInline.lit(Hw.this.closeBracket))) = {
      var error = " ";
      {
        var inputpos$macro$2 = fresh$macro$83;
        val inputsize$macro$3 = input$macro$1.size;
        {
          val inputpositioned$macro$4 = new fastparsers.tools.ToPosition.IndexedCharSeqToPosition(input$macro$1);
          {
            var success$macro$5 = false;
            var fresh$macro$84: Char = ' ';
            var fresh$macro$87: List[Any] = null;
            var fresh$macro$93: Array[Char] = null;
            var fresh$macro$96: Any = null;
            var fresh$macro$101: Array[Char] = null;
            {
              {
                if (inputpos$macro$2 < (inputsize$macro$3) && (input$macro$1(inputpos$macro$2) == ('{')))
                  {
                    fresh$macro$84 = '{';
                    inputpos$macro$2 = inputpos$macro$2.$plus(1);
                    success$macro$5 = true
                  }
                else
                  {
                    success$macro$5 = false;
                    ()
                  };
                if (success$macro$5)
                  {
                    var fresh$macro$85 = true;
                    val fresh$macro$86 = new ListBuffer[Any]();
                    while$25(){
                      if (fresh$macro$85)
                        {
                          {
                            val fresh$macro$94 = inputpos$macro$2;
                            {
                              {
                                val fresh$macro$95 = member(input$macro$1, inputpos$macro$2);
                                success$macro$5 = fresh$macro$95.success;
                                if (success$macro$5)
                                  {
                                    inputpos$macro$2 = fresh$macro$95.inputPos;
                                    fresh$macro$96 = fresh$macro$95.result
                                  }
                                else
                                  error = fresh$macro$95.error
                              };
                              if (success$macro$5)
                                {
                                  fresh$macro$86 += (fresh$macro$96);
                                  {
                                    val fresh$macro$88 = inputpos$macro$2;
                                    {
                                      {
                                        val fresh$macro$92 = inputpos$macro$2;
                                        {
                                          var fresh$macro$91 = 0;
                                          val fresh$macro$90 = Hw.this.comma.length;
                                          while$24(){
                                            if (inputpos$macro$2 < (inputsize$macro$3) && (input$macro$1(inputpos$macro$2) == (' ') || (input$macro$1(inputpos$macro$2) == ('\t')) || (input$macro$1(inputpos$macro$2) == ('\n')) || (input$macro$1(inputpos$macro$2) == ('\r'))))
                                              {
                                                inputpos$macro$2 = inputpos$macro$2.$plus(1);
                                                while$24()
                                              }
                                            else
                                              ()
                                          };
                                          while$23(){
                                            if (inputpos$macro$2 < (inputsize$macro$3) && (fresh$macro$91 < (fresh$macro$90)) && (input$macro$1(inputpos$macro$2) == (Hw.this.comma.charAt(fresh$macro$91))))
                                              {
                                                {
                                                  fresh$macro$91 = fresh$macro$91.$plus(1);
                                                  inputpos$macro$2 = inputpos$macro$2.$plus(1)
                                                };
                                                while$23()
                                              }
                                            else
                                              ()
                                          };
                                          if (fresh$macro$91 == (fresh$macro$90))
                                            {
                                              success$macro$5 = true;
                                              fresh$macro$93 = Hw.this.comma
                                            }
                                          else
                                            {
                                              success$macro$5 = false;
                                              ();
                                              inputpos$macro$2 = fresh$macro$92
                                            }
                                        }
                                      };
                                      if (success$macro$5.unary_$bang)
                                        {
                                          fresh$macro$85 = false;
                                          inputpos$macro$2 = fresh$macro$88
                                        }
                                      else
                                        ()
                                    }
                                  }
                                }
                              else
                                {
                                  fresh$macro$85 = false;
                                  inputpos$macro$2 = fresh$macro$94
                                }
                            }
                          };
                          while$25()
                        }
                      else
                        ()
                    };
                    {
                      fresh$macro$87 = fresh$macro$86.toList;
                      success$macro$5 = true
                    }
                  }
                else
                  ()
              };
              if (success$macro$5)
                {
                  val fresh$macro$100 = inputpos$macro$2;
                  {
                    var fresh$macro$99 = 0;
                    val fresh$macro$98 = Hw.this.closeBracket.length;
                    while$27(){
                      if (inputpos$macro$2 < (inputsize$macro$3) && (input$macro$1(inputpos$macro$2) == (' ') || (input$macro$1(inputpos$macro$2) == ('\t')) || (input$macro$1(inputpos$macro$2) == ('\n')) || (input$macro$1(inputpos$macro$2) == ('\r'))))
                        {
                          inputpos$macro$2 = inputpos$macro$2.$plus(1);
                          while$27()
                        }
                      else
                        ()
                    };
                    while$26(){
                      if (inputpos$macro$2 < (inputsize$macro$3) && (fresh$macro$99 < (fresh$macro$98)) && (input$macro$1(inputpos$macro$2) == (Hw.this.closeBracket.charAt(fresh$macro$99))))
                        {
                          {
                            fresh$macro$99 = fresh$macro$99.$plus(1);
                            inputpos$macro$2 = inputpos$macro$2.$plus(1)
                          };
                          while$26()
                        }
                      else
                        ()
                    };
                    if (fresh$macro$99 == (fresh$macro$98))
                      {
                        success$macro$5 = true;
                        fresh$macro$101 = Hw.this.closeBracket
                      }
                    else
                      {
                        success$macro$5 = false;
                        ();
                        inputpos$macro$2 = fresh$macro$100
                      }
                  }
                }
              else
                ()
            };
            ParseResult(success$macro$5, error, if (success$macro$5)
              fresh$macro$87
            else
              null, inputpos$macro$2)
          }
        }
      }
    }
  };
  val fresh$macro$103 = 0;
  new fresh$macro$102()
}
