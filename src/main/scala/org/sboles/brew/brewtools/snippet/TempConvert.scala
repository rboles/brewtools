package org.sboles.brew.brewtools {
  package snippet {

    import _root_.scala.xml.{NodeSeq, Text}
    import _root_.net.liftweb.util._
    import _root_.net.liftweb.common._
    import _root_.net.liftweb.http._
    import _root_.net.liftweb.http.js._

    import _root_.java.text.DecimalFormat

    import org.sboles.brew.brewtools.lib._
    import org.sboles.brew.brewlib.Temperature

    import Helpers._

    /**
     * Companion object
     */
    object TempConvert {
      private val decimalFormat = new DecimalFormat("##.##")

      private val sId = "TempConvert"

      def myId(suffix: String): String = sId+"_"+suffix

      def summarize(dC: String, dF: String): NodeSeq = {
        <div>
          <strong>{dF}</strong>
          <strong>&deg;</strong> Fahrenheit
          is equivalent to
          <strong>{dC}</strong>
          <strong>&deg;</strong> Celsius
        </div>
      }

      def unset: JsCmd = {
        JsCmds.Replace(myId("summary"),
                       SHtml.span(<span></span>,
                                  JsCmds.Noop,
                                  "id" -> myId("summary")))
      }
    }

    /**
     * Provides a calculator for temperature measure conversions
     */
    class TempConvert extends StatefulSnippet {
      import TempConvert._

      private val logger = Logger(classOf[TempConvert])

      private var dF = "60"
      private var dC = Temperature.fahrenheitToCelsius(59).toInt.toString

      private var submitted = false

      def dispatch: DispatchIt = {
        case "render" => render _
      }

      def render(xhtml: NodeSeq): NodeSeq = {

        def onSubmit(): Unit = {
          submitted = true
        }

        bind("e", xhtml,
             "fahrenheit" -> SHtml.ajaxText(dF,
                                            onChangeFahrenheit(_),
                                            "id" -> myId("fahrenheit"),
                                            "class" -> "gravityInput"),

             "celsius" -> SHtml.ajaxText(dC,
                                         onChangeCelsius(_),
                                         "id" -> myId("celsius"),
                                         "class" -> "gravityInput"),

             "submit" -> SHtml.ajaxButton("Calculate",
                                          onClickSubmit _,
                                          "class" -> "submitButton"),

             "summary" -> SHtml.span(<span></span>,
                                     JsCmds.Noop,
                                     "id" -> myId("summary"))
             )
      }

      def onChangeCelsius(v: String): JsCmd = {
        dC = v
        if ( dC.length > 0 ) {
          val c = try {
            dC.toDouble
          } catch {
            case _ => 0.0
          }
          dF = Temperature.celsiusToFahrenheit(c).toInt.toString
          val summary = summarize(dC, dF)
          val cmds = List(
            JsCmds.Replace(myId("fahrenheit"),
                           SHtml.ajaxText(dF,
                                          onChangeFahrenheit(_),
                                          "id" -> myId("fahrenheit"),
                                          "class" -> "gravityInput")),

            JsCmds.Replace(myId("summary"),
                           SHtml.span(summary,
                                      JsCmds.Noop,
                                      "id" -> myId("summary"))))

          JsCmds.seqJsToJs(cmds)
        } else {
          unset
        }
      }

      def onChangeFahrenheit(v: String): JsCmd = {
        dF = v
        if ( dF.length > 0 ) {
          val f = try {
            dF.toDouble
          } catch {
            case _ => 0.0
          }
          dC = Temperature.fahrenheitToCelsius(f).toInt.toString
          val summary = summarize(dC, dF)
          val cmds = List(
            JsCmds.Replace(myId("celsius"),
                           SHtml.ajaxText(dC,
                                          onChangeCelsius(_),
                                          "id" -> myId("celsius"),
                                          "class" -> "gravityInput")),

            JsCmds.Replace(myId("summary"),
                           SHtml.span(summary,
                                      JsCmds.Noop,
                                      "id" -> myId("summary"))))

          JsCmds.seqJsToJs(cmds)
        } else {
          unset
        }
      }

      def onClickSubmit: JsCmd = {
        JsCmds.Noop
      }
    }
  }
}
