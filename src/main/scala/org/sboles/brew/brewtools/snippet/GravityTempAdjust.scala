package org.sboles.brew.brewtools {
  package snippet {

    import _root_.scala.xml.{NodeSeq, Text}
    import _root_.net.liftweb.util._
    import _root_.net.liftweb.common._
    import _root_.net.liftweb.http._
    import _root_.net.liftweb.http.js._

    import _root_.java.text.DecimalFormat

    import org.sboles.brew.brewtools.lib._

    import Helpers._

    /**
     * Companion object
     */
    object GravityTempAdjust {
      val decimalFormat = new DecimalFormat("#.###")
    }

    /**
     * Provides a calculator for adjusting a gravity measurement for
     * temperature
     */
    class GravityTempAdjust extends StatefulSnippet {
      import GravityTempAdjust._

      private val logger = Logger(classOf[GravityTempAdjust])

      private val sId = "GravityTempAdjust"

      private var temp = ""
      private var gravity = ""

      private var submitted = false

      def myId(suffix: String): String = sId+"_"+suffix

      def dispatch: DispatchIt = {
        case "render" => render _
      }

      def render(xhtml: NodeSeq): NodeSeq = {

        def onSubmit(): Unit = {
          submitted = true
        }

        bind("e", xhtml,
             "temp" -> SHtml.ajaxText(temp, onChangeTemp(_),
                                      "id" -> myId("temp")),
             "gravity" -> SHtml.ajaxText(gravity, onChangeGravity(_),
                                         "id" -> myId("gravity")),
             "result" -> SHtml.span(<span></span>,
                                    JsCmds.Noop,
                                    "id" -> myId("result")),
             "submit" -> SHtml.ajaxButton("Calculate",
                                          onClickSubmit _,
                                          "class" -> "submitButton"),
             "summary" -> SHtml.span(<span></span>,
                                     JsCmds.Noop,
                                     "id" -> myId("summary")))
      }

      def onChangeTemp(v: String): JsCmd = {
        temp = v
        onChange
      }

      def onChangeGravity(v: String): JsCmd = {
        gravity = v
        onChange
      }

      def onChange: JsCmd = {
        val dTemp: Double = try {
          temp.toDouble
        } catch {
          case _ => 0.0
        }

        val dGravity: Double = try {
          gravity.toDouble
        } catch {
          case _ => 0.0
        }

        val dAdjust: Double = {
          if ( dTemp > 0 && dGravity > 0 ) {
            dGravity + ((dTemp - 60) / 10 * 0.003)
          } else {
            0.0
          }
        }

        val cmds = if ( dAdjust > 0 ) {
          val summary =
            <span>
              <strong>{decimalFormat.format(dGravity)}</strong>
                at
              <strong>{decimalFormat.format(dTemp)}</strong> dF
                is equivalent to
              <strong>{decimalFormat.format(dAdjust)}</strong>
                at 60 dF
            </span>
          val result = <span class="result">{decimalFormat.format(dAdjust)}</span>
          List(
            JsCmds.Replace(myId("result"),
                           SHtml.span(result,
                                      JsCmds.Noop,
                                      "id" -> myId("result"))),

            JsCmds.Replace(myId("summary"),
                           SHtml.span(summary,
                                      JsCmds.Noop,
                                      "id" -> myId("summary")))
          )
        } else {
          List(
            JsCmds.Replace(myId("result"),
                           SHtml.span(<span></span>,
                                      JsCmds.Noop,
                                      "id" -> myId("result"))),

            JsCmds.Replace(myId("summary"),
                           SHtml.span(<span></span>,
                                      JsCmds.Noop,
                                      "id" -> myId("summary")))
          )
        }


        JsCmds.seqJsToJs(cmds)
      }

      def onClickSubmit: JsCmd = JsCmds.Noop
    }
  }
}
