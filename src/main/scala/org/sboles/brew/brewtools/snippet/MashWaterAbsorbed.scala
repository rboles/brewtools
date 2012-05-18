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
    object MashWaterAbsorbed {
      val decimalFormat = new DecimalFormat("#.###")

      def calculatorSummary(dGrain: Double, dWater: Double): NodeSeq = {
        <span></span>
      }
    }

    /**
     * Provides a calculator for water absorption by the mash
     */
    class MashWaterAbsorbed extends StatefulSnippet {
      import MashWaterAbsorbed._

      private val logger = Logger(classOf[MashWaterAbsorbed])

      private val sId = "MashWaterAbsorbed"

      private var grain = ""
      private var water = ""
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
             "grain" -> SHtml.ajaxText(grain,
                                       onChangeGrain(_),
                                       "id" -> myId("grain")),
             "water" -> SHtml.span(<span></span>,
                                   JsCmds.Noop,
                                   "id" -> myId("water")),
             "summary" -> SHtml.span(<span></span>,
                                     JsCmds.Noop,
                                     "id" -> myId("summary")))
      }

      def onChangeGrain(v: String): JsCmd = {
        grain = v
        onChange
      }

      def onChange: JsCmd = {
        val dGrain: Double = try {
          grain.toDouble
        } catch {
          case _ => 0.0
        }

        val dWater: Double = {
          if ( dGrain > 0 ) {
            0.0
          } else {
            0.0
          }
        }

        val cmds = if ( dWater > 0 ) {
          val summary = calculatorSummary(dGrain, dWater)
          List(
            JsCmds.Replace(myId("water"),
                           SHtml.span(<span>{decimalFormat.format(dWater)}</span>,
                                      JsCmds.Noop,
                                      "id" -> myId("water"))),

            JsCmds.Replace(myId("summary"),
                           SHtml.span(summary,
                                      JsCmds.Noop,
                                      "id" -> myId("summary")))
            )
        } else {
          List(
            JsCmds.Replace(myId("water"),
                           SHtml.span(<span></span>,
                                      JsCmds.Noop,
                                      "id" -> myId("water"))),

            JsCmds.Replace(myId("summary"),
                           SHtml.span(<span></span>,
                                      JsCmds.Noop,
                                      "id" -> myId("summary")))
            )
        }

        JsCmds.seqJsToJs(cmds)
      }

    }

  }
}
