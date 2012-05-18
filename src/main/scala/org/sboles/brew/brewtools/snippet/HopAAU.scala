package org.sboles.brew.brewtools {
  package snippet {

    import _root_.scala.xml.{NodeSeq, Text}
    import _root_.net.liftweb.util._
    import _root_.net.liftweb.common._
    import _root_.net.liftweb.http._
    import _root_.net.liftweb.http.js._

    import _root_.java.text.DecimalFormat

    import org.sboles.brew.brewtools.lib._
    import org.sboles.brew.brewlib.Hops

    import Helpers._

    /**
     * Companion object
     */
    object HopAAU {
      private val logger = Logger(classOf[HopAAU])

      val decimalFormat = new DecimalFormat("#.##")
    }

    /**
     * Provides a calculator for converting hops measured in AAU to an
     * ounce measurement at a specific percentage AA.
     */
    class HopAAU extends StatefulSnippet {
      import HopAAU._

      private val sId = "HopAAU"

      private var aau = ""
      private var hopAA = ""
      private var hopOz = ""

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
             "aau" -> SHtml.ajaxText(aau,
                                     onChangeAAU(_),
                                     "id" -> myId("aau")),
             "hop_aa" -> SHtml.ajaxText(hopAA,
                                        onChangeHopAA(_),
                                        "id" -> myId("hopAA")),
             "ounces" -> SHtml.span(<span></span>,
                                    JsCmds.Noop,
                                    "id" -> myId("ounces")),
             "submit" -> SHtml.ajaxButton("Calculate",
                                          onClickSubmit _,
                                          "class" -> "submitButton"),
             "summary" -> SHtml.span(<span></span>,
                                     JsCmds.Noop,
                                     "id" -> myId("summary")))
      }

      def onChangeAAU(v: String): JsCmd = {
        aau = v

        onChange
      }

      def onChangeHopAA(v: String): JsCmd = {
        hopAA = v

        onChange
      }

      def onChange: JsCmd = {
        val dAAU: Double = try {
          aau.toDouble
        } catch {
          case _ => 0.0
        }

        val dAA: Double = try {
          hopAA.toDouble
        } catch {
          case _ => 0.0
        }

        val dOZ = Hops.aauToOuncesAA(aau, hopAA)

        val cmds = if ( dOZ > 0 ) {
          val ozDesc = if ( dOZ == 1 ) "ounce" else "ounces"
          
          val summary =
            <span>
              <strong>{decimalFormat.format(dAAU)}</strong> AAU
                is equivalent to
              <strong>{decimalFormat.format(dOZ)}</strong> {ozDesc}
                of hops at
              <strong>{decimalFormat.format(dAA)}</strong>% AA
            </span>

          val result = <span class="result">{decimalFormat.format(dOZ)}</span>

          List(
            JsCmds.Replace(myId("ounces"),
                           SHtml.span(result,
                                      JsCmds.Noop,
                                      "id" -> myId("ounces"))),

            JsCmds.Replace(myId("summary"),
                           SHtml.span(summary,
                                      JsCmds.Noop,
                                      "id" -> myId("summary")))
          )
        } else {
          List(
            JsCmds.Replace(myId("ounces"),
                           SHtml.span(<span></span>,
                                      JsCmds.Noop,
                                      "id" -> myId("ounces"))),

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
