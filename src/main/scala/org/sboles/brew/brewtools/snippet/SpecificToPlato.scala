package org.sboles.brew.brewtools {
  package snippet {

    import _root_.scala.xml.{NodeSeq, Text}
    import _root_.net.liftweb.util._
    import _root_.net.liftweb.common._
    import _root_.net.liftweb.http._
    import _root_.net.liftweb.http.js._

    import _root_.java.text.DecimalFormat

    import org.sboles.brew.brewtools.lib._

    import org.sboles.brew.brewlib.Gravity

    import Helpers._

    /**
     * Companion object
     */
    object SpecificToPlato {
      val decimalFormat = new DecimalFormat("#.###")
    }

    /**
     * Provides a calculator for adjusting converting specific gravity to
     * degrees plato
     */
    class SpecificToPlato extends StatefulSnippet {
      import SpecificToPlato._

      private val logger = Logger(classOf[SpecificToPlato])

      private val sId = "SpecificToPlato"

      private var specific = ""
      private var plato = ""
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
             "specific" -> SHtml.ajaxText(specific,
                                          onChangeSpecific(_),
                                         "id" -> myId("specific")),
             "plato" -> SHtml.span(<span></span>,
                                   JsCmds.Noop,
                                   "id" -> myId("plato")),
             "submit" -> SHtml.ajaxButton("Calculate",
                                          onClickSubmit _,
                                          "class" -> "submitButton"),
             "summary" -> SHtml.span(<span></span>,
                                     JsCmds.Noop,
                                     "id" -> myId("summary")))
      }

      def onChangeSpecific(v: String): JsCmd = {
        specific = v
        onChange
      }

      def onChange: JsCmd = {
        val dSpecific: Double = try {
          specific.toDouble
        } catch {
          case _ => 0.0
        }

        val dPlato: Double = Gravity.specificToPlato(dSpecific)

        val cmds = if ( dPlato > 0 ) {
          val summary = 
            <span>
              Specific gravity:
            <strong>{decimalFormat.format(dSpecific)}</strong>
              is equivalent to
            <strong>{decimalFormat.format(dPlato)}</strong>
              degrees Plato
            </span>
          val result = <span class="result">{
            decimalFormat.format(dPlato)}</span>

          List(
            JsCmds.Replace(myId("plato"),
                           SHtml.span(result,
                                      JsCmds.Noop,
                                      "id" -> myId("plato"))),

            JsCmds.Replace(myId("summary"),
                           SHtml.span(summary,
                                      JsCmds.Noop,
                                      "id" -> myId("summary")))
          )
        } else {
          List(
            JsCmds.Replace(myId("plato"),
                           SHtml.span(<span></span>,
                                      JsCmds.Noop,
                                      "id" -> myId("plato"))),

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
