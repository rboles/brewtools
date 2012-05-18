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
    import org.sboles.brew.brewlib.Temperature

    import Helpers._

    /**
     * Companion object
     */
    object AlcoholByVolume {

      /**
       * Provides storage for form params
       */
      private class Params(og: Double, fg: Double,
                           abv: Double, eqn: String,
                           ogTemp: Int, fgTemp: Int, tempMeasure: String) {
        def og(): Double = og
        def fg(): Double = fg
        def abv(): Double = abv
        def eqn(): String = eqn
        def ogTemp(): Int = ogTemp
        def fgTemp(): Int = fgTemp
        def tempMeasure(): String = tempMeasure
      }

      private val logger = Logger(classOf[AlcoholByVolume])

      val decimalFormat = new DecimalFormat("#.###")

      private val sId = "AlcoholByVolume"

      /**
       * @param suffix ID suffix
       * @return Unique ID for contents in the control
       */
      def myId(suffix: String): String = sId+"_"+suffix

      /**
       * @return ABV equation sequence
       */
      def abvEqnSeq: Seq[(String, String)] = {
        val eqns = List("Daniels", "Papazian")
        eqns.map(e => (e.toLowerCase, e))
      }

      /**
       * @returm Temperature sequence
       */
      def tempSeq(start: Int, end: Int): Seq[(String, String)] = {
        (start until end).map(i => (i.toString, i.toString)).reverse
      }

      /**
       * @return Measurement type sequence
       */
      def measureSeq: Seq[(String, String)] = {
        val l = List("Fahrenheit", "Celsius")
        l.map(e => (e.toLowerCase, e))
      }

      def eqnToHtml(eqn: String): NodeSeq = {
        eqn match {
          case "papazian" => {
            <span class="equation">
              (OG - FG) * 131.25
            </span>
          }
          case "daniels" => {
            <span class="equation">
              (76.08 * (OG-FG) / (1.775-OG)) * (FG / 0.794)
            </span>
          }
          case _ => <span></span>
        }
      }

      def summarize(params: Params): NodeSeq = {
        val eqnStr = params.eqn.toLowerCase match {
          case "papazian" => "Papazian"
          case "daniels" => "Daniels"
          case _ => "unknown"
        }
        val measureStr = params.tempMeasure.toLowerCase match {
          case "celsius" => "Celsius"
          case "fahrenheit" => "Fahrenheit"
          case _ => "unknown"
        }
        <div>
          An initial gravity of
          <strong>{decimalFormat.format(params.og)}</strong>
          at <strong>{params.ogTemp.toString} &deg;</strong>
          {measureStr}
          and a final gravity of
          <strong>{decimalFormat.format(params.fg)}</strong>
          at <strong>{params.fgTemp.toString} &deg;</strong>
          {measureStr}
          result in a beer with a
          <strong>{decimalFormat.format(params.abv)}%</strong>
          alcohol by volume, using the
          <strong>{eqnStr}</strong> equation:
          <div>
              {eqnToHtml(params.eqn)}
          </div>
        </div>
      }

      /**
       * Unset the summary and resut content
       * @return JS commands
       */
      def unset: JsCmd = {
        val cmds = List(
          JsCmds.Replace(myId("summary"),
                         SHtml.span(<span></span>,
                                    JsCmds.Noop,
                                    "id" -> myId("summary"))),

          JsCmds.Replace(myId("abv"),
                         SHtml.span(<span></span>,
                                    JsCmds.Noop,
                                    "id" -> myId("result"))))
        JsCmds.seqJsToJs(cmds)
      }

      /**
       * Unset the summary and result content
       * @param JS commands
       * @return JS commands
       */
      def unset(cmd: JsCmd): JsCmd = {
        val cmds = List(cmd, unset)
        JsCmds.seqJsToJs(cmds)
      }

    }

    /**
     * Provides a calculator for alcohol by volume
     */
    class AlcoholByVolume extends StatefulSnippet {
      import AlcoholByVolume._

      private var og = ""
      private var ogTemp = "60"
      private var fg = ""
      private var fgTemp = "60"
      private var eqn = "daniels"
      private var tempMeasure = "fahrenheit"

      private var submitted = false

      def dispatch: DispatchIt = {
        case "render" => render _
      }

      def render(xhtml: NodeSeq): NodeSeq = {

        def onSubmit(): Unit = {
          submitted = true
        }

        bind("e", xhtml,
             "original_gravity" -> SHtml.ajaxText(og,
                                                  onChangeOriginalGravity(_),
                                                  "id" -> myId("original_gravity"),
                                                  "class" -> "gravityInput"),

             "original_temp" -> SHtml.ajaxSelect(tempSeq(32, 213),
                                                 Full(ogTemp),
                                                 onChangeOriginalTemp(_),
                                                 "id" -> myId("original_temp")),


             "final_gravity" -> SHtml.ajaxText(fg,
                                               onChangeFinalGravity(_),
                                               "id" -> myId("final_gravity"),
                                               "class" -> "gravityInput"),

             "final_temp" -> SHtml.ajaxSelect(tempSeq(32, 213),
                                              Full(fgTemp),
                                              onChangeFinalTemp(_),
                                              "id" -> myId("final_temp")),

             "temp_measure" -> SHtml.ajaxSelect(measureSeq,
                                                Full(tempMeasure),
                                                onChangeTempMeasure(_)),

             "select_equation" -> SHtml.ajaxSelect(abvEqnSeq,
                                                   Full(eqn),
                                                   onChangeAbvEqn(_),
                                                   "id" -> myId("abv_eqn")),

             "abv" -> SHtml.span(<span></span>,
                                 JsCmds.Noop,
                                 "id" -> myId("result")),

             "submit" -> SHtml.ajaxButton("Calculate",
                                          onClickSubmit _,
                                          "class" -> "submitButton"),

             "summary" -> SHtml.span(<span></span>,
                                     JsCmds.Noop,
                                     "id" -> myId("summary"))
           )

      }

      /**
       * Triggered when the ABV equation type changes.
       * @param v Equantion type
       * @return JS commands
       */
      def onChangeAbvEqn(v: String): JsCmd = {
        eqn = v
        if ( og.length > 0 && fg.length > 0 ) onChange
        else unset
      }

      def onChangeOriginalGravity(v: String): JsCmd = {
        og = v
        if ( og.length > 0 && fg.length > 0 ) onChange
        else unset
      }

      def onChangeOriginalTemp(v: String): JsCmd = {
        ogTemp = v
        if ( og.length > 0 && fg.length > 0 ) onChange
        else unset
      }

      def onChangeFinalGravity(v: String): JsCmd = {
        fg = v
        if ( og.length > 0 && fg.length > 0 ) onChange
        else unset
      }

      def onChangeFinalTemp(v: String): JsCmd = {
        fgTemp = v
        if ( og.length > 0 && fg.length > 0 ) onChange
        else unset
      }

      /**
       * Saves the state of temperature measure type, updates the
       * temperature select controls and then triggers form update
       * @param Temperature measure
       * @return JS commands
       */
      def onChangeTempMeasure(v: String): JsCmd = {
        tempMeasure = v

        tempMeasure.substring(0,1).toLowerCase match {
          case "c" => {
            ogTemp = Temperature.fahrenheitToCelsius(ogTemp).toInt.toString
            fgTemp = Temperature.fahrenheitToCelsius(fgTemp).toInt.toString
          }
          case _ => {
            ogTemp = Temperature.celsiusToFahrenheit(ogTemp).toInt.toString
            fgTemp = Temperature.celsiusToFahrenheit(fgTemp).toInt.toString
          }
        }

        val cmds = tempMeasure.substring(0,1).toLowerCase match {
          case "c" => fahrenheitControlToCelsius
          case _ => celsiusControlToFahrenheit
        }

        if ( og.length > 0 && fg.length > 0 ) {
          onChange(cmds)
        } else {
          unset(cmds)
        }
      }

      /**
       * Changes the temperature select controls from Fahrenheit to Celsius
       * @return JS commands
       */
      def fahrenheitControlToCelsius: JsCmd = {
        val cmds = List(
          JsCmds.Replace(myId("original_temp"),
                         SHtml.ajaxSelect(tempSeq(0, 101),
                                          Full(ogTemp),
                                          onChangeOriginalTemp(_),
                                          "id" -> myId("original_temp"))),
          JsCmds.Replace(myId("final_temp"),
                         SHtml.ajaxSelect(tempSeq(0, 101),
                                          Full(fgTemp),
                                          onChangeFinalTemp(_),
                                          "id" -> myId("final_temp")))
          )
        JsCmds.seqJsToJs(cmds)
      }

      /**
       * Changes the temperature select controls from Celsius to Fahrenheit
       * @return JS commands
       */
      def celsiusControlToFahrenheit: JsCmd = {
        val cmds = List(
          JsCmds.Replace(myId("original_temp"),
                         SHtml.ajaxSelect(tempSeq(32, 213),
                                          Full(ogTemp),
                                          onChangeOriginalTemp(_),
                                          "id" -> myId("original_temp"))),
          JsCmds.Replace(myId("final_temp"),
                         SHtml.ajaxSelect(tempSeq(32, 213),
                                          Full(fgTemp),
                                          onChangeFinalTemp(_),
                                          "id" -> myId("final_temp")))
          )
        JsCmds.seqJsToJs(cmds)
      }

      /**
       * Calculates the result based on the state of the controls
       * @return JS commands
       */
      def onChange: JsCmd = {
        val dOGTemp: Double =
          tempMeasure.substring(0,1).toLowerCase match {
            case "c" => Temperature.celsiusToFahrenheit(ogTemp)
            case _ => ogTemp.toDouble
          }

        val dFGTemp: Double =
          tempMeasure.substring(0,1).toLowerCase match {
            case "c" => Temperature.celsiusToFahrenheit(fgTemp)
            case _ => fgTemp.toDouble
          }

        val dOG: Double = try {
          Gravity.tempAdjustFahrenheit(og.toDouble, dOGTemp)
        } catch {
          case _ => 0.0
        }

        val dFG: Double = try {
          Gravity.tempAdjustFahrenheit(fg.toDouble, dFGTemp)
        } catch {
          case _ => 0.0
        }

        val dABV: Double = {
          if ( dOG > 0 && dFG > 0 ) {
            eqn match {
              case "daniels" =>  Gravity.abvDaniels(dOG, dFG)
              case "papazian" => Gravity.abvPapazian(dOG, dFG)
              case _ => 0.0
            }
          } else {
            0.0
          }
        }

        val params = new Params(og.toDouble,fg.toDouble,
                                dABV, eqn,
                                ogTemp.toInt, fgTemp.toInt,
                                tempMeasure)

        val summary = summarize(params)

        val result = <span class="result">{decimalFormat.format(dABV)}</span>

        val cmds = if ( dABV > 0 ) {
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

      def onChange(cmd: JsCmd): JsCmd = {
        JsCmds.seqJsToJs(List(onChange, cmd))
      }

      def onClickSubmit: JsCmd = JsCmds.Noop
    }
  }
}
