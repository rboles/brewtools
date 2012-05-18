
package org.sboles.brew.brewtools.lib.rest

import scala.xml.NodeSeq

import java.text.DecimalFormat

import net.liftweb.common.{Box, Full, Logger}
import net.liftweb.http._
import net.liftweb.http.rest._
import net.liftweb.mapper.By

import org.sboles.brew.brewlib._

/**
 * Serves up responses to BrewTools XML requests
 * @author sboles
 */
object BrewToolsXml extends RestHelper {

  private val logger = Logger(classOf[BrewToolsXml])

  serve {
    case Req("api" :: "temp_convert" :: _, "xml",
             GetRequest) => tempConverToXml

    case Req("api" :: "hop_aau" :: _, "xml",
             GetRequest) => hopAauToXml

    case Req("api" :: "gravity_plato" :: _, "xml",
             GetRequest) => gravityPlatoToXml

    case Req("api" :: "gravity_adjust" :: _, "xml",
             GetRequest) => gravityAdjustToXml

    case Req("api" :: "abv" :: _, "xml",
             GetRequest) => abvToXml
  }

  private val decimalFormat = new DecimalFormat("##.##")

  /**
   * Calculates Celsius and Fahrenheit conversions. Request parameters
   * may include multiple ';' delimited temperatures
   * @return Response XML
   */
  def tempConverToXml: LiftResponse = {
    val hasT  = S.param("t").toOption
    val hasDC = S.param("c").toOption
    val hasDF = S.param("f").toOption

    val l0 = hasT match {
      case Some(v) => {
        v.split(' ').map(t => {
          val c = decimalFormat.format(Temperature.celsiusToFahrenheit(t))
          val f = decimalFormat.format(Temperature.fahrenheitToCelsius(t))
          <conversion t={t} c={c} f={f} />
        }).toList
      }
      case None => Nil
    }

    val l1 = hasDC match {
      case Some(v) => {
        v.split(' ').map(c => {
          val f = decimalFormat.format(Temperature.celsiusToFahrenheit(c))
          <conversion t={c} c={c} f={f} />
        }).toList
      }
      case None => Nil
    }

    val l2 = hasDF match {
      case Some(v) => {
        v.split(' ').map(f => {
          val c = decimalFormat.format(Temperature.fahrenheitToCelsius(f))
          <conversion t={f} c={c} f={f} />
        }).toList
      }
      case None => Nil
    }

    <temperature>{l0 ++ l1 ++ l2}</temperature>
  }

  /**
   * Converts hops measured in AAU to an equivalent measurement in ounces.
   * @return Response XML
   */
  def hopAauToXml: LiftResponse = {
    val hasAAU = S.param("aau").toOption
    val hasAA = S.param("aa").toOption

    val nodes = hasAAU match {
      case Some(aau) => {
        hasAA match {
          case Some (v) => {
            v.split(' ').map(aa => {
              val oz = Hops.decimalFormat(Hops.aauToOuncesAA(aau, aa))
              <hop_aau aa={aa} aau={aau} oz={oz} />
            }).toList
          }
          case _ => Nil
        }
      }
      case _ => Nil
    }

    <hops>{nodes}</hops>
  }

  /**
   * Converts specific gravity to degrees Plato
   * @return Response XML
   */
  def gravityPlatoToXml: LiftResponse = {
    val hasSpecfic = S.param("s").toOption

    val l0 = hasSpecfic match {
      case Some(v) => {
        v.split(' ').map(specific => {
          val p = Gravity.specificToPlato(specific)
          <plato s={specific} p={decimalFormat.format(p)} />
        }).toList
      }
      case None => Nil
    }

    <gravity>{l0}</gravity>
  }

  /**
   * Converts Celsius to Fahrenheit
   * @return Response XML
   */
  def gravityAdjustCelsiusToXml: NodeSeq = {
    val hasGravity = S.param("g").toOption
    val hasTemp = S.param("c").toOption

    hasGravity match {
      case Some(g) => {
        hasTemp match {
          case Some(v) => {
            v.split(' ').map(t => {
              val f = decimalFormat.format(Temperature.celsiusToFahrenheit(t))
              val adj = Gravity.tempAdjustCelsius(g.toDouble, t.toDouble)
              <adjust g={g} f={f} c={t} r={Gravity.decimalFormat.format(adj)} />
            }).toList
          }
          case _ => Nil
        }
      }
      case _ => Nil
    }
  }

  /**
   * Converts Fahrenheit to Celsius
   * @return Response XML
   */
  def gravityAdjustFahrenheitToXml: NodeSeq = {
    val hasGravity = S.param("g").toOption
    val hasTemp = S.param("f").toOption

    hasGravity match {
      case Some(g) => {
        hasTemp match {
          case Some(v) => {
            v.split(' ').map(t => {
              val c = decimalFormat.format(Temperature.fahrenheitToCelsius(t))
              val adj = Gravity.tempAdjustFahrenheit(g.toDouble, t.toDouble)
              <adjust g={g} c={c} f={t} r={Gravity.decimalFormat.format(adj)} />
            }).toList
          }
          case _ => Nil
        }
      }
      case _ => Nil
    }
  }

  /**
   * Corrects gravity for temperature
   * @return Response XML
   */
  def gravityAdjustToXml: LiftResponse = {
    <gravity>{gravityAdjustFahrenheitToXml ++ gravityAdjustCelsiusToXml}</gravity>
  }

  /**
   * Wraps ABV calculation; handles parameter exception
   * @return Response XML
   */
  def abvToXml: LiftResponse = {
    try {
      _abvToXml
    } catch {
      case e:Exception => {
        <error><msg>{e.getMessage}</msg></error>
      }
    }
  }

  /**
   * Calculates ABV
   * @return Response XML
   */
  private def _abvToXml: LiftResponse = {
    val hasTempInitial    = S.param("ti").toOption
    val hasTempFinal      = S.param("tf").toOption
    val hasTempType       = S.param("tc").toOption
    val hasGravityInitial = S.param("gi").toOption
    val hasGravityFinal   = S.param("gf").toOption
    val hasEquation       = S.param("eq").toOption

    def getTemp(hasTemp: Option[String]): String = {
      hasTemp match {
        case Some(temp) => {
          hasTempType match {
            case Some(v) => {
              if ( v.toLowerCase == "t" ) {
                val c = Temperature.celsiusToFahrenheit(temp)
                Temperature.decimalFormat.format(c)
              } else {
                temp
              }
            }
            case _ => temp
          }
        }
        case _ => Temperature.decimalFormat.format(60.0)
      }
    }

    def abv(eq: String, og: String, fg: String, ot: String, ft: String): Double = {
      eq match {
        case "d" => Gravity.abvDaniels(og, fg, ot, ft)
        case _ => Gravity.abvPapazian(og, fg, ot, ft)
      }
    }

    def getEq(hasEquation: Option[String]): String =
      hasEquation match {
        case Some(eq) => {
          eq.toLowerCase match {
            case "d" => "d"
            case _ => "p"
          }
        }
        case _ => "p"
      }

    val ti = getTemp(hasTempInitial)
    val tf = getTemp(hasTempFinal)
    val eq = getEq(hasEquation)

    val l0 = hasGravityInitial match {
      case Some(gi) => {
        hasGravityFinal match {
          case Some(v) => {
            v.split(' ').map(gf => {
              val r = abv(eq, gi, gf, ti, tf)
              <abv eq={eq} gi={gi} gf={gf}
              r={Gravity.abvDecimalFormat.format(r)} />
            })
          }
          case _ => Nil
        }
      }
      case _ => Nil
    }

    <gravity>{l0}</gravity>
  }
}


class BrewToolsXml { }
