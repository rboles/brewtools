
package org.sboles.brew.brewtools.lib.rest

import java.text.DecimalFormat

import net.liftweb.common.{Box, Full, Logger}
import net.liftweb.json.JsonAST.{JArray,JObject}
import net.liftweb.json.JsonDSL._
import net.liftweb.http._
import net.liftweb.http.rest._
import net.liftweb.mapper.By

import org.sboles.brew.brewlib._

/**
 * Serves up responses to BrewTools JSON requests
 * @author sboles
 */
object BrewToolsJson extends RestHelper {

  private val logger = Logger(classOf[BrewToolsJson])

  serve {
    case Req("api" :: "temp_convert" :: _, "json",
             GetRequest) => tempConvertToJson

    case Req("api" :: "hop_aau" :: _, "json",
             GetRequest) => hopAauToJson

    case Req("api" :: "gravity_plato" :: _, "json",
             GetRequest) => gravityPlatoToJson

    case Req("api" :: "gravity_adjust" :: _, "json",
             GetRequest) => gravityAdjustToJson

    case Req("api" :: "abv" :: _, "json",
             GetRequest) => abvToJson
  }

  private val decimalFormat = new DecimalFormat("##.##")

  /**
   * Calculates Celsius and Fahrenheit conversions. Request parameters
   * may include multiple ';' delimited temperatures
   * @return Response JSON
   */
  def tempConvertToJson: LiftResponse = {
    val hasT  = S.param("t").toOption
    val hasDC = S.param("c").toOption
    val hasDF = S.param("f").toOption

    val l0: List[JObject] = hasT match {
      case Some(v) => {
        v.split(' ').map(t => {
          val c = decimalFormat.format(Temperature.celsiusToFahrenheit(t))
          val f = decimalFormat.format(Temperature.fahrenheitToCelsius(t))
          ("t" -> t) ~ ("c" -> c) ~ ("f" -> f)
        }).toList
      }
      case None => Nil
    }

    val l1: List[JObject] = hasDC match {
      case Some(v) => {
        v.split(' ').map(c => {
          val f = decimalFormat.format(Temperature.celsiusToFahrenheit(c))
          ("t" -> c) ~ ("c" -> c) ~ ("f" -> f)
        }).toList
      }
      case None => Nil
    }

    val l2: List[JObject] = hasDF match {
      case Some(v) => {
        v.split(' ').map(f => {
          val c = decimalFormat.format(Temperature.fahrenheitToCelsius(f))
          ("t" -> f) ~ ("c" -> c) ~ ("f" -> f)
        }).toList
      }
      case None => Nil
    }

    JsonResponse.apply(new JArray(l0) ++ new JArray(l1) ++ new JArray(l2))
  }

  /**
   * Converts hops measured in AAU to an equivalent measurement in ounces.
   * @return Response JSON
   */
  def hopAauToJson: LiftResponse = {
    val hasAAU  = S.param("aau").toOption
    val hasAA = S.param("aa").toOption

    val l0 = hasAAU match {
      case Some(aau) => {
        hasAA match {
          case Some(v) => {
            v.split(' ').map(aa => {
              val oz = Hops.decimalFormat(Hops.aauToOuncesAA(aau, aa))
              ("aau" -> aau) ~ ("aa" -> aa) ~ ("oz" -> oz)
            }).toList
          }
          case _ => Nil
        }
      }
      case _ => Nil
    }

    JsonResponse.apply(new JArray(l0))
  }

  /**
   * Converts specific gravity to degrees Plato
   * @return Response JSON
   */
  def gravityPlatoToJson: LiftResponse = {
    val hasSpecific = S.param("s").toOption

    val l0 = hasSpecific match {
      case Some(v) => {
        v.split(' ').map(specific => {
          val plato = decimalFormat.format(Gravity.specificToPlato(specific))
          ("s" -> specific) ~ ("p" -> plato)
        }).toList
      }
      case _ => Nil
    }

    JsonResponse.apply(new JArray(l0))
  }

  /**
   * Converts Celsius to Fahrenheit
   * @return Response JSON
   */
  def gravityAdjustCelsiusToJson: JArray = {
    val hasGravity = S.param("g").toOption
    val hasTemp = S.param("c").toOption

    new JArray(hasGravity match {
      case Some(g) => {
        hasTemp match {
          case Some(v) => {
            v.split(' ').map(t => {
              val f = decimalFormat.format(Temperature.celsiusToFahrenheit(t))
              val adj = Gravity.tempAdjustCelsius(g.toDouble, t.toDouble)
              ("g" -> g) ~ ("f" -> f) ~ ("c" -> t) ~
              ("r" -> Gravity.decimalFormat.format(adj))
            }).toList
          }
          case _ => Nil
        }
      }
      case _ => Nil
    })
  }

  /**
   * Converts Fahrenheit to Celsius
   * @return Response JSON
   */
  def gravityAdjustFahrenheitToJson: JArray = {
    val hasGravity = S.param("g").toOption
    val hasTemp = S.param("f").toOption

    new JArray(hasGravity match {
      case Some(g) => {
        hasTemp match {
          case Some(v) => {
            v.split(' ').map(t => {
              val adj = Gravity.tempAdjustFahrenheit(g.toDouble, t.toDouble)
              val c = decimalFormat.format(Temperature.fahrenheitToCelsius(t))
              ("g" -> g) ~ ("f" -> t) ~ ("c" -> c) ~
              ("r" -> Gravity.decimalFormat.format(adj))
            }).toList
          }
          case _ => Nil
        }
      }
      case _ => Nil
    })
  }

  /**
   * Corrects gravity for temperature
   * @return Response JSON
   */
  def gravityAdjustToJson: LiftResponse = {
    JsonResponse.apply(gravityAdjustCelsiusToJson ++
                       gravityAdjustFahrenheitToJson)
  }

  /**
   * Calculaotes ABV
   * @return Response JSON
   */
  private def abvToJson: LiftResponse = {
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

    new JArray(hasGravityInitial match {
      case Some(gi) => {
        hasGravityFinal match {
          case Some(v) => {
            v.split(' ').map(gf => {
              val r = abv(eq, gi, gf, ti, tf)
              ("eq" -> eq) ~ ("gi" -> gi) ~ ("gf" -> gf) ~
              ("r" -> Gravity.abvDecimalFormat.format(r))
            }).toList
          }
          case _ => Nil
        }
      }
      case _ => Nil
    })
  }
}

class BrewToolsJson { }
