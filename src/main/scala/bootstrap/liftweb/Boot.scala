package bootstrap.liftweb

import _root_.net.liftweb.util._
import _root_.net.liftweb.common._
import _root_.net.liftweb.http._
import _root_.net.liftweb.http.provider._
import _root_.net.liftweb.sitemap._
import _root_.net.liftweb.sitemap.Loc._

import Helpers._

import _root_.org.sboles.brew.brewtools.lib.rest._

/**
 * A class that's instantiated early and run. Allows the application
 * to modify lift's environment
 */
class Boot {

  private val logger = Logger(classOf[Boot])

  def boot {
    // where to search snippet
    LiftRules.addToPackages("org.sboles.brew.brewtools")

    // site map
    val apiMenu = Menu(Loc("API", "api" :: "index" :: Nil, "API", Hidden))
    val homeMenu = Menu(Loc("Home", "index" :: Nil, "", Hidden))
    val allMenus = homeMenu :: apiMenu :: Nil
    val mySiteMap = SiteMap(allMenus: _*)

    LiftRules.setSiteMap(mySiteMap)

    LiftRules.early.append(makeUtf8)

    LiftRules.statelessDispatchTable.append(BrewToolsXml)

    LiftRules.statelessDispatchTable.append(BrewToolsJson)
  }

  /**
   * Force the request to be UTF-8
   */
  private def makeUtf8(req: HTTPRequest) {
    req.setCharacterEncoding("UTF-8")
  }
}
