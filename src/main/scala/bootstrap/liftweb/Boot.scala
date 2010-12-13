package bootstrap.liftweb

import _root_.net.liftweb.common._
import _root_.net.liftweb.util._
import _root_.net.liftweb.http._
import _root_.net.liftweb.sitemap._
import _root_.net.liftweb.sitemap.Loc._
import Helpers._
import com.nancydeschenes.mosaique.snippet.Mosaique

/**
 * A class that's instantiated early and run.  It allows the application
 * to modify lift's environment
 */
class Boot {
  def boot {
    // where to search snippet
    LiftRules.addToPackages("com.nancydeschenes.mosaique")

    // Build SiteMap
    val entries = Menu(Loc("Home", List("index"), "Home", Hidden)) :: Nil
    LiftRules.setSiteMap(SiteMap(entries: _*))

    LiftRules.dispatch.append {
      case Req(List("image", red, green, blue), _, _) =>
        () => Mosaique.picture(red, green, blue)
    }
  }
}
