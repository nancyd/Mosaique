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
      case Req(List("image1", color1, ignored), _, _) =>
        () => Mosaique.picture(Array(color1))
      case Req(List("image4", color1, color2, color3, color4, ignored), _, _) =>
        () => Mosaique.picture(Array(color1, color2, color3, color4))
      case Req(List("image9", color1, color2, color3, color4,
    		  color5, color6, color7, color8, color9, ignored), _, _) =>
        () => Mosaique.picture(Array(color1, color2, color3, color4, color5, 
        		color6, color7, color8, color9))
    }
    
    ResourceServer.allow {
    	case "jqueryui" :: _ => true
    	case "js" :: _ => true
    }
  }
}

