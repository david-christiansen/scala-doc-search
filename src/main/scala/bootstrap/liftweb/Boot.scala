package bootstrap.liftweb

import net.liftweb._
import util._
import Helpers._

import common._
import http._
import sitemap._
import Loc._
import mapper._

class Boot {
  def boot {
    if (!DB.jndiJdbcConnAvailable_?) {
      val vendor =
        new StandardDBVendor(Props.get("db.driver") openOr "org.h2.Driver",
                             Props.get("db.url") openOr
                             "jdbc:h2:lift_proto.db;AUTO_SERVER=TRUE",
                             Props.get("db.user"), Props.get("db.password"))

      LiftRules.unloadHooks.append(vendor.closeAllConnections_! _)

      DB.defineConnectionManager(DefaultConnectionIdentifier, vendor)
    }

    //Register packages for auto-finding snippet
    LiftRules.addToPackages("websearch")

    //Build SiteMap
    val entries = List(
      Menu.i("Home") / "index",
      //Show anything from the Static dir
      Menu(Loc("Static", Link(List("static"), true, "/static/index"),
               "Static Content")),
      Menu.i("Search") / "search" / "index"
    )
    LiftRules.setSiteMap(SiteMap(entries:_*))

    
    // Force the request to be UTF-8
    LiftRules.early.append(_.setCharacterEncoding("UTF-8"))
  }
}
