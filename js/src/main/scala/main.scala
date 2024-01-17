import org.scalajs.dom

import scala.scalajs.js

import be.adamv.cz2.*

@main def m =
  val root = dom.document.querySelector("#board").asInstanceOf[dom.html.Div]
  val tn = org.scalajs.dom.document.createTextNode("Test")
  root.appendChild(tn)
  