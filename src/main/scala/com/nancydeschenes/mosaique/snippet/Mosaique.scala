package com.nancydeschenes.mosaique.snippet

import com.nancydeschenes.mosaique.model._
import net.liftweb._
import http._
import scala.xml.{ NodeSeq, Group }

import mapper._
import S._
import SHtml._

import common._
import util._
import Helpers._

import java.awt.image.BufferedImage
import java.awt.Color;
import java.awt.AlphaComposite
import javax.imageio.ImageIO
import java.net.URL

class Mosaique extends StatefulSnippet {

  //  var mr: MosaiqueRequest;
  var width: Int = 1;
  var height: Int = 1;
  var name: String = "NOT PROVIDED";
  var sourceFile: Box[FileParamHolder] = null

  var image: BufferedImage = null;
  var scaled: BufferedImage = null;
  var uniqueMarker = 0;
  def dispatch = {
    case "form" => form _
  }

  def form(xhtml: NodeSeq): NodeSeq = {
    bind("mosaique", xhtml, // namespace
      "name" -> SHtml.text(name, name = _),
      "width" -> SHtml.text(width.toString(), v => setWidth(v)),
      "height" -> SHtml.text(height.toString(), v => setHeight(v)),
      "sourceFile" -> SHtml.fileUpload(ul => setSourceFile(ul)),
      "renderMosaique" -> renderMosaique _);
  }

  def renderMosaique(xml: NodeSeq): NodeSeq = {
    if (image == null) {
      return <span>Upload a file first</span>
    }
    val scaled = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB)
    val g = scaled.createGraphics;
    g.setComposite(AlphaComposite.Src)
    g.drawImage(image, 0, 0, width, height, null)
    val rows = new Array[Array[Color]](height);

    for (i <- 0 to height - 1) {
      rows(i) = new Array[Color](width)
      for (j <- 0 to width - 1) {
        rows(i)(j) = getColor(scaled, j, i)
      }
    }

    <lift:children>
      <div>Your mosaique here:{ name }is{ width }X{ height }</div>
      <span class="table">
        { rows.map(doLine(_)) }
      </span>
    </lift:children>
  }

  def doLine(cols: Array[Color]): NodeSeq = {
    <lift:children>
      <div style="height:20px">
        { cols.map(doCell(_)) }
      </div>
    </lift:children>
  }

  def getColor(img: BufferedImage, col: Int, row: Int): Color = {
    val color = img.getRGB(col, row);
    return new Color(color);
  }

  def doCell(color: Color): NodeSeq = {
	  import _root_.scala.compat.Platform
    val red = color.getRed;
    val green = color.getGreen;
    val blue = color.getBlue;
    uniqueMarker += 1;
    val imgUrl = "/image/" + red + "/" + green + "/" + blue + "/" +  uniqueMarker;
    <img src={ imgUrl }/>
  }

  def setWidth(s: String) = {
    println("width: " + s)
    width = s.toInt;
  }

  def setHeight(s: String) = {
    println("height: " + s)
    height = s.toInt;
  }

  def setSourceFile(s: FileParamHolder) {
    println("Source file: " + s);
    image = ImageIO.read(s.fileStream);
    var w = image.getWidth
    var h = image.getHeight
    println(w + " x " + h + " : " + image)
  }
}

object Mosaique {
  val baseURL = "http://piximilar-flickr.hackmtl.tineye.com/rest/?method=color_search"

  def picture(red: Any, green: Any, blue: Any): Box[LiftResponse] = {
    println("Red: " + red)
    println("Green: " + green)
    println("Blue: " + blue)
    val rgb = red + "," + green + "," + blue
    val apiCall = baseURL + "&colors[0]=" + rgb + "&weights[0]=1";
    val url = new URL(apiCall);
    import net.liftweb.util.JSONParser
    import net.liftweb.util.IoHelpers;
    val bytes = IoHelpers.readWholeStream(url.openStream);
    val contentString = new String(bytes);
    val b = JSONParser.parse(contentString)
    b match {
      case Full(data: Map[String, Any]) => {
        data.get("result") match {
          case Some(results: List[Map[String, String]]) => {
		    val filesAndScores : List[(String, Double)] =
		    	results.map((x) => (x.get("filepath").get, x.get("score").get.toDouble))
            val filepath = pickOne(filesAndScores);
            val imgUrl = "http://piximilar-flickr.hackmtl.tineye.com/collection/?filepath=" + filepath + "&size=20"
            S.redirectTo(imgUrl);
          }
          case None => {
            Failure("Invalid JSON submitted: \"%s\"".format(contentString))
          }
        }
      }
      case other =>
        {
          println("error: " + contentString);
          Failure("Invalid JSON submitted: \"%s\"".format(contentString))
        }
        Failure("Invalid JSON submitted: \"%s\"".format(contentString))
    }
  }

  def pickOne(options: List[(String, Double)]): String = {
    // add up the scores.
    var sum : Double = 0;
    val distributions = options.map((option) => {
    	sum += option._2;
    	(option._1, sum)
    	
    })
    println("sum = " + sum);
    
    var x = 0;
    import scala.util.Random;
    val rand = Random.nextDouble * sum;
    // linear search
    distributions foreach { dist =>
      if (dist._2 > rand) {
    	  println("returning the " + x + "th result")
    	  return dist._1
      }
    }
    "bad math";
  }
}
