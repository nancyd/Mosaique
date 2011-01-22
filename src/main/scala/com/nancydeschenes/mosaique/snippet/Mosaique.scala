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

//import java.awt.image.BufferedImage
import java.awt.Color;
//import java.awt.AlphaComposite
//import javax.imageio.ImageIO

import com.google.appengine.api.images.Image;
import com.google.appengine.api.images.ImagesService;
import com.google.appengine.api.images.ImagesServiceFactory;
import com.google.appengine.api.images.Transform;

import java.net.URL


class Mosaique extends StatefulSnippet {

  val imageService = ImagesServiceFactory.getImagesService();

  //  var mr: MosaiqueRequest;
  var width: Int = 1;
  var height: Int = 1;
  var name: String = "NOT PROVIDED";
  var sourceFile: Box[FileParamHolder] = null

  var image: Image = null;
  var scaled: Image = null;
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
    
    // Each tile is based on a 2x2 sample
    val scaleTransform = ImagesServiceFactory.makeResize(width *2, height * 2)
    val scaled = imageService.applyTransform(scaleTransform, image)
    val rows = new Array[Array[Tuple4[Color, Color, Color, Color]]](height);

    for (i <- 0 to height - 1) {
      rows(i) = new Array[Tuple4[Color, Color, Color, Color]](width)
      for (j <- 0 to width - 1) {
        rows(i)(j) = (getColor(scaled, j * 2, i * 2),
          getColor(scaled, j * 2 + 1, i * 2),
          getColor(scaled, j * 2, i * 2 + 1),
          getColor(scaled, j * 2 + 1, i * 2 + 1))
      }
    }

    <lift:children>
      <div>Your mosaique here:{ name }is{ width }X{ height }</div>
      <span class="table">
        { rows.map(doLine(_)) }
      </span>
    </lift:children>
  }

  def getColor(img: Image, col: Int, row: Int): Color = {
    val color = img.getRGB(col, row);
    return new Color(color);
  }

  def doLine(cols: Array[Tuple4[Color, Color, Color, Color]]): NodeSeq = {
    <lift:children>
      <div style="height:20px">
        { cols.map(doCell(_)) }
      </div>
    </lift:children>
  }

  def doCell(colors: Tuple4[Color, Color, Color, Color]): NodeSeq = {
    import _root_.scala.compat.Platform

    uniqueMarker += 1;
    val imgUrl = "/image/" + colors.productIterator.map(c => colorToString(c)).mkString("/");
    <img src={ imgUrl + "/" + uniqueMarker }/>
  }

  def colorToString(colorIHope: Any): String = {
    colorIHope match {
      case c: Color => {
        (c.getRed, c.getGreen, c.getBlue).productIterator.mkString(",");
      }
    }
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
    
    image = ImagesServiceFactory.makeImage(s.file)
    var w = image.getWidth
    var h = image.getHeight
    println(w + " x " + h + " : " + image)
  }
}

object Mosaique {
  val baseURL = "http://piximilar-flickr.hackmtl.tineye.com/rest/?method=color_search"

  def picture(colors: Tuple4[String, String, String, String]): Box[LiftResponse] = {

    var colorWeights: Map[String, Int] = Map()
    colors.productIterator.foreach { c =>
      c match {
        case color: String => {
          val x = colorWeights.getOrElse(color, 0);
          colorWeights += (color -> (x + 1))
        }
      }
    }

    var apiCall = baseURL; // + "&colors[0]=" + rgb + "&weights[0]=1";
    var idx = 0;
    colorWeights.foreach(color => {
      apiCall += "&colors%5B" + idx + "%5D=" + color._1 + "&weights%5B" + idx + "%5D=" + (color._2 * 0.25)
      idx += 1;
    })

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
            val filesAndScores: List[(String, Double)] =
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
    var sum: Double = 0;
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
      x += 1;
      if (dist._2 > rand) {
        println("returning the " + x + "th result")
        return dist._1
      }
    }
    "bad math";
  }
}
