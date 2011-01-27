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
  var sampleSize =  "1"
  val sampleSizes = List("1", "4")

  var image: BufferedImage = null;
  var scaled: BufferedImage = null;
  var uniqueMarker = 0;
  def dispatch = {
    case "form" => form _
  }
  
  
  def form(xhtml: NodeSeq): NodeSeq = {
    bind("mosaique", xhtml, // namespace
      "sampleSize" -> SHtml.radio(sampleSizes, Empty, sampleSize=_).toForm,
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
    val images : Array[Array[String]] = sampleSize match {
//    	case "9" => getImageUrls9
    	case "4" => getImageUrls4
    	case _ => getImageUrls1
    }
    val lines = 0 until images.length
    <lift:children>
      <div>Your mosaique here:{ name }is{ width }X{ height }</div>
      <span class="table">
        { 
        	lines.map(l => doLine(images(l), l)) 
        }
      </span>
    </lift:children>
  }
  
  
  def getImageUrls1 : Array[Array[String]] = {
    val scaled = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB)
    val g = scaled.createGraphics;
    g.setComposite(AlphaComposite.Src)
    g.drawImage(image, 0, 0, width, height, null)
    val rows = new Array[Array[String]](height);

    for (i <- 0 to height - 1) {
      rows(i) = new Array[String](width)
      for (j <- 0 to width - 1) {
        val color = getColor(scaled, i, j);
        
        rows(i)(j) = colorToString(color)
      }
    }
    return rows;
  }
  
  def getImageUrls4 : Array[Array[String]] = {
    val scaled = new BufferedImage(width * 2, height * 2, BufferedImage.TYPE_INT_RGB)
    val g = scaled.createGraphics;
    g.setComposite(AlphaComposite.Src)
    g.drawImage(image, 0, 0, width * 2, height * 2, null)
    val rows = new Array[Array[String]](height);

    for (i <- 0 to height - 1) {
      rows(i) = new Array[String](width)
      for (j <- 0 to width - 1) {
        val colors =
        new Tuple4(getColor(scaled, i * 2, j * 2),
          getColor(scaled, i * 2 + 1, j * 2),
          getColor(scaled, i * 2, j * 2 + 1),
          getColor(scaled, i * 2 + 1, j * 2 + 1));

        rows(i)(j) = 
          colors.productIterator.map(colorToString(_)).mkString(":")
      }
    }
    
	return rows;  
  }
  
  
  
//  def getImageUrls9 : Array[Array[String]] = {
//    val scaled = new BufferedImage(width * 3, height * 3, BufferedImage.TYPE_INT_RGB)
//    val g = scaled.createGraphics;
//    g.setComposite(AlphaComposite.Src)
//    g.drawImage(image, 0, 0, width * 3, height * 3, null)
//    val rows = new Array[Array[String]](height);
//
//    for (i <- 0 to height - 1) {
//      rows(i) = new Array[String](width)
//      for (j <- 0 to width - 1) {
//        val colors =
//        new Tuple9(getColor(scaled, j * 3, i * 3),
//        		getColor(scaled, j * 3 + 1, i * 3),
//        		getColor(scaled, j * 3 + 2, i * 3),
//        		getColor(scaled, j * 3, i * 3 + 1),
//        		getColor(scaled, j * 3 + 1, i * 3 + 1),
//        		getColor(scaled, j * 3 + 2, i * 3 + 1),
//        		getColor(scaled, j * 3, i * 3 + 2),
//        		getColor(scaled, j * 3 + 1, i * 3 + 2),
//        		getColor(scaled, j * 3 + 2, i * 3 + 2));
//
//        rows(i)(j) = "/image9/" + 
//          colors.productIterator.map(colorToString(_)).mkString("/")
//      }
//    }
//    return rows;
//  }
  

  def getColor(img: BufferedImage, row: Int, col: Int): Color = {
    val color = img.getRGB(col, row);
    return new Color(color);
  }

  def doLine(cols: Array[String], row: Int): NodeSeq = {
   	val colIndexes = 0 until cols.length
    <lift:children>
     <div style="height:20px">
        { 
        	colIndexes.map(c => doCell(cols(c), row, c)) 
        }
      </div>
     </lift:children> 
  }

  def doCell(colors: String, row: Int, col:Int) : NodeSeq = {
    import _root_.scala.compat.Platform

    uniqueMarker += 1;
    println(row + " " + col + " = " + colors)
    <span id={"imgPick-" + row + "-" + col + "-" + colors}
    	class="imagePicker" style="padding:0 ; margin: 0; spacing:0">
    </span>
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
    image = ImageIO.read(s.fileStream);
    var w = image.getWidth
    var h = image.getHeight
    println(w + " x " + h + " : " + image)
  }
}

object Mosaique {
  val baseURL = "http://piximilar-flickr.hackmtl.tineye.com/rest/?method=color_search"

  def picture(colors: Array[String]) : Box[LiftResponse] = {

    var colorWeights: Map[String, Int] = Map()
    colors.foreach { c =>
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
