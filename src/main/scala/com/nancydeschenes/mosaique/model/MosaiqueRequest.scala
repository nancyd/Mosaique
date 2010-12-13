package com.nancydeschenes.mosaique.model

import java.awt.image.BufferedImage
import net.liftweb.http._


/** Curently unused */

class MosaiqueRequest (
	var name: String,
    var width: Int,
	var height: Int) {
	
	var sourceFile: FileParamHolder = null;
	var image: BufferedImage = null;
}
