

    $.widget("ui.imagePicker", {
      _create: function() {
        var wholeId = this.element[0].id;
        var idParts = wholeId.split("-");
        if (idParts.length != 4) {
          alert("nala can't count");
          return;
        }
        var tileX = idParts[1]
        var tileY = idParts[2]
        var colors = idParts[3].split(":");
        var imagesLoaded = false;

		if (colors.length == 1) {
		  this.element.html('<img src="image1/' + colors[0] + "/" + tileX + tileY + '" />'); 
		} else {
		  this.element.html('<img src="image4/' + colors.join("/") + "/" + tileX + tileY + '" />'); 
		}

		var pickerHtml = "<table>";
		for (i = 0; i < 3; i++) {
		  pickerHtml = pickerHtml + "<tr>";
		  for (j = 0; j < 3; j++) {
		    pickerHtml = pickerHtml + "<td>";
		    pickerHtml = pickerHtml + "<img id=\"imgPickerDialog-" + tileX + "-" + tileY + "-" + i + "-" + j + "\" style=\"height:75px; width: 75px\"/>"		
		    pickerHtml = pickerHtml + "</td>";
		  }
		  pickerHtml = pickerHtml + "</tr>";
		}
		pickerHtml = pickerHtml + "</table>";
		var picker = $('<div></div>')
		.html(pickerHtml)
		.dialog({
			autoOpen: false,
			title: 'Basic Dialog'
		});
		
				
		this.element.click(function(e) {
		  if (!imagesLoaded) {
		    var url = "http://piximilar-flickr.hackmtl.tineye.com/rest/"
		    var data = {
		      method: "color_search"
		    }
		    for (c = 0; c < colors.length; c++) {
		      data["colors[" + c + "]"] = colors[c];
		      data["weights[" + c + "]"] = "1";
		    }
		    $.ajax({ 
		      url: url, 
		      type: "GET",
		      dataType: 'jsonp',
		      data:data,
		  	  success: function (data, textStatus, request) {
		  		  if (data.result.length > 0) {
		  		    for (i = 0; i < 3; i++) {
		  		      for (j = 0; j < 3; j++ ) {
		  		        if (data.result[3*i + j]) {
		  		 		  var filepath = data.result[3*i + j].filepath;
		  		          var srcBig = "http://piximilar-flickr.hackmtl.tineye.com/collection/?filepath=" + filepath + "&size=75"
		  		          var w = $('#imgPickerDialog-' + tileX + "-" + tileY + "-" + i + '-' + j)
		  		          w.attr('src', srcBig);
		  		          w.click(function(e) {
		  		            srcSmall = e.target.src.replace("size=75", "size=20");
		  		            $("span[id^='imgPick-" + tileX + "-" + tileY + "'] > img").attr('src', srcSmall);
		  		            picker.dialog('close');
		  		          })
		  		        }
		  		      }
		  		    }
		  		    imagesLoaded = true;
		  		  } else {
		  		    alert("no results");
		  		  }
		  	  },
		  	  error: function(req, statusText, t) {
		  	    alert("Error: " + statusText);
		  	  }
		    });
		  }
		  picker.dialog('open');
		})
      },
            
    })
