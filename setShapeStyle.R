# library(devtools)
# devtools::install_github("rstudio/leaflet")
# devtools::install_github("Leaflet/Leaflet.label",upgrade = c("never"))

### R functions
# add in methods from https://github.com/rstudio/leaflet/pull/598
setCircleMarkerRadius <- function(map, layerId, radius, data=getMapData(map)){
  options <- list(layerId = layerId, radius = radius)
  # evaluate all options
  options <- evalFormula(options, data = data)
  # make them the same length (by building a data.frame)
  options <- do.call(data.frame, c(options, list(stringsAsFactors=FALSE)))
  leaflet::invokeMethod(map, data, "setRadius", options$layerId, options$radius)
}

setCircleMarkerStyle <- function(map, layerId
                                 , radius = NULL
                                 , stroke = NULL
                                 , color = NULL
                                 , weight = NULL
                                 , opacity = NULL
                                 , fill = NULL
                                 , fillColor = NULL
                                 , fillOpacity = NULL
                                 , dashArray = NULL
                                 , options = NULL
                                 , data = getMapData(map)
){
  if (!is.null(radius)){
    setCircleMarkerRadius(map, layerId = layerId, radius = radius, data = data)
  }
  
  options <- c(list(layerId = layerId),
               options,
               filterNULL(list(stroke = stroke, color = color,
                               weight = weight, opacity = opacity,
                               fill = fill, fillColor = fillColor,
                               fillOpacity = fillOpacity, dashArray = dashArray
               )))
  
  if (length(options) < 2) { # no style options set
    return()
  }
  # evaluate all options
  options <- evalFormula(options, data = data)
  
  # make them the same length (by building a data.frame)
  options <- do.call(data.frame, c(options, list(stringsAsFactors=FALSE)))
  layerId <- options[[1]]
  style <- options[-1] # drop layer column
  
  #print(list(style=style))
  leaflet::invokeMethod(map, data, "setStyle", "marker", layerId, style);
}

setShapeStyle <- function( map, data = getMapData(map), layerId,
                           stroke = NULL, color = NULL,
                           weight = NULL, opacity = NULL,
                           fill = NULL, fillColor = NULL,
                           fillOpacity = NULL, dashArray = NULL,
                           smoothFactor = NULL, noClip = NULL,
                           options = NULL
){
  options <- c(list(layerId = layerId),
               options,
               filterNULL(list(stroke = stroke, color = color,
                               weight = weight, opacity = opacity,
                               fill = fill, fillColor = fillColor,
                               fillOpacity = fillOpacity, dashArray = dashArray,
                               smoothFactor = smoothFactor, noClip = noClip
               )))
  # evaluate all options
  options <- evalFormula(options, data = data)
  # make them the same length (by building a data.frame)
  options <- do.call(data.frame, c(options, list(stringsAsFactors=FALSE)))
  
  layerId <- options[[1]]
  style <- options[-1] # drop layer column
  
  #print(list(style=style))
  leaflet::invokeMethod(map, data, "setStyle", "shape", layerId, style);
}

### JS methods
leafletjs <-  tags$head(
  # add in methods from https://github.com/rstudio/leaflet/pull/598
  tags$script(HTML(
    '
window.LeafletWidget.methods.setStyle = function(category, layerId, style){
  var map = this;
  if (!layerId){
    return;
  } else if (!(typeof(layerId) === "object" && layerId.length)){ // in case a single layerid is given
    layerId = [layerId];
  }

  //convert columnstore to row store
  style = HTMLWidgets.dataframeToD3(style);
  //console.log(style);

  layerId.forEach(function(d,i){
    var layer = map.layerManager.getLayer(category, d);
    if (layer){ // or should this raise an error?
      layer.setStyle(style[i]);
    }
  });
};

window.LeafletWidget.methods.setRadius = function(layerId, radius){
  var map = this;
  if (!layerId){
    return;
  } else if (!(typeof(layerId) === "object" && layerId.length)){ // in case a single layerid is given
    layerId = [layerId];
    radius = [radius];
  }

  layerId.forEach(function(d,i){
    var layer = map.layerManager.getLayer("marker", d);
    if (layer){ // or should this raise an error?
      layer.setRadius(radius[i]);
    }
  });
};
'
  ))
)
