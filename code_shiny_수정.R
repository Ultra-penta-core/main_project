#load R packages
library(shiny)
library(leaflet)
library(RColorBrewer)
library(xts)
library(rgdal)
library(dplyr)


#helper function for choropleth animation
setShapeStyle <- function( map, data = getMapData(map), layerId,
                           stroke = NULL, color = NULL,
                           weight = NULL, opacity = NULL,
                           fill = NULL, fillColor = NULL,
                           fillOpacity = NULL, dashArray = NULL,
                           smoothFactor = NULL, noClip = NULL, label = NULL,
                           options = NULL){
  
  options <- c(list(layerId = layerId),
               options,
               filterNULL(list(stroke = stroke, color = color,
                               weight = weight, opacity = opacity,
                               fill = fill, fillColor = fillColor,
                               fillOpacity = fillOpacity, dashArray = dashArray,
                               smoothFactor = smoothFactor, noClip = noClip, label = label
               )))
  
  options <- evalFormula(options, data = data)
  options <- do.call(data.frame, c(options, list(stringsAsFactors=FALSE)))
  
  layerId <- options[[1]]
  style <- options[-1]
  if("label" %in% colnames(style)){
    labelData = style[,"label", FALSE]
    style = style[,-which(colnames(style)=="label"), FALSE]
    leaflet::invokeMethod(map, data, "setLabel", "shape", layerId, label)
  }
  leaflet::invokeMethod(map, data, "setStyle", "shape", layerId, style);
}

#helper function in JS for choropleth animation
leafletjs <-  tags$head(
  tags$script(HTML('
  
window.LeafletWidget.methods.setStyle = function(category, layerId, style){
  var map = this;
  if (!layerId){
    return;
  } else if (!(typeof(layerId) === "object" && layerId.length)){
    layerId = [layerId];
  }
  style = HTMLWidgets.dataframeToD3(style);
  layerId.forEach(function(d,i){
    var layer = map.layerManager.getLayer(category, d);
    if (layer){
      layer.setStyle(style[i]);
    }
  });
};

window.LeafletWidget.methods.setLabel = function(category, layerId, label){
  var map = this;
  if (!layerId){
    return;
  } else if (!(typeof(layerId) === "object" && layerId.length)){
    layerId = [layerId];
  }
  layerId.forEach(function(d,i){
    var layer = map.layerManager.getLayer(category, d);
    if (layer){
      layer.unbindTooltip();
      layer.bindTooltip(label[i])
    }
  });
};
'
  ))
)


#load spatial data
world_spdf <- readOGR( 
  dsn = getwd() ,  
  layer = "TM_WORLD_BORDERS_SIMPL-0.3",
  verbose = FALSE
)
#load gdp data set
gdp_medals <- read.csv(file = "./data/gdp_medals.csv", fileEncoding="UTF-8-BOM", stringsAsFactors = FALSE)
gdp_medals <- na.omit(gdp_medals)

olympic_df <- read.csv(file = "./data/athlete_events.csv", fileEncoding="UTF-8-BOM", stringsAsFactors = FALSE)

#match cases and spatial data via ISO3/Country Code
world_spdf$Cases <- gdp_medals$Total[match(world_spdf$ISO3, gdp_medals$Code)]
world_spdf$gdp <- gdp_medals$GDP.per.Capita[match(world_spdf$ISO3, gdp_medals$Code)]
world_spdf$gdp <- as.integer(world_spdf$gdp)
world_spdf$gdp
#world_spdf$medal <- olympic$Medal[match(world_spdf$ISO3, olympic$NOC)]

#create label texts
world_spdf@data$LabelText <- paste0(
  "<b>Country:</b> ", world_spdf@data$NAME,"<br>", 
  "<b>Medals:</b> ", format(world_spdf@data$Cases, nsmall=0, big.mark=","),"<br>",
  "<b>GDP:</b> ", format(world_spdf@data$gdp, nsmall=0, big.mark=",")
)

#define colorpalette for chart legend
paletteBins <- c(0, 25, 50, 75, 100, 250, 500, 750, 1000, 2000, 3000, 4000, 5010)
paletteBins_gdp <- c(0, 1000, 3000, 5000, 10000, 20000, 40000, 60000,80000,110000)
colorPalette <- colorBin(palette = "magma", domain = gdp_medals$Total, na.color = "transparent", bins = paletteBins)
colorPalette_gdp <- colorBin(palette = "YlOrRd", domain = gdp_medals$gdp.per.Capita, na.color = "transparent", bins = paletteBins_gdp)

#shiny UI
ui <- fluidPage(
  leafletjs,
  titlePanel("국가별 GDP와 메달 수의 관계"),
  
  sidebarPanel(width = 2,
               
               radioButtons(inputId = "mapType",
                            label = "국가별 Map visualization",
                            choices = c("국가별 1인당 GDP", "국가별 올림픽 총 메달"),
                            #selected = "Markers",
                            inline = FALSE),
               
               uiOutput("dateUI")
               
  ),
  
  mainPanel(width = 10,
            
            leafletOutput("map", width = "100%", height = "500px")
  )
)



#shiny server
server <- function(input, output, session) {
  
  #  filteredData <- reactive({
  #    req(input$dateSel)
  #    covidData[covidData$Date_reported == input$dateSel, ]
  #  })
  
  #  output$distTable <- renderDataTable({
  #    if (is.null(world_spdf)) return()
  #    world_spdf
  #  })
  
  
  
  # create the base leaflet map
  
  output$map<- renderLeaflet({
    
    if(input$mapType == "국가별 1인당 GDP"){
      
      leaflet(world_spdf) %>%
        addTiles()  %>%
        setView(lat = 0, lng = 0, zoom = 2) %>%
        
        addPolygons(
          layerId = ~ISO3,
          fillColor = "lightgray",
          stroke = TRUE,
          fillOpacity = 1,
          color = "white",
          weight = 1
        ) %>%
        
        
        # 리플렛 범위주기
        leaflet::addLegend(pal = colorPalette_gdp, 
                           values = world_spdf@data$gdp,
                           opacity=0.9, title = "GDP PER CAPITAL", position = "bottomleft")
      
    }else if(input$mapType == "국가별 올림픽 총 메달"){
      leaflet(world_spdf) %>%
        addTiles()  %>%
        setView(lat = 0, lng = 0, zoom = 2) %>%
        
        addPolygons(
          layerId = ~ISO3,
          fillColor = "lightgray",
          stroke = TRUE,
          fillOpacity = 1,
          color = "white",
          weight = 1
        ) %>%
        leaflet::addLegend(pal = colorPalette, 
                           values = world_spdf@data$Cases,
                           opacity=0.9, title = "TOTAL MEDAL", position = "bottomleft")}
    
  })
  
  #prepare data depending on selected date and draw either markers or update polygons depending on the selected map type
  observe({

    
    if(input$mapType == "국가별 1인당 GDP"){
      
      world_spdf$GDP <- gdp_medals$GDP.per.Capita[match(world_spdf$ISO3, gdp_medals$Code)]
      
      world_spdf@data$LabelText <- paste0(
        "<b>Country:</b> ", world_spdf@data$NAME,"<br>", 
        "<b>GDP:</b> ", format(world_spdf@data$GDP, nsmall=0, big.mark=","))
      
      leafletProxy("map", data = world_spdf) %>%
        clearMarkers() %>%
        setShapeStyle(layerId = ~ISO3, fillColor = "lightgray") %>%
        addCircleMarkers(lng = ~LON,
                         lat = ~LAT,
                         radius = ~log(gdp)*2.5,
                         weight = 1,
                         opacity = 1,
                         color = ~ifelse(gdp > 0, "black", "transparent"),
                         fillColor = ~ifelse(gdp > 0, colorPalette_gdp(gdp), "transparent"),
                         fillOpacity = 0.8,
                         label = ~lapply(LabelText, htmltools::HTML))
      
    }else if(input$mapType == "국가별 올림픽 총 메달"){
      
      world_spdf@data$LabelText <- paste0(
        "<b>Country:</b> ", world_spdf@data$NAME,"<br>", 
        "<b>Medals:</b> ", format(world_spdf@data$Cases, nsmall=0, big.mark=","))
      
      leafletProxy("map", data = world_spdf) %>%
        clearMarkers() %>%
        setShapeStyle(layerId = ~ISO3, fillColor = ~ifelse(Cases > 0, colorPalette(Cases), "lightgray"), label = world_spdf$LabelText)
      
    }
    
  })
}



shinyApp(ui, server)  