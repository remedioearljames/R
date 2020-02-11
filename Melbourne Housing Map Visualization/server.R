library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)
library(babynames)

# Leaflet bindings are a bit slow; for now we'll just sample to compensate
set.seed(100)
zipdata <- allzips[sample.int(nrow(allzips), 10000),]
# By ordering by centile, we ensure that the (comparatively rare) SuperZIPs
# will be drawn last and thus be easier to see
zipdata <- zipdata[order(zipdata$centile),]
# ed_exp5 <- select(filter(education, Region == 2),c(State,Minor.Population:Education.Expenditures))
FinalData <- cleandata[ which(cleandata$Price < 5000000), ]
  
print(max(FinalData$Price))
print(max(FinalData$Rooms))
print(max(FinalData$Car))
function(input, output, session) {
  
  ## Interactive Map ###########################################
  
  # Create the map
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles(
        urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
        attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
      ) %>%
      setView(lng = 144.946457, lat = -37.840935, zoom = 11)
  })
  
  # A reactive expression that returns the set of zips that are
  # in bounds right now
  zipsInBounds <- reactive({
    if (is.null(input$map_bounds))
      return(FinalData[FALSE,])
    bounds <- input$map_bounds
    latRng <- range(bounds$north, bounds$south)
    lngRng <- range(bounds$east, bounds$west)
    
    subset(FinalData,
           Lat >= latRng[1] & Lat <= latRng[2] &
             Lng >= lngRng[1] & Lng <= lngRng[2])
  })
  
  # Precalculate the breaks we'll need for the two histograms
  centileBreaks <- hist(plot = FALSE, FinalData$Price, breaks = 20)$breaks
  
  output$histCentile <- renderPlot({
    # If no zipcodes are in view, don't plot
    if (nrow(zipsInBounds()) == 0)
      return(NULL)
    
    hist(
         zipsInBounds()$Price,
         breaks = centileBreaks,
         main = "House Price",
         xlab = "Price",
         xlim = range(input$rangePrice[1], input$rangePrice[2]),
         col = '#00DD00',
         border = 'white')
  })
 
  output$Animation <- renderPlot({
   
   
  })
  
 
  # This observer is responsible for maintaining the circles and legend,
  # according to the variables the user has chosen to map to color and size.
  
 
  observe({
    colorBy <- input$color
    sizeBy <- input$size
    minPrice <- input$rangePrice[1]
    maxPrice <- input$rangePrice[2]
    rooms <- input$rooms
    brooms <- input$bathrooms
    cars <- input$cars
    region  <- input$regions
   
    print(max(FinalData$Bedroom))
      if(maxPrice > minPrice ){
        FinalData <- filter(FinalData, Price < maxPrice & Price > minPrice)
      } 
      if(rooms != "" & rooms > 0 & rooms <= max(FinalData$Bedroom))
      {
        FinalData <- filter(FinalData, Bedroom == rooms)
      }  
      if(brooms != "" & brooms > 0 & brooms <= max(FinalData$Bathroom))
      { 
        FinalData <- filter(FinalData, Bathroom == brooms)
      } 
      if(region != "" & !is.null(region) )
      { 
       FinalData <- filter(FinalData, Region == region)
      } 
    # if (colorBy == "superzip") {
    #   # Color and palette are treated specially in the "superzip" case, because
    #   # the values are categorical instead of continuous.
    #   colorData <- ifelse(cleandata$Price >= (100 - input$threshold), "yes", "no")
    #   pal <- colorFactor("magma", colorData)
    # } else {
      # colorData <- FinalData[[Price]]
    #   pal <- colorBin("magma", colorData, 7, pretty = FALSE)
    #    
    # }
    # 
    # if (sizeBy == "superzip") {
    #   # Radius is treated specially in the "superzip" case.
    #   radius <- ifelse(cleandata$Price >= (100 - input$threshold), 30000, 3000)
    # } else {
    #  # radius <- cleandata[[Bedroom]] / max(cleandata[[Bedroom]]) * 30000
    #   radius <- 100
    # }
    colorBin
    binpal <- colorBin("RdYlBu", FinalData$Price, 6, pretty = FALSE,reverse = TRUE)
    leafletProxy("map", data = FinalData) %>%
      clearShapes() %>%
      addCircles(~Lng, ~Lat, radius=100, layerId=~Price, stroke=FALSE, fillOpacity=0.8, fillColor = ~binpal(Price)) %>%
      addLegend("bottomright", pal=binpal, values=~Price, title=colorBy,layerId="colorLegend") %>%
      setView(lng = FinalData[0]$Lng, lat = FinalData[0]$Lat, zoom = 11)
    
  })
  
  # Show a popup at the given location
  showZipcodePopup <- function(price, lat, lng) {
    selectedZip <- cleandata[which(cleandata$Price == price & cleandata$Lat == lat & cleandata$Lng == lng),]
    print(selectedZip)
    content <- as.character(tagList(
      tags$h4("Price:", dollar(as.integer(selectedZip$Price))),
      tags$strong(HTML(sprintf("%s, %s %s",
                               selectedZip$Price, selectedZip$state.x, selectedZip$zipcode
      ))), tags$br(),
      sprintf("Region: %s", selectedZip$Region), tags$br(),
      sprintf("Address: %s", selectedZip$Address), tags$br(),
      sprintf("Suburb: %s", selectedZip$suburb), tags$br(),
      sprintf("Post Code: %s", selectedZip$Postcode), tags$br(),
      sprintf("Council area: %s", selectedZip$CouncilArea), tags$br(),
      sprintf("Year Built: %s", selectedZip$YearBuilt)
    ))
    leafletProxy("map") %>% addPopups(lng, lat, content, layerId = price)
  }
  
  # When map is clicked, show a popup with city info
  observe({
    leafletProxy("map") %>% clearPopups()
    event <- input$map_shape_click
    if (is.null(event))
      return()
    
    isolate({
       showZipcodePopup(event$id, event$lat, event$lng)
    })
  })
  
  
  ## Data Explorer ###########################################
  observe({
    cities <- if (is.null(input$states)) character(0) else {
      filter(cleantable, State %in% input$states) %>%
        `$`('City') %>%
        unique() %>%
        sort()
    }
    stillSelected <- isolate(input$cities[input$cities %in% cities])
    updateSelectizeInput(session, "cities", choices = cities, 
                         selected = stillSelected, server = TRUE)
  })
  
  observe({
    cities <- if (is.null(input$states)) character(0) else {
      filter(cleantable, State %in% input$states) %>%
        `$`('City') %>%
        unique() %>%
        sort()
    }
    stillSelected <- isolate(input$cities[input$cities %in% cities])
    updateSelectizeInput(session, "cities", choices = cities, 
                         selected = stillSelected, server = TRUE)
  })
  
  observe({
    zipcodes <- if (is.null(input$states)) character(0) else {
      cleantable %>%
        filter(State %in% input$states,
               is.null(input$cities) | City %in% input$cities) %>%
        `$`('Zipcode') %>%
        unique() %>%
        sort()
    }
    stillSelected <- isolate(input$zipcodes[input$zipcodes %in% zipcodes])
    updateSelectizeInput(session, "zipcodes", choices = zipcodes,
                         selected = stillSelected, server = TRUE)
  })
  
  #pop-up function 
  observe({
    if (is.null(input$goto))
      print(input$goto)
      return()
    isolate({
      map <- leafletProxy("map")
      map %>% clearPopups()
      dist <- 0.5
      zip <- input$goto$zip
      lat <- input$goto$lat
      lng <- input$goto$lng
      showZipcodePopup(zip, lat, lng)
      map %>% fitBounds(lng - dist, lat - dist, lng + dist, lat + dist)
    
    })
  })
  
  output$ziptable <- DT::renderDataTable({
  df <- cleandata %>%
    mutate(Action = paste('<a class="go-map" href="" data-lat="',Lat, '" data-long="',Lng, '" data-zip="',Price, '"></a>', sep=""))
 DT::datatable(df, escape = FALSE)
  })
}