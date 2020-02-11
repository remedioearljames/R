library(leaflet)
library(DataCombine)
library(rgl)
library(ggplot2)
library(gganimate)
library(hrbrthemes)
library(viridis)


# Choices for drop-downs
vars <- c(
  "Price" = "Price",
  "Land Size" = "Landsize",
  "Median income" = "income",
  "Population" = "adultpop"
)

regionlist <- c(
  "Eastern" = "superzip",
  "Land Size" = "centile",
  "House Hold Income" = "college",
  "Median income" = "income",
  "Population" = "adultpop"
)

 
navbarPage("Melbourne Housing", id="nav",

  tabPanel("Interactive map",
    div(class="outer",
    
      tags$head(
        # Include our custom CSS
        includeCSS("styles.css"),
        includeScript("gomap.js")
      ),
      
      # If not using custom CSS, set height of leafletOutput to a number instead of percent
      leafletOutput("map", width="100%", height="100%"),

      # Shiny versions prior to 0.11 should use class = "modal" instead.
      absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
        draggable = FALSE, top = 60, left = "auto", right = 20, bottom = "auto",
        width = 330, height = "auto",

        h2("Filter"),
        
        # Input: Specification of range within an interval ----
        sliderInput("rangePrice", "Price Range",
                    min = 1, max = 6000000,
                    value = c(0,6000000)),
        textInput("rooms", label = h4("Bedrooms"), placeholder = "Enter text..."),
        textInput("bathrooms", label = h4("Bathrooms"), placeholder = "Enter text..."),
          #textInput("cars", label = h4("Car Parking"), placeholder = "Enter text..."),
        #selectInput("color", "Legend", vars),
        selectInput("regions", "Region", c("All Region"="",as.vector(cleandata$Region)), selected = NULL), 
        #selectInput("size", "Variable", vars, selected = "adultpop"),
        # conditionalPanel("input.color == 'superzip' || input.size == 'superzip'",
          # Only prompt for threshold when coloring or sizing by superzip
          # numericInput("threshold", "SuperZIP threshold (top n percentile)", 5)
        # ),
        
      ),
      tags$div(id = 'plots',
               
               plotOutput("histCentile", height = 400, width = 500),
           #    plotOutput("scatterCollegeIncome", height = 250, width = 300),
       )
    )
  ),

  
   tabPanel("Data explorer",
    hr(),
    DT::dataTableOutput("ziptable")
  ),

  conditionalPanel("false", icon("crosshair"))
)
