#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shiny)
library(magrittr)
# library(rvest)
# library(readxl)
library(dplyr)
library(maps)
# library(reshape2)
#install.packages("ggiraph")
library(ggiraph)
library(RColorBrewer)
library(geojsonio)
#install.packages("plotly")
#library(plotly)
#install.packages("shinyWidgets")

#library(shinyWidgets)
#install.packages("shinydashboard")
library(shinydashboard)
library(shinythemes)
library(leaflet)
library(reshape2)
library(ggplot2)

library(tidyverse)
#install.packages("vroom")
library(vroom)
# library(sp)
library(sf)
# library(tigris)
library(leaflet)
library(htmlwidgets)
library(shiny)
library(shinythemes)
library(htmltools)
library(raster)

labels <- sprintf(
    "<strong>%s</strong>",
    spdf$DESIG_ENG) %>%
    lapply(htmltools::HTML)
# pal = colorBin("Spectral", domain = areal_June21_1$Ag_Ad_R,bins = c(0,10,20,50,100,300,600,1000,2000,4000,6000),pretty = FALSE, reverse = TRUE)
map_interactive <- spdf %>%
    leaflet() %>%
    addTiles() %>%
    addPolygons(color = "red",
                weight = 0.5,
                label = labels,
                #stroke = FALSE,
                #smoothFactor = 0.5,
                opacity = 1,
                fillOpacity = 0.5,
                # fillColor = ~pal(Ag_Ad_R),
                highlightOptions = highlightOptions(weight = 1,
                                                    fillOpacity = 0.5,
                                                    color = "black",
                                                    opacity = 0.5,
                                                    bringToFront = TRUE),
                labelOptions = labelOptions(
                    style =
                        list(
                            "font-weight" = "normal",
                            padding = "3px 8px"
                        ),
                    textsize = "15px", direction = "auto"
                ))

labels1 <- sprintf(
    "<strong>%s</strong><br>%s<strong>",
    spdff$Name.y,spdff$Time.y) %>%
    lapply(htmltools::HTML)

# pal = colorBin("Spectral", domain = areal_June21_1$Ag_Ad_R,bins = c(0,10,20,50,100,300,600,1000,2000,4000,6000),pretty = FALSE, reverse = TRUE)
map_interactive1 <- spdff %>%
    leaflet() %>%
    addTiles() %>%
    addPolygons(color = "orange",
                weight = 0.5,
                label = labels1,
                #stroke = FALSE,
                #smoothFactor = 0.5,
                opacity = 1,
                fillOpacity = 0.5,
                # fillColor = ~pal(Ag_Ad_R),
                highlightOptions = highlightOptions(weight = 1,
                                                    fillOpacity = 0.5,
                                                    color = "black",
                                                    opacity = 0.5,
                                                    bringToFront = TRUE),
                labelOptions = labelOptions(
                    style =
                        list(
                            "font-weight" = "normal",
                            padding = "3px 8px"
                        ),
                    textsize = "15px", direction = "auto"
                ))

# pal <- colorNumeric(c("#0C2C84", "#41B6C4", "#FFFFCC"), values(DEMM[[1]]),
#                     na.color = "transparent")
# Jan <- leaflet() %>% addTiles() %>% addRasterImage(DEM1, colors = colorNumeric(c("#0C2C84", "#41B6C4", "#FFFFCC"), values(DEM1),
#                                                 na.color = "transparent"), opacity = 0.8)  %>%
#     addLegend(pal = colorNumeric(c("#0C2C84", "#41B6C4", "#FFFFCC"), values(DEM1),
#                                  na.color = "transparent"), values = values(DEM1),
#               title = "Habitat Suitability")

ui <- bootstrapPage(
    tags$head(includeHTML("gtag.html")),
    navbarPage(theme = shinytheme("flatly"), collapsible = TRUE,
               HTML('<a style="text-decoration:none;cursor:default;color:#FFFFFF;" class="active" href="#">Save Whales</a>'), id="nav",
               windowTitle = "Save Whales",
               tabPanel("Marine Protected Areas",
                        div(class="outer",
                            tags$head(includeCSS("styles.css")),
                            leafletOutput("map", width="100%", height="100%"))),
               
               tabPanel("Slow Zones",
                        div(class="outer",
                            tags$head(includeCSS("styles.css")),
                            leafletOutput("map1", width="100%", height="100%")))
               # tabPanel("Habitat Suitability)",
               #          div(class="outer",
               #              tags$head(includeCSS("styles.css")),
               #              leafletOutput("Jan", width="100%", height="100%")))
               # )
)
)

server <- function(input, output, session){
    output$map <- renderLeaflet({ 
        map_interactive
    })
    output$map1 <- renderLeaflet({
        map_interactive1
    })
    # month <- reactive({
        # m <- lapply(DEMM, function(x) {x[c("Janaury")]})
        # return(m)
    # })
    # output$Jan <- renderLeaflet({
    #     Jan
    # })
}

# Run the application 
shinyApp(ui = ui, server = server)
