library(shiny)
library(shiny.semantic)
library(tidyverse)
library(leaflet)
library(geodist)

source("modules/select_ship.R")

ships <- read_csv("data/ships.csv", n_max = 10000)

shipIcon <- iconList(
    start = makeIcon("www/ship_start_icon.png", iconWidth = 60, iconHeight = 30),
    end = makeIcon("www/ship_end_icon.png", iconWidth = 60, iconHeight = 30)
)

ui <- semanticPage(
    title = "Ships",
    select_ship_ui("select_ship"),
    textOutput(outputId = "selected_ship_name"),
    leafletOutput("ship_path_map")
)


server <- shinyServer(function(input, output, session) {

    selected_ship_info <- callModule(module = select_ship, id = "select_ship", data = ships)

    output$selected_ship_name <- renderText(paste0("current selection: ",
                                                   selected_ship_info()$SHIPNAME[1],
                                                   " max distance: ",
                                                   selected_ship_info()$dist_since_last_obs[1],
                                                   " m"))

    output$ship_path_map <- renderLeaflet({
        leaflet(data = selected_ship_info()) %>%
            addTiles() %>%
            addMarkers(lng = ~LON, lat = ~LAT,
                       icon = ~shipIcon[position],
                       popup = paste0("Position: ", selected_ship_info()$position, "<br>",
                                      "Latitude: ", selected_ship_info()$LAT, "<br>",
                                      "Longitude: ", selected_ship_info()$LON, "<br>")) %>%
            addPolylines(lng = ~LON, lat = ~LAT,
                         weight = 3, dashArray = "5",
                         label = paste0("Distance: ", selected_ship_info()$dist_since_last_obs[1] , " m"),
                         labelOptions = labelOptions(permanent = TRUE))
    })



})

shinyApp(ui = ui, server = server)
