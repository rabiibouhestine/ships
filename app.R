library(shiny)
library(shiny.semantic)
library(tidyverse)
library(leaflet)
library(geodist)

source("modules/select_ship.R")

ships <- read_csv("data/ships.csv", n_max = 10000)

shipIcon <- iconList(
    start = makeIcon("www/ship_start_icon.png", iconWidth = 50, iconHeight = 25),
    end = makeIcon("www/ship_end_icon.png", iconWidth = 60, iconHeight = 30)
)

ui <- semanticPage(
    theme = "cosmo",
    title = "Ships",
    div(class = "ui container grid",
        div(class = "six wide column",
            segment(class = "raised segment",
                    select_ship_ui("select_ship")
            ),
            segment(class = "raised segment",
                    h1(class = "ui header", "First header")
            )
        ),
        div(class = "ten wide column",
            segment(class = "raised segment",
                    textOutput(outputId = "selected_ship_name")
            ),
            segment(class = "raised segment",
                    leafletOutput("ship_path_map")
            )
        )
    )
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
                                      "Longitude: ", selected_ship_info()$LON, "<br>",
                                      "Time of observation: ", selected_ship_info()$DATETIME)) %>%
            addPolylines(lng = ~LON, lat = ~LAT,
                         weight = 2, dashArray = "5",
                         label = paste0("Distance: ", selected_ship_info()$dist_since_last_obs[1] , " m"),
                         labelOptions = labelOptions(permanent = TRUE))
    })



})

shinyApp(ui = ui, server = server)
