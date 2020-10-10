# loading packages
library(shiny)
library(shiny.semantic)
library(tidyverse)
library(leaflet)
library(geodist)
# importing modules
source("modules/select_ship.R")
# reading data
ships <- read_csv("data/ships.csv", n_max = 100000)
# importing ship icons
shipIcon <- iconList(
    start = makeIcon("ship_start_icon.png", iconWidth = 50, iconHeight = 25),
    end = makeIcon("ship_end_icon.png", iconWidth = 60, iconHeight = 30)
)



# app semantic ui
ui <- semanticPage(
    title = "Ships",
    div(class = "ui container grid",
        div(class = "row",
            div(class = "sixteen wide column",
                div(class="ui horizontal divider", h1("Vessel Dashboard"))
            )
        ),
        div(class = "row",
            div(class = "five wide column",
                segment(class = "raised segment",
                        select_ship_ui("select_ship"),
                        br(),
                        uiOutput("selected_ship_card")
                )
            ),
            div(class = "eleven wide column",
                segment(class = "raised segment",
                        p(strong("Longest sailed distance between two consecutive observations:")),
                        textOutput(outputId = "selected_ship_name")
                ),
                segment(class = "raised segment",
                        leafletOutput("ship_path_map", height = 328)
                )
            )
        ),
        div(class = "row",
            div(class = "sixteen wide column",
                uiOutput("data_span_note")
            )
        )
    )
)

# app server logic
server <- shinyServer(function(input, output, session) {
    # Get the relevant information from the module select_ship
    selected_ship_info <- callModule(module = select_ship, id = "select_ship", data = ships)

    # Render Ship Card
    output$selected_ship_card <- renderUI(
        card(
            div(class = "content",
                div(class = "header", icon("ship"), selected_ship_info()$SHIPNAME[1]),
                div(class = "meta", paste0("Type: ", selected_ship_info()$ship_type[1])),
                div(class = "description",
                    h3(strong("Vessel details:")),
                    strong("ID: "), selected_ship_info()$SHIP_ID[1], br(),
                    strong("Flag: "), selected_ship_info()$FLAG[1], br(),
                    strong("Width: "), selected_ship_info()$WIDTH[1], " m", br(),
                    strong("Length: "), selected_ship_info()$LENGTH[1], " m", br(),
                    strong("Average Speed: "), round(selected_ship_info()$average_speed[1],2), " kn", br(),
                    strong("Deadweight Tonnage: "), selected_ship_info()$DWT[1], " tonne", br(),
                    strong("Total Traveled Distance: "), selected_ship_info()$total_traveled_distance[1], " m"
                )
            )
        )
    )

    # Render Vessel Longest Distance Note
    output$selected_ship_name <- renderText(
        paste0(selected_ship_info()$SHIPNAME[1],
               " sailed for a distance of ",
               selected_ship_info()$dist_since_last_obs[1],
               " m between ",
               selected_ship_info()$DATETIME[2],
               " and ",
               selected_ship_info()$DATETIME[1],
               "."
        )
    )

    # Render map
    output$ship_path_map <- renderLeaflet({
        leaflet(data = selected_ship_info()) %>%
            addTiles() %>%
            addMarkers(lng = ~LON, lat = ~LAT,
                       icon = ~shipIcon[position],
                       popup = ~paste0("Position: ", position, br(),
                                       "Latitude: ", LAT, "<br>",
                                       "Longitude: ", LON, "<br>",
                                       "Time of observation: ", DATETIME)) %>%
            addPolylines(lng = ~LON, lat = ~LAT,
                         weight = 2, dashArray = "5",
                         label = paste0("Distance: ", selected_ship_info()$dist_since_last_obs[1] , " m"),
                         labelOptions = labelOptions(permanent = TRUE)) %>%
            addControl(html = "<img src='ship_start_icon.png' style='width:40px;height:20px;'> Start position<br/>
                               <img src='ship_end_icon.png' style='width:40px;height:20px;'> End position",
                       position = "topright") %>%
            addControl(html = "Click on vessel for more information",
                       position = "bottomleft")
    })

    # Render Data Span Note
    output$data_span_note <- renderUI(
        message_box(class = "icon",
                    icon_name = "info circle",
                    header = "Note:",
                    content = paste0(
                        "The observations in the data that has been used in the making of this dahboard span from ",
                        min(ships$DATETIME),
                        " to ",
                        max(ships$DATETIME)
                    )
        )
    )


})
# run app
shinyApp(ui = ui, server = server)
