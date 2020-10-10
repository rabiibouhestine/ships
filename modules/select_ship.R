# select ship module ui
select_ship_ui <- function(id) {
  ns <- NS(id)

  div(p(strong("Select vessel type:")),
      dropdown_input(input_id = ns("select_ship_type"), choices = c()),
      br(),
      p(strong("Select vessel name:")),
      dropdown_input(input_id = ns("select_ship_name"), choices = c())
  )
}

# select ship module server logic
select_ship <- function(input, output, session, data) {

  update_dropdown_input(session, "select_ship_type", choices = unique(data$ship_type))

  observe({
    selected_type_data <- data %>% filter(ship_type == input$select_ship_type)

    update_dropdown_input(session, "select_ship_name", choices = unique(selected_type_data$SHIPNAME))
  })

  selected_ship_info <- reactive({
    req(input$select_ship_name)

    data %>%
      # keep only the selected ship
      filter(SHIPNAME == input$select_ship_name) %>%
      # sort by observation date time
      arrange(desc(DATETIME)) %>%
      # calculate sequential traveled distance using geodist to get distance between each two consecutive observations
      mutate(dist_since_last_obs = round(c(geodist(., sequential = TRUE, measure = "geodesic"),0),0),
             # calculate total traveled distance
             total_traveled_distance = sum(dist_since_last_obs),
             # calculate average speed
             average_speed = mean(SPEED)) %>%
      # keep only the two observations relevant to the longest traveled distance
      filter(row_number() %in% c(which.max(dist_since_last_obs == max(dist_since_last_obs)),
                                 which.max(dist_since_last_obs == max(dist_since_last_obs))+1)) %>%
      # create position variable to help distinguishing the two observations
      mutate(position = c("end", "start"))
  })

  return(selected_ship_info)
}
