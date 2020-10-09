select_ship_ui <- function(id) {
  ns <- NS(id)

  div(dropdown_input(input_id = ns("select_ship_type"), choices = c()),
      dropdown_input(input_id = ns("select_ship_name"), choices = c())
  )
}

select_ship <- function(input, output, session, data) {

  update_dropdown_input(session, "select_ship_type", choices = unique(data$ship_type))

  observe({

    selected_type_data <- data %>% filter(ship_type == input$select_ship_type)

    update_dropdown_input(session, "select_ship_name", choices = unique(selected_type_data$SHIPNAME))
  })

  selected_ship_info <- reactive({
    req(input$select_ship_name)

    data %>%
      filter(SHIPNAME == input$select_ship_name) %>%
      arrange(desc(DATETIME)) %>%
      mutate(dist_since_last_obs = round(c(geodist(., sequential = TRUE, measure = "geodesic"),0),0)) %>%
      filter(row_number() %in% c(which.max(dist_since_last_obs == max(dist_since_last_obs)),
                                 which.max(dist_since_last_obs == max(dist_since_last_obs))+1)) %>%
      mutate(position = c("end", "start"))
  })

  return(selected_ship_info)
}
