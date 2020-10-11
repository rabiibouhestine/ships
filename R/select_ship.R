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
    get_longest_dist_obs(data, input$select_ship_name)
  })

  return(selected_ship_info)
}
