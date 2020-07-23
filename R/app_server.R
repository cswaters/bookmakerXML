#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  # List the first level callModules here
  shiny::observeEvent(input$click, {
    odds <- shiny::reactive(bm_get_odds_xml())
    descriptions <- shiny::reactive(odds() %>%
                                      bm_filter_by_btype(input$bet_type) %>%
                                      bm_get_desc())
    
    output$league_drpdwn <-
      shiny::renderUI(shiny::selectizeInput("desc",
                                            "Select sport",
                                            choices = descriptions()))
    bet_type <- reactive(input$bet_type)
    desc <- reactive(input$desc)
    
    # Download odds button
    mod_download_button_server("download_button_ui_1", odds, bet_type, desc)
    # Output odds to reactable table
    mod_reactable_odds_server("reactable_odds_ui_1",
                              odds, 
                              bet_type,
                              desc)
    
  })
  shiny::observeEvent(input$done, {
    shiny::stopApp()
  })
}
