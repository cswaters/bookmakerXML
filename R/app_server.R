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
    output$download <- shiny::downloadHandler(
      filename = function() {
        f_name <- bm_mk_fname(input$desc)
      },
      content = function(file) {
        utils::write.csv(
          x = bm_xml_parse_pipeline(odds(),
                                    input$bet_type,
                                    shiny::req(input$desc)),
          file = file,
          row.names = FALSE
        )
      }
    )
    output$tbl <- reactable::renderReactable({
      grp_by <- ifelse(input$bet_type == "futures", "dt",
                       c("period", "dt"))
      bm_mk_reactable_tbl(df = bm_tbl_ouput_pipeline(
        shiny::req(odds()),
        input$bet_type,
        shiny::req(input$desc)
      ),
      grp_by = grp_by)
      
    })
    
    
  })
  shiny::observeEvent(input$done, {
    shiny::stopApp()
  })
}
