#' reactable_odds UI Function
#'
#' @description View odds in reactable table.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList 
#' @importFrom shinycssloaders withSpinner
#' @importFrom reactable reactableOutput 
mod_reactable_odds_ui <- function(id) {
  ns <- NS(id)
  tagList(shinycssloaders::withSpinner(reactable::reactableOutput(ns("tbl")),
                                       type = 6))
}

#' reactable_odds Server Function
#'
#' @noRd
#' 
#' @importFrom reactable renderReactable
#' @importFrom shiny moduleServer req
#' 
mod_reactable_odds_server <- function(id, odds, bet_type, desc) {
  moduleServer(id, function(input, output, session) {
    output$tbl <- reactable::renderReactable({
      grp_by <- ifelse(bet_type() == "futures", "dt",
                       c("period", "dt"))
      bm_mk_reactable_tbl(df = bm_tbl_ouput_pipeline(shiny::req(odds()),
                                                     bet_type(),
                                                     shiny::req(desc())),
                          grp_by = grp_by)
      
    })
  })
  
}

## To be copied in the UI
# mod_reactable_odds_ui("reactable_odds_ui_1")

## To be copied in the server
# callModule(mod_reactable_odds_server, "reactable_odds_ui_1")
