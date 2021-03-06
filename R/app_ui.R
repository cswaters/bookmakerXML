#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # List the first level UI elements here 
    miniUI::miniPage(
      miniUI::gadgetTitleBar("Bookmaker Odds"),
      miniUI::miniContentPanel(
        miniUI::miniButtonBlock(
          shiny::actionButton("click", "Get odds"),
          
          mod_download_button_ui("download_button_ui_1")
          ),
        shiny::radioButtons(
          "bet_type",
          "",
          choiceNames = c("Matchups", "Futures"),
          choiceValues = c("matchups", "futures"),
          selected = "matchups",
          inline = TRUE
        ),
        shiny::uiOutput("league_drpdwn"),
        mod_reactable_odds_ui("reactable_odds_ui_1")
      )
    )
  )
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
 
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'bookmakerXML'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}

