#' download_button UI Function
#'
#' @description Download displayed odds.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList downloadButton
mod_download_button_ui <- function(id){
  ns <- NS(id)
  tagList(
    shiny::downloadButton(ns("download"), "Download")
  )
}
    
#' download_button Server Function
#'
#' @noRd 
#' 
#' @importFrom shiny moduleServer downloadHandler req
#' 
mod_download_button_server <- function(id, odds, bet_type, desc){
  
  moduleServer(id, function(input, output, session){
    output$download <- shiny::downloadHandler(
      filename = function() {
        f_name <- bm_mk_fname(desc())
      },
      content = function(file) {
        utils::write.csv(
          x = bm_xml_parse_pipeline(odds(),
                                    bet_type(),
                                    shiny::req(desc())),
          file = file,
          row.names = FALSE
        )
      }
    )
  })
}
    
## To be copied in the UI
# mod_download_button_ui("download_button_ui_1")
    
## To be copied in the server
# callModule(mod_download_button_server, "download_button_ui_1")
 
