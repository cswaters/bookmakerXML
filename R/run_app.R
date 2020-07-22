#' Run the Shiny Application
#'
#' @param ... A series of options to be used inside the app.
#'
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
run_app <- function(
  ...
) {
  with_golem_options(
    app = shiny::runGadget(
      app = app_ui, 
      server = app_server,
      viewer = shiny::paneViewer(500)
    ), 
    golem_opts = list(...)
  )
}
