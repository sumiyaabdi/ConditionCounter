#' Launch Shiny Application
#'
#' Connects to Eunomia database. Plots occurrences of selected conditions of interest.
#'
#' @returns Runs the shiny application
#' @export
launchShinyApp <- function() {

  shiny::runApp('inst/shiny', display.mode = "normal")
}
