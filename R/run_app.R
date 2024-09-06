#' Run the traceShiny shiny app
#'
#' @export
run_traceShiny <- function() {
  app_dir <- system.file("app", package = "traceShiny")
  if (app_dir == "") {
    stop(
      "Could not find the app directory. Try re-installing `traceShiny`.",
      call. = FALSE
    )
  }
  shiny::shinyAppDir(appDir = app_dir)
}
