#' Run the interactive analysis tool (Shiny app) in a web browser
#'
#' In addition to the functions provided in this package, the \code{modoma}
#' package also provides an interactive tool. The tool will be launched in a
#' web browser.
#' @export
launch <- function() {
  shiny::runApp(system.file("shiny", package = "modoma"),
                display.mode = "normal",
                launch.browser = TRUE)
}
