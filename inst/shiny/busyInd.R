# Set up a button to have an animated loading indicator and a checkmark
# for better user experience
# Need to use with the corresponding `withBusyIndicator` server function
withBusyIndicatorUI <- function(button) {
  id <- button[['attribs']][['id']]
  div(
    `data-for-btn` = id,
    button,
    span(
      class = "btn-loading-container",
      shinyjs::hidden(
        icon("spinner", class = "btn-loading-indicator fa-spin"),
        icon("check", class = "btn-done-indicator")
      )
    ),
    shinyjs::hidden(
      div(class = "btn-err",
          div(icon("exclamation-circle"),
              tags$b("Error: "),
              span(class = "btn-err-msg")
          )
      )
    )
  )
}

## Call this function from the server with the button id that is clicked and the
## expression to run when the button is clicked
withBusyIndicatorServer <- function(buttonId,
                                    session,
                                    expr) {
  ns <- session$ns
  ## Show the "busy" message, hide the other messages, disable the button
  loadingEl <- sprintf("[data-for-btn=%s] .btn-loading-indicator", ns(buttonId))
  doneEl <- sprintf("[data-for-btn=%s] .btn-done-indicator", ns(buttonId))
  errEl <- sprintf("[data-for-btn=%s] .btn-err", ns(buttonId))
  shinyjs::disable(buttonId, asis = TRUE)
  shinyjs::show(selector = loadingEl, asis = TRUE)
  shinyjs::hide(selector = doneEl, asis = TRUE)
  shinyjs::hide(selector = errEl, asis = TRUE)
  on.exit({
    shinyjs::enable(buttonId, asis = TRUE)
    shinyjs::hide(selector = loadingEl, asis = TRUE)
  })
  ## Try to run the code when the button is clicked and show an error message if
  ## an error occurs or a success message if it completes
  tryCatch({
    value <- expr
    shinyjs::show(selector = doneEl)
    shinyjs::delay(2000, shinyjs::hide(selector = doneEl, anim = TRUE,
                                       animType = "fade", time = 0.5,
                                       asis = TRUE))
    value
  }, error = function(err) {
    errorFunc(err, buttonId, session)
  })
}

## When an error happens after a button click, show the error
errorFunc <- function(err,
                      buttonId,
                      session) {
  ns <- session$ns
  errEl <- sprintf("[data-for-btn=%s] .btn-err", ns(buttonId))
  errElMsg <- sprintf("[data-for-btn=%s] .btn-err-msg", ns(buttonId))
  errMessage <- gsub("^ddpcr: (.*)", "\\1", err$message)
  shinyjs::html(html = errMessage, selector = errElMsg)
  shinyjs::show(selector = errEl, anim = TRUE, animType = "fade", asis = TRUE)
}
