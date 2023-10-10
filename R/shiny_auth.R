# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

library(shiny)


shiny_auth_verify_code <- function(params, info) {
  verify_result <- list(verify=FALSE, error=FALSE, error_message='')
  if(!is.null(params$code)){
    h <- c(info$application_id, info$application_token)
    names(h) <- c('X-Application-Id', 'X-Application-Token')

    # sanitize code
    code <- stringr::str_replace_all(params$code, " ", "")

    # handle error for httr
    tryCatch(
      {
        respons <- httr::POST(
          info$info_url,
          body = list(code = params$code),
          add_headers(.headers = h),
          encode = "json"
        )
        result <- httr::content(respons)

        if(isTRUE(result$result) && isFALSE(result$expired) && isTRUE(result$authorized)) {
          verify_result$verify <- TRUE
        }
      },
      error = function(e) {
        # Handle the error here
        # For example, you can print an error message or log it
        cat("Error during HTTP request:", conditionMessage(e), "\n")
        verify_result$error <- TRUE
        verify_result$error_message <- 'An error occurred while connecting to the server. Please try again later or contact support if the issue persists.'
      }
    )

  }
  return(verify_result)
}


shiny_auth_secured_ui <- function(ui, info) {
  function(req) {
    verify_result <- shiny_auth_verify_code(shiny::parseQueryString(req$QUERY_STRING), info)
    if (!verify_result$verify) {
      if(verify_result$error) {
        p(verify_result$error_message)
      } else {
        url <- info$login_url
        redirect <- sprintf("location.replace(\"%s\");", url)
        shiny::tags$script(shiny::HTML(redirect))
      }
    } else {
      if (is.function(ui)) {
        ui(req)
      } else {
        ui
      }
    }
  }
}
