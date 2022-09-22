.onLoad <- function(lib, pkg) {
  if (Sys.getenv("SEC_USER") == "") {
    Sys.setenv(SEC_USER = paste(
      "Name", Sys.Date(), "contact@example.com"
    ))
  }
}

#' @importFrom httr GET user_agent write_disk POST
#' @importFrom jsonlite fromJSON
#' @importFrom cli cli_abort cli_progress_step cli_progress_update cli_alert_info cli_alert_warning
#' @importFrom utils read.csv write.csv
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
## usethis namespace: end
NULL
