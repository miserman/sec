#' Download Company Information
#'
#' Retrieve information about companies.
#'
#' @param ciks A vector of Central Index Keys, or a \code{data.frame} with a \code{ciks}
#' column.
#' @param type Type of information to collect, between \code{submissions}, which includes things
#' like Standard Industrial Classification, ticker, and previous names, as well as information
#' about recent filings, and \code{facts}, including financial
#' @param out Path to a directory in which to save filings. Each filing will be saved
#' within a subdirectory: \code{{out}/{cik}/{SEC Accession Number}}.
#' @param load Logical; if \code{TRUE}, will read in the downloaded info file.
#' @param overwrite Logical; if \code{TRUE}, download the info file even if it already exists.
#' @param user User agent string (e.g., \code{"Company Name AdminContact@example.com"});
#' defaults to the \code{SEC_USER} environment variable, which can be set with
#' \code{\link{Sys.setenv}} (e.g., \code{Sys.setenv(SEC_USER = "...")}).
#' @param verbose Logical; if \code{FALSE}, will not show status updates.
#' @seealso Search for filings with \code{\link{sec_search}}.
#' @examples
#' \dontrun{
#'
#' # retrieve information about Pepsico
#' pepsi <- sec_companies("0000077476")
#' pepsi$`0000077476`[1:10]
#'
#' # retrieve financial information about Pepsico
#' pepsi_facts <- sec_companies("0000077476", "facts")
#' pepsi_facts$`0000077476`$facts$`us-gaap`$Revenues
#' }
#' @returns An invisible list with named entries for each successful CIK if \code{load} is \code{TRUE},
#' or a vector of HTTP status codes otherwise.
#' @export

sec_companies <- function(ciks, type = "submissions", out = tempdir(), load = TRUE, overwrite = FALSE,
                          user = Sys.getenv("SEC_USER"), verbose = TRUE) {
  if (is.data.frame(ciks)) ciks <- ciks$ciks
  ciks <- formatC(unique(ciks), width = 10, flag = 0)
  dir.create(out, FALSE, TRUE)
  out <- paste0(normalizePath(out, "/"), "/")
  type <- if (grepl("^s", tolower(type))) "submissions" else "facts"
  base_url <- paste0(
    "https://data.sec.gov/",
    if (type == "submissions") "submissions" else "api/xbrl/companyfacts",
    "/CIK"
  )
  user_agent <- httr::user_agent(user)
  n <- length(ciks)
  reqs <- structure(rep(200, n), names = ciks)
  res <- list()
  if (missing(verbose) && n == 1) verbose <- FALSE
  if (verbose) {
    cli_progress_step(
      "0 / {n} downloaded",
      msg_done = "finished; {sum(reqs == 200)} of {n} successful", spinner = TRUE
    )
  }
  for (i in seq_len(n)) {
    dir <- paste0(out, ciks[[i]], "/")
    dir.create(dir, FALSE, TRUE)
    file <- paste0(dir, type, ".json")
    if (overwrite || !file.exists(file)) {
      req <- httr::GET(paste0(base_url, ciks[[i]], ".json"), httr::write_disk(file), user_agent)
      if (req$status_code != 200) unlink(file)
      Sys.sleep(max(.001, .1 - req$times[["total"]]))
      reqs[i] <- req$status_code
    }
    if (load && file.exists(file)) {
      res[[ciks[[i]]]] <- jsonlite::read_json(file, simplifyVector = TRUE)
    }
    if (verbose) cli_progress_update()
  }
  invisible(if (load) res else reqs)
}
