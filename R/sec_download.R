#' Download Filings
#'
#' Download SEC filings based on Central Index Key (CIK) and document ID (as returned
#' from \code{\link{sec_search}}.
#'
#' @param ciks A vector of Central Index Keys, or a \code{data.frame} with a \code{ciks}
#' column.
#' @param out Path to a directory in which to save filings. Each filing will be saved
#' within a subdirectory: \code{{out}/{cik}/{SEC Accession Number}}.
#' @param ids A vector of document IDs. These are combinations of SEC Accession Numbers
#' and file names, separated by a colon (e.g., \code{"0001564590-22-032043:gs-424b2.htm"}).
#' If the file name is not included, this will default to the complete submission text file
#' (e.g., \code{"0001564590-22-032043.txt"}).
#' @param complete Logical; if \code{TRUE}, will download the complete submission text file
#' rather than another files specified after a colon in \code{ids}.
#' @param user User agent string (e.g., \code{"Company Name AdminContact@example.com"});
#' defaults to the \code{SEC_USER} environment variable, which can be set with
#' \code{\link{Sys.setenv}} (e.g., \code{Sys.setenv(SEC_USER = "...")}).
#' @param verbose Logical; if \code{FALSE}, will not show status updates.
#' @seealso Search for filings with \code{\link{sec_search}}.
#' @examples
#' \dontrun{
#'
#' # search for filings based on SIC
#' forms <- sec_search(SICs = 2080, forms = "10-K", limit = 100)
#'
#' # download some of those filings
#' sec_download(forms[1:3, ], out = tempdir())
#' }
#' @returns An invisible vector of HTTP status codes.
#' @export

sec_download <- function(ciks, out, ids = NULL, complete = FALSE, user = Sys.getenv("SEC_USER"), verbose = TRUE) {
  if (is.data.frame(ciks)) {
    if (is.null(ids)) ids <- ciks$id
    ciks <- ciks$ciks
  }
  ciks <- formatC(unique(ciks), width = 10, flag = 0)
  if (missing(out)) cli_abort("{.arg out} must be specified")
  if (is.null(ids)) cli_abort("{.arg ids} must be specified if {.arg ciks} is not a data.frame containing it")
  dir.create(out, FALSE, TRUE)
  out <- paste0(normalizePath(out, "/"), "/")
  base_url <- "https://www.sec.gov/Archives/edgar/data/"
  user_agent <- httr::user_agent(user)
  n <- length(ciks)
  reqs <- structure(rep(200, n), names = ciks)
  if (verbose) {
    cli_progress_step(
      "0 / {n} downloaded",
      msg_done = "finished; {sum(reqs == 200)} of {n} successful", spinner = TRUE
    )
  }
  for (i in seq_len(n)) {
    id <- strsplit(ids[[i]], ":", fixed = TRUE)[[1]]
    if (complete || length(id) < 2) {
      id[2] <- id[1]
      if (!grepl("-", id[2])) {
        id[2] <- paste0(
          substr(id[2], 1, 10), "-", substr(id[2], 11, 12), "-", substr(id[2], 12, 18)
        )
      }
      id[2] <- paste0(id[2], ".txt")
    }
    dir <- paste0(out, ciks[[i]], "/", id[1], "/")
    dir.create(dir, FALSE, TRUE)
    file <- paste0(dir, id[2])
    if (!file.exists(file)) {
      req <- httr::GET(paste0(
        base_url, ciks[[i]], "/", gsub("-", "", id[1], fixed = TRUE), "/", id[2]
      ), httr::write_disk(file), user_agent)
      if (req$status_code != 200) unlink(file)
      Sys.sleep(max(.001, .1 - req$times[["total"]]))
      reqs[i] <- req$status_code
    }
    if (verbose) cli_progress_update()
  }
  invisible(reqs)
}
