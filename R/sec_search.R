#' Search for Filings
#'
#' Search for forms by keywords, Central Index Keys, and/or Standard Industrial Classification
#' codes, filed after 2001.
#'
#' @param query A character with words or phrases to search for within the form
#' (e.g., \code{grape "fruit juice"}).
#' @param entity A character with a name, ticker, or Central Index Key.
#' @param CIKs A vector of Central Index Keys (e.g., \code{c(0000317540, 0000077476)}).
#' @param SICs A vector of Standard Industrial Classification codes (e.g., \code{2080}).
#' @param forms A vector of forms to include (e.g., \code{"10-K"}).
#' @param date_to Latest date to include results from (e.g., \code{"2019-01-01"}); defaults to current.
#' @param date_from Earliest date to include results from; defaults to the earliest available:
#' \code{"2001-01-01"}.
#' @param outFile Path of a file to write results to.
#' @param overwrite Logical; if \code{FALSE}, will read in an existing results file rather than
#' executing the search.
#' @param start Number of results to skip.
#' @param limit Maximum number of results to return, in steps of 100.
#' @param verbose Logical; if \code{FALSE}, will no display status messages.
#' @seealso To retrieve a simpler list of companies, see \code{\link{sec_search_companies}}.
#' @examples
#' \dontrun{
#'
#' # search for 10-K filings by beverage companies
#' results <- sec_search(SICs = 2080, forms = "10-K", limit = 100)
#' }
#' @returns A \code{data.frame} of results.
#' @export

sec_search <- function(query = NULL, entity = NULL, CIKs = NULL, SICs = NULL, forms = NULL, date_to = NULL,
                       date_from = "2001-01-01", outFile = NULL, overwrite = TRUE, start = 0, limit = Inf,
                       verbose = TRUE) {
  if (!overwrite && !is.null(outFile) && file.exists(outFile)) {
    return(read.csv(outFile))
  }
  if (!is.null(query) && !is.character(query)) {
    cli_abort("query must be a character; use other arguments to enter numeric CIKs or SICs")
  }
  body <- Filter(length, list(
    dateRange = "custom",
    q = query,
    entityName = as.character(entity),
    ciks = as.list(formatC(CIKs, width = 10, flag = 0)),
    sics = as.list(as.character(SICs)),
    startdt = date_to,
    enddt = date_from,
    forms = as.list(toupper(forms)),
    from = start
  ))
  req <- httr::POST(
    "https://efts.sec.gov/LATEST/search-index",
    body = body, encode = "json"
  )
  req$status_code
  res <- jsonlite::fromJSON(rawToChar(req$content))
  if (!is.null(res$errorMessage)) cli_abort("request failed\n  message: {res$errorType} {res$errorMessage}")
  if (res$hits$total$value == 0) cli_abort("no results found")
  if (verbose) {
    cli_alert_info(paste0(
      "found {res$hits$total$value} records", if (res$hits$total$value > limit) "; returning {limit}"
    ))
  }
  extract_hits <- function(res) {
    d <- res$hits$hits
    if (nrow(d)) {
      d[["_source"]]$id <- d[["_id"]]
      d <- d[["_source"]]
      d$items <- NULL
      do.call(rbind, lapply(seq_len(nrow(d)), function(r) {
        if (length(d[r, 1][[1]]) == 1) {
          do.call(cbind, lapply(d[r, ], as.character))
        } else {
          row <- lapply(d[r, ], function(x) lapply(x, function(v) if (length(v)) v else "")[[1]])
          if (length(row$biz_states) != length(row$biz_locations)) {
            row$biz_states <- sub("^.*, ", "", row$biz_locations)
          }
          do.call(cbind, row)
        }
      }))
    }
  }
  hits <- extract_hits(res)
  limit <- min(limit, res$hits$total$value)
  if (limit > nrow(hits)) {
    while (nrow(hits) < limit) {
      body$from <- nrow(hits)
      req <- httr::POST(
        "https://efts.sec.gov/LATEST/search-index",
        body = body, encode = "json"
      )
      if (req$status_code != 200) {
        if (verbose) cli_alert_warning("page {nrow(hits) / 100 + 1} failed: {rawToChar(req$content)}")
        break
      }
      res <- jsonlite::fromJSON(rawToChar(req$content))
      if (!nrow(res$hits$hits)) break
      hits <- rbind(hits, extract_hits(res))
    }
  }
  hits[hits == "character(0)"] <- ""
  hits <- as.data.frame(hits)
  if (!is.null(outFile)) write.csv(hits, outFile, row.names = FALSE)
  hits
}
