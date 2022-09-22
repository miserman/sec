#' Search for Companies (Legacy)
#'
#' Search for companies based on Standard Industrial Classification.
#' @param SIC Standard Industrial Classification code (e.g., \code{2080}).
#' @param start Number of results to skip.
#' @param limit Maximum number of results to return, in steps of 100.
#' @param user User agent string (e.g., \code{"Company Name AdminContact@example.com"});
#' defaults to the \code{SEC_USER} environment variable, which can be set with
#' \code{\link{Sys.setenv}} (e.g., \code{Sys.setenv(SEC_USER = "...")}).
#' @param verbose Logical; if \code{FALSE}, will not show status updates.
#' @seealso For more complete results, see \code{\link{sec_search}}.
#' @examples
#' \dontrun{
#'
#' # get a list of beverage companies
#' sec_search_companies(2080)
#' }
#' @returns A \code{data.frame} of results, with columns for \code{CIK} (Central Index Key),
#' \code{Company}, and \code{Location}.
#' @export

sec_search_companies <- function(SIC, start = 0, limit = Inf, user = Sys.getenv("SEC_USER"), verbose = TRUE) {
  base_url <- paste("https://www.sec.gov/cgi-bin/browse-edgar?SIC=")
  count <- min(100, limit)
  extract_table <- function(req) {
    d <- rawToChar(req$content)
    tab <- regmatches(d, regexec("<table.*</table>", d))[[1]]
    if (length(tab)) {
      rows <- strsplit(tab, "<tr[^>]*>")[[1]][-(1:2)]
      p <- as.data.frame(do.call(rbind, lapply(strsplit(rows, "\\s*<[^>]+>"), "[", c(3, 6, 9))))
      colnames(p) <- c("CIK", "Company", "Location")
      p
    }
  }
  user_agent <- httr::user_agent(user)
  req <- httr::GET(
    paste0(base_url, SIC, "&start=", start, "&count=", count),
    user_agent
  )
  res <- NULL
  if (req$status_code == 200) {
    res <- extract_table(req)
    if (nrow(res) < limit && nrow(res) >= 100) {
      while (nrow(res) < limit) {
        req <- httr::GET(
          paste0(base_url, SIC, "&start=", nrow(res), "&count=100"),
          user_agent
        )
        if (req$status_code != 200) {
          if (verbose) cli_alert_warning("page {nrow(res) / 100 + 1} failed: {rawToChar(req$content)}")
          break
        }
        res2 <- extract_table(req)
        if (!length(res2)) break
        res <- rbind(res, res2)
      }
    }
  } else {
    stop("request failed: ", rawToChar(req$content), call. = FALSE)
  }
  res
}
