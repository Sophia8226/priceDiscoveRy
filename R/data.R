#' One week of one-second S&P 500 futures and spot prices
#'
#' An example data set containing synchronized S&P 500 futures and spot-price
#' observations at one-second frequency over one week. The timestamps represent
#' New York local time.
#'
#' @format A `data.table` and `data.frame` with 590400 rows and 5 variables:
#' \describe{
#'   \item{date}{A character string giving the calendar date in
#'     `"YYYY-MM-DD"` format.}
#'   \item{time}{A character string giving the New York local clock time in
#'     `"HH:MM:SS"` format.}
#'   \item{datetime}{A character string combining `date` and `time` in
#'     `"YYYY-MM-DD HH:MM:SS"` format.}
#'   \item{V1}{A numeric S&P 500 futures-price series.}
#'   \item{V2}{A numeric S&P 500 spot-index series before application of the
#'     spot-price multiplier.}
#' }
#'
#' @source Data provided by the thesis supervisor for thesis research and
#'   package testing.
"example_sp500_futures_spot_oneweek_24h_1sec"
