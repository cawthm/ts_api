#' Get real-time quote for a symbol
#'
#' @param symbol The stock symbol
#' @param accessToken The TradeStation API access token
#'
#' @return A list containing quote information
#' @export
getQuote <- function(symbol, accessToken) {
  endpoint <- paste0("https://api.tradestation.com/v2/data/quote/", symbol)
  
  response <- makeApiCall(endpoint, accessToken = accessToken)
  
  httr2::resp_body_json(response)
}

#' Get historical bar data for a symbol
#'
#' @param symbol The stock symbol
#' @param interval The bar interval (e.g., "1min", "5min", "1hour", "1day")
#' @param startDate The start date for historical data (YYYY-MM-DD)
#' @param endDate The end date for historical data (YYYY-MM-DD)
#' @param accessToken The TradeStation API access token
#'
#' @return A data frame containing historical bar data
#' @export
getBarData <- function(symbol, interval, startDate, endDate, accessToken) {
  endpoint <- paste0("https://api.tradestation.com/v2/stream/barchart/", symbol, "/", interval)
  
  response <- makeApiCall(
    endpoint,
    query = list(
      startDate = startDate,
      endDate = endDate,
      sessionTemplate = "Default"
    ),
    accessToken = accessToken
  )
  
  data <- httr2::resp_body_json(response)
  as.data.frame(data$Bars)
}