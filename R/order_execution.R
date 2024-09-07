#' Place an order
#'
#' @param symbol The stock symbol
#' @param quantity The number of shares to trade
#' @param orderType The type of order (e.g., "Market", "Limit")
#' @param price The limit price (required for Limit orders)
#' @param accessToken The TradeStation API access token
#'
#' @return A list containing order confirmation details
#' @export
placeOrder <- function(symbol, quantity, orderType, price = NULL, accessToken) {
  endpoint <- "https://api.tradestation.com/v2/orders"
  
  body <- list(
    Symbol = symbol,
    Quantity = quantity,
    OrderType = orderType,
    TradeAction = "BUY",  # Assuming a buy order, add parameter for sell orders
    Route = "Intelligent"
  )
  
  if (orderType == "Limit" && !is.null(price)) {
    body$LimitPrice <- price
  }
  
  response <- makeApiCall(endpoint, method = "POST", body = body, accessToken = accessToken)
  
  httr2::resp_body_json(response)
}

#' Cancel an existing order
#'
#' @param orderId The ID of the order to cancel
#' @param accessToken The TradeStation API access token
#'
#' @return A list containing cancellation confirmation details
#' @export
cancelOrder <- function(orderId, accessToken) {
  endpoint <- paste0("https://api.tradestation.com/v2/orders/", orderId)
  
  response <- makeApiCall(endpoint, method = "DELETE", accessToken = accessToken)
  
  httr2::resp_body_json(response)
}