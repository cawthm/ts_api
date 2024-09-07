#' Authenticate with TradeStation API
#'
#' @param clientId Your TradeStation API client ID
#' @param clientSecret Your TradeStation API client secret
#' @param redirectUri The redirect URI specified in your TradeStation API application
#'
#' @return A list containing the access token and refresh token
#' @export
authenticateTradeStation <- function(clientId, clientSecret, redirectUri) {
  authUrl <- "https://api.tradestation.com/v2/authorize"
  tokenUrl <- "https://api.tradestation.com/v2/security/authorize"
  
  # Create the OAuth client
  client <- httr2::oauth_client(
    id = clientId,
    secret = clientSecret,
    token_url = tokenUrl, 
    name = "tradestation"
  )
  
  # Perform the OAuth flow
  token <- httr2::oauth_flow_auth_code(
    client = client,
    auth_url = authUrl,
    scope = "ReadAccount Trade Matrix",
    redirect_uri = redirectUri
  )
  
  list(
    accessToken = token$access_token,
    refreshToken = token$refresh_token
  )
}

#' Refresh TradeStation API access token
#'
#' @param refreshToken The refresh token obtained from authenticateTradeStation
#' @param clientId Your TradeStation API client ID
#' @param clientSecret Your TradeStation API client secret
#'
#' @return A list containing the new access token and refresh token
#' @export
refreshToken <- function(refreshToken, clientId, clientSecret) {
  tokenUrl <- "https://api.tradestation.com/v2/security/authorize"
  
  response <- httr2::request(tokenUrl) |>
    httr2::req_body_form(
      grant_type = "refresh_token",
      refresh_token = refreshToken,
      client_id = clientId,
      client_secret = clientSecret
    ) |>
    httr2::req_perform()
  
  if (httr2::resp_is_error(response)) {
    httr2::resp_check_status(response)
  }
  
  content <- httr2::resp_body_json(response)
  list(
    accessToken = content$access_token,
    refreshToken = content$refresh_token
  )
}