#' Make an API call to TradeStation
#'
#' @param endpoint The API endpoint URL
#' @param method The HTTP method (GET, POST, DELETE)
#' @param query The query parameters (optional)
#' @param body The request body (optional)
#' @param accessToken The TradeStation API access token
#'
#' @return An httr2 response object
#' @keywords internal
makeApiCall <- function(endpoint, method = "GET", query = NULL, body = NULL, accessToken) {
  req <- httr2::request(endpoint) |>
    httr2::req_method(method) |>
    httr2::req_headers(
      Authorization = paste("Bearer", accessToken),
      "Content-Type" = "application/json"
    ) |>
    httr2::req_timeout(30)  # Set a timeout of 30 seconds
  
  if (!is.null(query)) {
    req <- req |> httr2::req_url_query(!!!query)
  }
  
  if (!is.null(body)) {
    req <- req |> httr2::req_body_json(body)
  }
  
  # Perform the request with automatic retries for rate limiting
  response <- req |>
    httr2::req_retry(
      max_tries = 3,
      backoff = ~ 10,
      is_transient = ~ httr2::resp_status(.x) == 429
    ) |>
    httr2::req_perform()
  
  # Check for errors
  httr2::resp_check_status(response)
  
  response
}