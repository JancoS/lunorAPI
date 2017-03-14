#' luno_limit_order
#'
#' @param ticker_pair
#' @param order_type
#' @param volume_bitcoin
#' @param price_pair_currency
#'
#' @return
#' @export
#'
#' @examples
#'
luno_limit_order <- function(ticker_pair,order_type=c("BID","ASK"), volume_bitcoin, price_pair_currency) {

  #Create Limit order

  resp <- luno_api_post(path = "/postorder",
                        post_list = list(pair = ticker_pair,
                                         type = order_type,
                                         volume = as.character(volume_bitcoin),
                                         price = as.character(price_pair_currency)),
                        login = TRUE)

  #Return the order number
  #resp$content$order_id

  #Rather returning whole object, in case there is an error
}




#' Title
#'
#' @param ticker_pair
#' @param order_type
#' @param volume_bitcoin
#' @param volume_pair_currency
#'
#' @return
#' @export
#'
#' @examples
#'
luno_market_order <- function(ticker_pair, order_type=c("BUY","SELL"), volume_bitcoin = NULL, volume_pair_currency = NULL) {

  if (order_type == "BUY" ) {

    if (missing(volume_pair_currency) | (volume_pair_currency <= 0)) {
      stop("For a BUY market please provide a positive `volume_pair_currency` i.e the amount to use to buy Bitcoin with")
    }

    if (!missing(volume_bitcoin)) {
      stop("For a BUY market order the `volume_bitcoin` parameter is not used. Please only use `volume_pair_currency`.")
    }

    resp <- luno_api_post(path = "/marketorder",
                          post_list = list(pair = ticker_pair,
                                           type = order_type,
                                           counter_volume = as.character(volume_pair_currency)),
                          login = TRUE)

  } else if (order_type == "SELL") {

    if (missing(volume_bitcoin) | (volume_bitcoin <= 0)) {
      stop("For a SELL market please provide a positive `volume_bitcoin` i.e the amount of Bitcoin to sell")
    }

    if (!missing(volume_pair_currency)) {
      stop("For a SELL market order the `volume_pair_currency` paramter is not used. Please only use `volume_bitcoin`.")
    }

    resp <- luno_api_post(path = "/marketorder",
                          post_list = list(pair = ticker_pair,
                                           type = order_type,
                                           base_volume = as.character(volume_bitcoin)),
                          login = TRUE)

  }

  #Return the order number
  #resp$content$order_id

  #Rather returning whole object, in case there is an error

}

#' luno_get_order
#'
#' @param order_id
#'
#' @return
#' @export
#'
#' @examples
#'
luno_get_order <- function(order_id) {

  resp <- luno_api_get(path = str_c("/orders/",order_id),
                        login = TRUE)
}

#' luno_cancel_order
#'
#' @param order_id
#'
#' @return
#' @export
#'
#' @examples
#'
luno_cancel_order <- function(order_id) {

  resp <- luno_api_post(path = "/stoporder",
                        post_list = list(order_id = order_id),
                        login = TRUE)
}

