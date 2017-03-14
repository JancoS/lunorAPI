
#' luno_order_book
#'
#' @param ticker_pair
#' @param check_ticker
#'
#' @return
#' @export
#'
#' @examples luno_order_book("XBTZAR")
luno_order_book <- function(ticker_pair, check_ticker = TRUE) {

  #Check if the Ticker Pair Exists
  if (check_ticker | missing(check_ticker)) {
    valid_ticker_pairs <- luno_ticker_pair_list()

    if (!(ticker_pair %in% valid_ticker_pairs)) {
      stop(
        sprintf(
          "Ticker pair %s does not exist. The valid ticker pairs are: (%s)",
          ticker_pair,
          str_c(valid_ticker_pairs,",")
        ),
        .call = FALSE
      )
    }
  }

  #Get the Order Book
  luno_api_get(str_c("/orderbook?pair=",ticker_pair))

}



#' @title luno_past_trades
#'
#' @description Past trades information, with a maximum of 100 trades are given per call
#'
#' @param ticker_pair
#'
#' @return
#' @export
#'
#' @examples luno_past_trades("XBTZAR")
luno_past_trades <- function(ticker_pair) {

  luno_api_get(str_c("/trades?pair=",ticker_pair))

}
