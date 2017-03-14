
#' luno_account_balance
#'
#' @return
#' @export
#'
#' @examples luno_account_balance()
#'
luno_account_balance <- function() {
  luno_api_auth("/balance")
}

#' luno_account_no
#'
#' @param currency_ticker
#'
#' @return
#' @export
#'
#' @examples   luno_account_no("ZAR")
#' luno_account_no("XBT")
#'
luno_account_no <- function(currency_ticker) {
  result <- luno_account_balance()

  account_no = ""

  for (i in 1:length(result$content$balance)) {
    if (identical(result$content$balance[[i]]$asset,currency_ticker))
      account_no = result$content$balance[[i]]$account_id
  }

  if (identical(account_no,"")) {
    stop(
      sprintf("No account exists for currency %s",
              ticker),
      call. = FALSE
    )
  }

  account_no
}

#' luno_account_transactions
#'
#' @param account_no
#' @param min_row
#' @param max_row
#'
#' @return
#' @export
#'
#' @examples
#'
luno_account_transactions <- function(account_no, min_row, max_row) {
  #Get transaction list
  luno_api_auth(str_c("/accounts/",account_no,"transactions?min_row=",min_row,"&max_row=",max_row))
}

#' luno_acc_trans_pending
#'
#' @param account_no
#'
#' @return
#' @export
#'
#' @examples
#'
luno_acc_trans_pending <- function(account_no) {

  luno_api_auth(str_c("/accounts/",account_no,"/pending"))

}

#' luno_acc_order_list
#'
#' @param filter_state
#' @param filter_ticker_pair
#'
#' @return
#' @export
#'
#' @examples luno_acc_order_list()
#'
luno_acc_order_list <- function(filter_state,filter_ticker_pair) {
  luno_api_auth("/listorders")
}


