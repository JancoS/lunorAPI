#' luno_auth
#'
#' @return
#' @export
#'
#' @examples
#'
luno_auth <- function() {

  auth_file <- Sys.getenv('LUNO_AUTH_FILE')
  auth_details <- readr::read_lines(auth_file) %>% jsonlite::fromJSON()

  #for now use environmental variables
  Sys.setenv(LUNO_PAT_ID = auth_details$key_id)
  Sys.setenv(LUNO_PAT_PASS = auth_details$key_pass)
}


#' luno_api_auth
#'
#' @param path
#'
#' @return
#' @export
#'
#' @examples
#'
luno_api_auth <- function(path) {
  luno_api_get(path,login=TRUE)
}

#' Title
#'
#' luno Personal Access Token
#' Use sys.setenv()
#'
#' @return
#' @export
#'
#' @examples
luno_pat <- function() {
  pat_id <- Sys.getenv('LUNO_PAT_ID')
  pat_pass <- Sys.getenv('LUNO_PAT_PASS')

  if (identical(pat_id,"") | identical(pat_pass,"")) {
    stop("Please set environmental variables LUNO_PAT_ID and LUNO_PAT_PASS to your personal access token details",
         call. = FALSE)
  }

  list(id = pat_id,pass = pat_pass)
}

#' luno_create_auth_file
#'
#' @param key_id
#' @param key_pass
#' @param file_path
#'
#' @return
#' @export
#'
#' @examples
#'
#'
luno_create_auth_file <- function(key_id, key_pass, file_path) {
  auth_details <- list("key_id" = key_id, "key_pass" = key_pass) %>% jsonlite::toJSON()
  readr::write_lines(auth_details, file_path)
}
