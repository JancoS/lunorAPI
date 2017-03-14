#' luno_api
#'
#' @param path
#' @param request_type
#' @param post_list
#' @param login
#'
#' @return
#' @export
#'
#' @examples
#'
luno_api <- function(path, request_type = c("GET","POST"), post_list = NULL, login = FALSE) {

  url <- modify_url("https://api.mybitx.com",path=str_c("/api/1",path))

  if (login == FALSE | missing(login)) {

    if (request_type == "POST") {
      resp <- POST(url, body = post_list)
    } else {
      resp <- GET(url)
    }

  } else {
    #Load pat environmental details from json file
    luno_auth()

    #Check login details (pat = personal access token)
    pat <- luno_pat()

    if (request_type == "POST") {
      resp <- POST(url, body = post_list, authenticate(pat$id,pat$pass))
    } else {
      resp <- GET(url, authenticate(pat$id,pat$pass))
    }

  }

  #Check for 1) http error
  #          2) that content is in json format

  if (http_error(resp)) {
    parsed <- xml2::read_html(resp)

    stop(
      sprintf(
        "luno API request failed [%s]\n%s\n",
        status_code(resp),
        xml_text(parsed) #Can improve the parsing!
      ),
      call. = FALSE
    )

  } else if (http_type(resp) != "application/json"){
    stop("API did not return json or html", call. = FALSE)

  }

  parsed <- jsonlite::fromJSON(content(resp,"text"),simplifyVector = FALSE)

  #Check for http error
  if (http_error(resp)) {
    stop(
      sprintf(
        "luno API request failed [%s]\n%s\n<%s>",
        status_code(resp),
        parsed$message,
        parsed$documentation_url
      ),
      call. = FALSE
    )
  }

  structure(
    list(
      content = parsed,
      path = path,
      response = resp
    ),
    class = "luno_api"
  )
}

#' print.luno_api
#'
#' @param x
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
#'
print.luno_api <- function(x, ...) {
  cat("<luno ", x$path, ">\n", sep="")
  str(x$content)
  invisible(x)
}


#' luno_api_get
#'
#' @param path
#' @param request_type
#' @param login
#'
#' @return
#' @export
#'
#' @examples luno_api_get("/ticker?pair=XBTZAR")
#'
luno_api_get <- function(path, request_type = "GET", login = FALSE) {
  luno_api(path = path, login = login)
}

#' luno_api_post
#'
#' @param path
#' @param post_list
#' @param login
#'
#' @return
#' @export
#'
#' @examples
#'
luno_api_post <- function(path, post_list, login = FALSE) {
  luno_api(path = path, request_type = "POST", post_list = post_list, login = login)
}

