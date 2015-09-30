#' trim_urls
#'
#' @description Remove links in a tweet message.
#'
#' @param  x   Character  Message of tweet
#' @return    Return the message of tweet without links.
#' @author  Istituto di Biometeorologia Firenze Italy  Alfonso Crisci \email{a.crisci@@ibimet.cnr.it}
#' @keywords  retweet
#'
#'
#'
#' @export

trim_urls <- function(x) {
  use::stringr
  str_replace_all(x, 'http[^[:blank:]]+', '')
}
