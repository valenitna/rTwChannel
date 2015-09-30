#' trim_hashtags
#'
#' @description Remove hashtag in a tweet message.
#'
#' @param  x   Character  Message of tweet
#' @return     Message of tweets without hashtag
#' @author  Istituto di Biometeorologia Firenze Italy  Alfonso crisci \email{a.crisci@@ibimet.cnr.it}
#' @keywords  retweet
#'
#'
#'
#' @export

trim_hashtags <- function(x) {
  use::stringr
  str_replace_all(x, '(#[[:alnum:]_]*)', '')
}
