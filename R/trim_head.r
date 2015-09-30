#' trim_head
#'
#' @description Remove head  caracters  before user in  retweeted messages
#'
#' @param  x   Character  Text  of tweet
#' @return     Return Text of tweet without head.
#' @author  Istituto di Biometeorologia Firenze Italy  Alfonso crisci \email{a.crisci@@ibimet.cnr.it}
#' @keywords  retweet
#'
#'
#'
#' @export

trim_head <- function(x) {
  use::stringr
  sub('^(.*)?@', '', x)
}
