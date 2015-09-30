#' extract_mentions
#'
#' @description Extract any mentions, a twitter account mentioned in message, from a tweet.
#'
#' @param  x   Character  Messages of channel's tweets.
#' @return     Return the list of mentioned users
#' @author  Istituto di Biometeorologia Firenze Italy  Alfonso Crisci \email{a.crisci@@ibimet.cnr.it}
#' @keywords  mentions
#'
#' @references  qdap R packages
#
#'
#' @export

extract_mentions=function(x) {
  use::qdap
  res=strip(x, digit.remove = FALSE,char.keep = c("_","@"))
  res=rm_tag(res,extract=T)
  return(res)
}
