#' trim_at
#'
#' @description Remove at caracter from users mentioned in  tweet message
#'
#' @param  x   Character  Message of tweet
#' @return     Message of tweet without @@ simbol.
#' @author  Istituto di Biometeorologia Firenze Italy  Alfonso crisci \email{a.crisci@@ibimet.cnr.it}
#' @keywords  retweet
#'
#'
#'
#' @export


trim_at <- function(x) {
  sub('@', '', x)
}
