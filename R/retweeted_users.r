#' retweeted_users
#'
#' @description Extract any user from native retweets from a tweet message
#'
#' @param  x   Character  Messages of channel's tweets
#' @return    A vector of retweted users
#' @author  Istituto di Biometeorologia Firenze Italy  Alfonso Crisci \email{a.crisci@@ibimet.cnr.it}
#' @keywords  user, retweet
#'
#' @references  qdapRegex R packages
#'
#' @export

retweeted_users=function(x) {
  require(qdapRegex)
  require(qdapTools)
  pat<-"RT @([:alnum:]*[_]*[:alnum:]*):"
  res = unlist(rm_default(x, pattern=pat,extract=T))
  if (length(res)>1) {res=res[length(res)]}
  res = gsub("^RT @","",res)
  res = gsub(":","",res)
  return(res)
}
