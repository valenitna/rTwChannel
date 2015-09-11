#' native_channel
#'
#' @description extract a channel object only with original tweet 
#'
#' @param  channel  data.frame  Channel object
#' @return channel_obj data.frame Return a data.frame 
#' @author  Istituto di Biometeorologia Firenze Italy  Alfonso Crisci \email{a.crisci@@ibimet.cnr.it}
#' @keywords  channel
#'
#'
#'
#' @export
#'
#'

native_channel=function(channel) {
  
  ls_retweet=unlist(lapply(x$text,FUN=function(x) is.retweet(x)))
  channel_obj=channel[which(ls_retweet==FALSE),]
  return(channel_obj)
}
