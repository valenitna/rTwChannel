#' download_channel
#'
#' @description To be used to download channel data from http://disit.org/tv/ Twitter Vigilance of University of Florence. 
#'
#' @param  channel   Character  Name of channel in web format ( without blank space)-
#' @param  outfile   Character  Name of file to be saved
#' @param  start_date   Character Date of end of channel (format YYYY-MM-DD).
#' @param  end_date   Character  Date of end of channel (format YYYY-MM-DD).
#' @param  user   Character  User for disit platform. Required.
#' @param  pass   Character  Password for disit platform. Required.
#' @param  format   Character  File format for downloaded file. Default is csv.
#' @return File of channel data.
#' @author  Istituto di Biometeorologia Firenze Italy  Alfonso Crisci \email{a.crisci@@ibimet.cnr.it}
#' @keywords  DISIT,twitter vigilance
#'
#
#'
#' @export

download_channel=function(channel,outfile,start_date,end_date,user,pass,format="csv") {
   url_web=paste0("http://disit.org/tv/query/query.php?channel=",channel,"&start_date=",start_date,"&end_date=",end_date,"&format=",format)
  extra_web=paste0("--user ",user," --password ",pass)
  download(url_web,destfile=outfile,extra = extra_web)
}
