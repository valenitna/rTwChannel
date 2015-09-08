#' create_xts_from_df
#'
#' @description Wrapper function to provide xts object creation from a data.frame object
#'
#' @param  x  data.frame  Data.frame with time var
#' @param  ntimevar  numeric  Index of var where time is indexed
#' @return A xts time series object.
#' @author  Istituto di Biometeorologia Firenze Italy  Alfonso crisci \email{a.crisci@@ibimet.cnr.it}
#' @keywords  xts,timeseries
#'
#'
#'
#' @export
#'
#'

create_xts_from_df=function(x,ntimevar) {
  require(xts)
  time=x[,ntimevar]
  x[,ntimevar]<-NULL
  rownames(ts_stat)<-time
  xts_obj=as.xts(ts_stat)
  return(xts_obj)
}
