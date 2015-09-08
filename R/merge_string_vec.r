#' merge_string_vec
#'
#' @description Merge character vec in one string.
#'
#' @param  sdata  character  char vector
#' @return Return text  merged in one element.
#' @author  Istituto di Biometeorologia Firenze Italy  Alfonso crisci \email{a.crisci@@ibimet.cnr.it}
#' @keywords string
#'
#'
#'
#' @export
#'
#'
merge_string_vec<-function(sdata) {
  return(do.call(paste, c(as.list(sdata), sep=" ")))
}
