#' create_state_obj
#'
#' @description Create a stat note object from time list of hashtag 
#'
#' @param  x  data.frame  Terms or user date list object from \code{channel_analytics}
#' @param  notes qdap_hash Annotation file 
#' @param  save logical  If a rds file is generated 
#' @param  fileName character 
#' @return list Return stat object
#' @author  Istituto di Biometeorologia Firenze Italy  Alfonso Crisci \email{a.crisci@@ibimet.cnr.it}
#' @keywords  match
#'
#'
#'
#' @export
#'
#'

create_state_obj = function(x,notes,save=TRUE,fileName="") {
  
  res=list()

  for ( i in 1:length(notes))
  {   
  notes[[i]]=notes[[i]][!duplicated(notes[[i]]),]
  res[[i]] = stat_notes(x,notes[[i]])
  }
  if ( save == TRUE) {
                    saveRDS(res,file=paste0(fileName,".rds"))
  }
  return(res)
  }
