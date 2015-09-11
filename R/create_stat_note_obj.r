#' create_state_obj
#'
#' @description Create a stat note object from time list of hashtag 
#'
#' @param  x  data.frame  Terms or user date list object from \code{channel_analytics}
#' @param  qdap_hash Annotation file 
#' @return list Return stat object
#' @author  Istituto di Biometeorologia Firenze Italy  Alfonso Crisci \email{a.crisci@@ibimet.cnr.it}
#' @keywords  match
#'
#'
#'
#' @export
#'
#'

create_state_obj=function(x,notes,save=TRUE,fileName="") {
  
  res=list()

  for ( i in seq_along(notes))
  { notes_temp=notes[[i]]
    notes_temp=notes_temp[!duplicated(notes_temp),]
    res[[i]]=stat_notes(x,notes_temp)
  }
  if ( save ==TRUE) {
                    saveRDS(res,file=paste0("fileName",".rds"))
  }
  return(res)
  }
