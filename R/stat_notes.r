#' stat_notes
#'
#' @description Create a list statistical object by using qdap keylist
#'
#' @param  x  character  Vector of word to compute statistics of notes
#' @param  notes  qdap hash object  Vector of annotation category
#' @return A list of statistics for each category of annotation present in notes.
#' @author  Istituto di Biometeorologia Firenze Italy  Alfonso Crisci \email{a.crisci@@ibimet.cnr.it}
#' @keywords  annotation
#'
#'
#'
#' @export
#'
#'

stat_notes=function(x,notes) {
  
  require(qdap)
  require(qdapTools)
  options( warn = -1)
  x[,2]=tolower(x[,2])
  label_notes=notes$y[1]
  x_unique=unique(x)
  elements=gsub("^@","",x[,2])
  elements_unique=gsub("^@","",x_unique[,2])
  mapnotes=notes[elements]
  mapnotes_unique=notes[elements_unique]
  mapnotes$data=x$data
  mapnotes_unique$data=x_unique$data
  mapnotes=na.omit(mapnotes)
  mapnotes_unique=na.omit(mapnotes_unique)
  freq_day=as.data.frame(as.matrix(table(mapnotes$data)))
  names(freq_day)=c("N")
  freq_day$data=rownames(freq_day)
  rownames(freq_day)<-NULL
  freq_day_unique=as.data.frame(as.matrix(table(mapnotes_unique$data)))
  names(freq_day_unique)=c("N")
  freq_day_unique$data=rownames(freq_day_unique)
  rownames(freq_day_unique)<-NULL
  res=list()
  res$df=data.frame(freq_day$data,freq_day$N)
  res$df_unique=data.frame(freq_day_unique$data,freq_day_unique$N)
  names(res$df)<-c("date",label_notes)
  names(res$df_unique)<-c("date",label_notes)
  res$N=nrow(na.omit(mapnotes))
  res$N_sumuniquedate=nrow(na.omit(mapnotes_unique))
  return(res)
  options(warn=0)
}
