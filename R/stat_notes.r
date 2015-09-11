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

stat_notes=function (x, notes) 
{
    require(qdap)
    require(qdapTools)
    require(data.table)
    
    options(warn = -1)
    x[,2] = tolower(x[,2])
    x[,2] = gsub("^@", "", x[, 2])
    label_notes = notes$y[1]
    x_unique = unique(x[,1:2])
    terms=x[,2]
    terms_unique=x_unique[,2]
    mapnotes=data.frame(data=x[,1],note_target=terms,authors=x$authors,noteterms=notes[terms]$y)
    mapnotes_unique =data.frame(data=x_unique[,1],note_target=terms_unique,noteterms=notes[terms_unique]$y)
   
    return(mapnotes_unique)
    options(warn = 0)
}
