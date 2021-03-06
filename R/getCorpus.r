#' getCorpus
#'
#' @description Creating corpus object by using tm R package a from messages.
#'
#' @param  textVector   Character  Message of tweet
#' @param  hashtag    remove hashtag from corpus
#' @return  Return   the tm corpus of message purged by mentions and links.
#' @author  Istituto di Biometeorologia Firenze Italy  Alfonso crisci \email{a.crisci@@ibimet.cnr.it}
#' @keywords  Tm, corpus
#'
#'
#'
#' @export

getCorpus <-function(textVector, hashtag=FALSE)
{ if ( hashtag==TRUE)
  {textVector=qdapRegex::rm_hash(textVector)}
  textVector=qdapRegex::rm_url(textVector)
  textVector=trim_users(textVector)
  textVector=trim_oddchar(textVector)
  doc.corpus <- tm::Corpus(VectorSource(textVector))
  doc.corpus <- tm::tm_map(doc.corpus, tolower)
  doc.corpus <- tm::tm_map(doc.corpus, removeNumbers)
  doc.corpus <- tm::tm_map(doc.corpus, removePunctuation)
  doc.corpus <- tm::tm_map(doc.corpus, PlainTextDocument)
  return(doc.corpus)
}
