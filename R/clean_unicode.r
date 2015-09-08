#' clean_unicode
#'
#' @description Wrapper function to provide elimination of unicode characters in text array
#'
#' @param  variable  character  char vector
#' @return Return a text vector cleaned
#' @author  NYU Center for Data Science Pablo Barberá \email{pablo.barbera@@nyu.edu}
#' @keywords  unicode
#'
#'
#'
#' @export
#'
#'

clean_unicode <- function(variable){
  unicode.errors <- c("\\\u0092", "\\\u0086", "\\\x8e", "\\\x8f", "\\\x84", "\\\x87", "\\\x88", "\\\x92", "\\\x96", "\\\x97",
                      "\\\xe7", "\\\xed", "\\\xbc", "\\\x9c", "\\\xf2", "\\\x86", "\\\xa1", "\\\x95", "\\\x9f", "\\\x9e",
                      "\\#", "\\\x98", "\\\xf1", "\\\xec", "\\\x8d", "\\\U3e65653c", "\\\xc0", "'", "\\\xea", "\\\xbf",
                      "\\\x8b", "\\\xab", "\\\xe1", "\\\U3e33383cc", "\\\x83ire/", "\\\xbb", "/", "\\\U3e33393c",
                      "\\\x91", "\\\xc1", "\\\U3e33663c", "\\\xdc", "\\\xd1", "%", "&", "\\\x82", "\xed\xec", "\x8c",
                      "\\n", "\\t", "<U\\+[[:alnum:]]+>", "\\r", "\\\x8a", "\\\xc8", "\\\xc7", "\\\xb4", "\\\xa3",
                      "\\\xe8", "\\\xce", "\\\xc2")
  unicode.corrected <- c("í", "á", "é", "é", "Ñ", "á", "ó", "í", "Ñ", "ó", "Á", "í", " ", "ú", "Ú", " ", " ", "i", "u", "u", "",
                         "ò", "Ó", "I", "ç", "Ó", "¿", "", "Í", "o", "a", " ", " ", "É", " ", "a", " ", "í", "e", "i", "ó",
                         "", "Í", " ", " ", "Ç", "", " ", " ", " ", "", "", "", "", "", "", "", "", "", "")
  pb <- txtProgressBar(min=1,max=length(unicode.errors))
  for (i in 1:length(unicode.errors)){
    variable <- gsub(unicode.errors[i], unicode.corrected[i], variable)
    setTxtProgressBar(pb, i)
  }
  return(variable)
}
