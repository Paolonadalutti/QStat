#' Performs a basic corpus cleanse
#'
#' This function calculates the intertextual distance in a corpus.
#'
#' @param corpus a tm corpus.
#' @param n scalar integer. The length of each chunk to sample.
#' @param k scalar integer. The number of samples to take in each text.
#' @param th can be 0 or 1. The intertextual distance threshold.
#' @return a list comprehending both the k sampled intertextual distances and the final sampled intertextual distance.
#' @details No details
#' @examples 
#' library(tm)
#' corpus <- c("This is a dirty  text (with a double space). 
#'    There's also a proper name: Lisa and some RANdoM capITal LETTers? 
#'    Also other capital examples! With other punctuation symbols.",
#'    "This is another dirty text, with some punctuation.")
#' corpus <- VCorpus(VectorSource(corpus),readerControl = list(language="ita"))
#' @export
#' @import tm

CleanCorpus <- function (corpus, CapitalAfterPunctuation = FALSE, alltolower = FALSE) {
  
  if(CapitalAfterPunctuation & alltolower) stop("CapitalAfterPunctuation and alltolower cannot be all TRUE.")
  
  if(CapitalAfterPunctuation){
    corpus <- tm_map(corpus, content_transformer(function(t) gsub("(^[A-Z])","\\L\\1",t,perl=TRUE)))
    corpus <- tm_map(corpus, content_transformer(function(t) gsub("([\\.\\?\\!] [A-Z])","\\L\\1",t,perl=TRUE)))
  }
  return(corpus)
}
