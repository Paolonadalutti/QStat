#' Function that merges several texts together
#'
#' This function reads a corpus as main input, and merges all texts together to obtain a unique text from the corpus.
#'
#' @param x a tm corpus.
#' @details This function reads a corpus and returns a character string. If needed, the character string should be coherced again to a corpus.
#' @examples
#' library(tm)
#' texts <- c("This is a text","This is another text")
#' corpus <- VCorpus(VectorSource(texts))
#' MergedText <- MergeAllTexts(corpus)
#' MergedCorpus <- VCorpus(VectorSource(MergedText))
#' MergedCorpus[["1"]]$content
#' @export
#' @import tm
#' 
MergeAllTexts<-function(x, collapsevalue = " "){
  l = length(x)
  for(z in 1:l){
    testo_i <- x[[z]]
    testo_i <- scan_tokenizer(testo_i)
    if(z == 1) {
      testo <- testo_i
    } else {
      testo <- c(testo, testo_i)
    }
  }
  testo <- paste(testo, collapse = collapsevalue)
  return(testo)
}
