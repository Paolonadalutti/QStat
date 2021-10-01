#' Function that splits each text of corpus in sub-texts
#'
#' This function reads a corpus as main input, splits each text in a fixed size number of sub-texts. 
#' The number of sub-texts is based on shorter text length. So the size parameter has to be chosen carefully in order to maximize the number of chunks that will be extracted.
#'
#' @param x a tm corpus.
#' @param size a scalar integer. The size of the final chunks.
#' @param titles a character vector. An identifier for corpus texts. If NULL, meta = id is taken.
#' @details This function reads a corpus and returns another corpus. 
#' @examples
#' library(tm)
#' texts <- data.frame(doc_id = c("Text A","Text B"), 
#' text = c("This is a text with 7 words.",
#'    "This is another text, this time with 8 words."))
#' corpus <- VCorpus(VectorSource(texts))
#' SplittedCorpus <- SplitCorpus(corpus, size = 2, titles = c("First text","Second text"))
#' contMat <- TermDocumentMatrix(SplittedCorpus, control = list(wordLengths=c(0,Inf),stopwords=FALSE))
#' as.matrix(contMat)
#' @import tm
#' 
SplitCorpus <- function(x, size, titles){
  Ntexts <- length(x)
  
  if(!exists("titles")) titles <- meta(x,"id")
  
  if(length(titles) != length(x)) stop("The number of titles provided doesn't match with texts number.")
  
  for (i in 1:Ntexts) {
    tc <- x[[i]]
    tc <- scan_tokenizer(tc)
    if (length(tc) < size) 
      stop(paste("Text", titles[i], "is too short for the selected sample size."))
  }
  
  splittedtexts <- character()
  textNames <- character()
  
  for (j in 1:Ntexts) {
    
    tc <- x[[j]]
    tc <- scan_tokenizer(tc)
    N <- length(tc)
    Nseeds <- floor(N/size)
    
    seeds <- seq(from = 1, to = (N-size), length.out = Nseeds)
    temp_textNames <- rep(titles[j],Nseeds)
    temp_textNames <- paste(temp_textNames," - chunk ",1:Nseeds,sep = "")
    textNames <- c(textNames,temp_textNames)
    for(s in 1:Nseeds){
      temp_splittedtexts <- tc[seeds[s]:(seeds[s]+size-1)]
      temp_splittedtexts <- paste(temp_splittedtexts, collapse = " ")
      splittedtexts <- c(splittedtexts,temp_splittedtexts)
    }
  }
  
  dftexts <- data.frame(doc_id = textNames, text = splittedtexts)
  
  corp <- Corpus(DataframeSource(dftexts))
  return(corp)
}
