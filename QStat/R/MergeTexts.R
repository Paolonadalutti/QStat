#' Corpus texts merging
#' 
#' This function can aggregate together texts, with some options available. Useful to analyze a large number of short texts.
#'
#' @param x a tm corpus.
#' @param gStyle can be "Factor", "SameNumber", "SameTokens". See Details.
#' @param gFactor a factor or character vector. Contains the category of each text.  
#' @param gSize a scalar integer. Is the number of final texts to have in each final group, or the number of tokens for each text depending on gStyle
#' @return a distance object.
#' @details #' Used in a corpus where short texts can be grouped based on some characteristics (author, time, ...), this function groups together the texts to form longer, more analyzable texts.
#' This function can be used with two grouping styles: fixed number of final texts for each group, and equal number of tokens for each final text.
#' The "SameNumber" option merges texts in order to have the same number of texts for each group. Allocation of texts is optimized in order to have a similar number of tokens in each macro-text.
#' The "SameTokens" option merges texts in order to have a different number of texts in the same group, but with an optimized balanced number of tokens.
#' @examples
#' library(tm)
#' texts <- c("This is a text for group A","This is another text for group A",
#' "This is the third text for group A","This is the last text for group A",
#' "This is a text for group B","This is another text for group B",
#' "This is the third text for group B","This is the last text for group B")
#' corpus <- VCorpus(VectorSource(texts))
#' group <- c("A","A","A","A","B","B","B","B")
#' mt <- MergeTexts(corpus, group =group, gStyle = "SameNumber", gSize = 2)
#' mt[[1]]$content
#' mt[[2]]$content
#' mt[[3]]$content
#' mt[[4]]$content

#' @export
#' @import tm
#' 
MergeTexts <- function(x,group, gStyle, gSize){
  Ntexts <- length(x)
  if(Ntexts != length(group)) stop("Grouping factor length is different from corpus number of texts.")
  groups <- unique(group)
  Ngroups <- length(unique(group))
  metadata <- character()
  
  if(gStyle == "SameNumber") {
    #unisco tot testi per ogni categoria
    for(g in 1:Ngroups){
      #cat(paste(g,"\n"))
      Glength <- sum(group==groups[g])
      soil <- floor(Glength/gSize) #Quanti testi corti devo aggregare assieme
      x_g <- x[c(1:length(group))[group==groups[g]]]
      metadata <- c(metadata,rep(groups[g],gSize))
      
      for (j in 1:gSize) {
        #cat(paste(j,"\n"))
        if(j==1){
          gStart <- 1
          gEnd <- soil
        } else {
          gStart <- soil*(j-1)+1
          gEnd <- soil*j
        }
        corpus_group <- x_g[gStart:gEnd]
        sector_j <- MergeAllTexts(corpus_group)
        if(j==1) {
          sector <- sector_j
        } else {
          sector <- c(sector,sector_j)
        }
      }
      corpus_lingua_i <- VCorpus(VectorSource(sector), readerControl = list(reader = readPlain))
      if(g==1){
        corpus_lingua <- corpus_lingua_i
      } else {
        corpus_lingua <- c(corpus_lingua,corpus_lingua_i,recursive=TRUE)
      }
    }
    
    meta(corpus_lingua, "language") <- metadata
    return(corpus_lingua)
    
  }
}
