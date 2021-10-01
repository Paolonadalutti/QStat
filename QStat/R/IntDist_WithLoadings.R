#' Intertextual matrix computation
#'
#' This function calculates the intertextual distance in a corpus.
#'
#' @param x a word-document contingency matrix.
#' @param th can be 0, 1. Sets the threshold for the hapax to be considered (0) or not (1).
#' @param ntops a scalar integer. The number of most influencing words to be returned.
#' @return a list made "dist" (the intertextual distance), "tops" (most influencing words) and "downs" (less influencing words).
#' @details No details
#' @examples
#' library(tm)
#' texts <- c("This is a text","This is another text")
#' corpus <- VCorpus(VectorSource(texts))
#' contmat <- TermDocumentMatrix(corpus)
#' IntDistWL(contmat,th=0, ntops = 2)
#' @export
#' @import tm
#' 

IntDistWL <- function (x, th, ntops = 10) 
{
  starttime <- Sys.time()
  if (!th %in% c(0, 1)) {
    warning("Invalid th value. Setting th to 0.")
    th = 0
  }
  N <- nrow(x)
  Nc <- ncol(x)
  coln <- colnames(x)
  distance <- function(a, b, th) {
    if (sum(a) > sum(b)) {
      a1 = a
      a = b
      b = a1
    }
    na = sum(a) 
    nb = sum(b)
    b.hat = b * na/nb #freq. standardizzate di b
    nb.primo = sum(b.hat[b.hat > th])
    abs.distance = sum(abs(a - b.hat)[a > 0]) + sum(abs(a - b.hat)[(a == 0) & (b.hat >= th)])
    
    tops = numeric(length(a))
    tops[a > 0] = abs(a - b.hat)[a > 0]
    tops[(a == 0) & (b.hat >= th)] = abs(a - b.hat)[(a == 0) & (b.hat >= th)]
    names(tops) = names(a)
    tops <- head(sort(tops, decreasing = TRUE), ntops)
    
    downs = numeric(length(a))
    downs[a > 0] = abs(a - b.hat)[a > 0]
    downs[(a == 0) & (b.hat >= th)] = abs(a - b.hat)[(a == 0) & (b.hat >= th)]
    names(downs) = names(a)
    downs <- downs[downs>0]
    downs <- tail(sort(downs, decreasing = TRUE), ntops)
    
    # tops = abs(a - b.hat)[a > 0] + abs(a - b.hat)[(a == 0) & (b.hat >= th)]
    
    distance = list(d = abs.distance/(na + nb.primo), tops = tops, downs = downs)
  }
  d <- matrix(NA, nrow = Nc, ncol = Nc, dimnames = list(coln, coln))
  x <- as.matrix(x)
  
  tlistdonws <- list()
  tlisttops <- list()
  tindex <- 1
  
  for (h in 1:Nc) {
    for (j in h:Nc) {
      tindex <- tindex+1
      dd <- distance(x[, h], x[, j], th)
      d[h, j] = d[j, h] = dd$d
      # tlist[[tindex]] = dd$tops
      tlisttops[[paste(coln[h],"-",coln[j])]] = dd$tops
      tlistdonws[[paste(coln[h],"-",coln[j])]] = dd$downs
    }
  }
  
  return(list( dist = as.dist(d), tops = tlisttops, downs = tlistdonws ) )
}
