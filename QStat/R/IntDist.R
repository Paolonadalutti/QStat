#' Intertextual matrix computation
#'
#' This function calculates the intertextual distance in a corpus.
#'
#' @param x a word-document contingency matrix.
#' @param th can be 0, 1. Sets the threshold for the hapax to be considered (0) or not (1).
#' @return a distance object.
#' @details No details
#' @examples
#' library(tm)
#' texts <- c("This is a text","This is another text")
#' corpus <- VCorpus(VectorSource(texts))
#' contmat <- TermDocumentMatrix(corpus)
#' intdist(contmat,th=0)
#' @export
#' @import tm
#' 
IntDist <- function (x, th) {
  starttime <- Sys.time()
  if(!th %in% c(0,1)){
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
    b.hat = b * na/nb
    nb.primo = sum(b.hat[b.hat > th])
    abs.distance = sum(abs(a - b.hat)[a > 0]) + sum(abs(a - b.hat)[(a == 0) & (b.hat >= th)])
    distance = abs.distance/(na + nb.primo)
  }
  d <- matrix(NA, nrow = Nc, ncol = Nc, dimnames = list(coln, coln))
  x <- as.matrix(x)
  for (h in 1:Nc) {
    for (j in h:Nc) {
      d[h, j] = d[j, h] = distance(x[, h], x[, j], th)
    }
  }
  # if(is(x)=="TermDocumentMatrix") x <- as.matrix(x)
  # d <- dist_make(t(x),distance,method="Intertextual")
  # cat(paste("elapsed time:",difftime(Sys.time(),starttime)))
  return(as.dist(d))
}
