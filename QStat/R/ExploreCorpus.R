#' Prints out corpus-level lexicographic measures
#'
#' This function saves a data frame with the main corpus-level lexicographic measures.
#'
#' @param x a contingency matrix.
#' @seealso \code{\link{ExploreTexts}}
#' @details No details
#' @examples 
#' library(tm)
#' corpus <- c("This is a text, with 9 words in total","This is another text, this time with 11 words in total")
#' corpus <- VCorpus(VectorSource(corpus))
#' contingencymatrix <- TermDocumentMatrix(corpus)
#' ExploreCorpus(contingencymatrix)
#' @export

ExploreCorpus <- function (x) 
{
  if(any(class(x) == "TermDocumentMatrix")){
    x <- x
  } else if(any(class(x) == "DocumentTermMatrix")){
    x <- t(x)
  }
  N <- sum(x)
  V <- nrow(x)
  Vn <- nrow(x)/N
  Nn <- apply(x, 1, sum)
  H <- length(Nn[Nn == 1])
  pcH <- H/V
  res <- list(N, V, Vn, H, pcH)
  names(res) <- c("N", "V", "V/N", "Hapax", "%Hapax")
  return(res)
}