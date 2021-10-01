#' Prints out text-level lexicographic measures
#'
#' This function saves a data frame with the main lexicographic measures about each text in the corpus.
#'
#' @param x a tm corpus.
#' @seealso \code{\link{ExploreCorpus}}
#' @details This function reads as a input both a Term-Document Matrix or a Document-Term Matrix.
#' @examples 
#' library(tm)
#' corpus <- c("This is a text, with 9 words in total","This is another text, this time with 11 words in total")
#' corpus <- VCorpus(VectorSource(corpus))
#' contingencymatrix <- TermDocumentMatrix(corpus)
#' ExploreTexts(contingencymatrix)
#' contingencymatrix <- DocumentTermMatrix(corpus)
#' ExploreTexts(contingencymatrix)
#' @export

ExploreTexts <- function (x) {
  if(any(class(x) == "TermDocumentMatrix")){
    textnames <- colnames(x)
    tab <- as.matrix(x)
  } else if(any(class(x) == "DocumentTermMatrix")){
    x <- t(x)
    textnames <- colnames(x)
    tab <- as.matrix(x)
  }
  nt <- ncol(tab)
  Nt <- apply(x, 2, sum)
  Vt <- integer()
  for (i in 1:nt) {
    Vt[i] <- length(tab[, i][tab[, i] != 0])
  }
  Ht <- integer()
  for (i in 1:nt) {
    Ht[i] <- length(tab[, i][tab[, i] == 1])
  }
  res <- cbind(Nt, Vt, Ht)
  colnames(res) <- c("Text N", "Text V", "Text H")
  return(res)
}