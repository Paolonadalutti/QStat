#' Sampled Intertextual Distance Calculation
#'
#' This function calculates the intertextual distance in a corpus.
#'
#' @param corpus a tm corpus.
#' @param n scalar integer. The length of each chunk to sample.
#' @param k scalar integer. The number of samples to take in each text.
#' @param th can be 0 or 1. The intertextual distance threshold.
#' @return a list comprehending both the k sampled intertextual distances and the final sampled intertextual distance.
#' @seealso \code{\link{IntDist}}
#' @details No details
#' @examples 
#' library(tm)
#' texts <- c("This is a text, with 9 words in total","This is another text, this time with 11 words in total")
#' corpus <- VCorpus(VectorSource(texts))
#' campdist(corpus,n=6,k=2,th=0)
#' @export
#' @import tm
SampledIntDist <- function (corpus, n, k, th = 0) {
  seed <- 1
  Ntexts <- length(corpus)
  #Cubo che contiene tutte le matrici di distanza campionate  
  distcube <- array(data = NA, dim = c(Ntexts, Ntexts, k), 
                    dimnames = list(names(corpus), names(corpus), as.character(1:k))) 
  flatdistmatrix <- matrix(NA, ncol = Ntexts, nrow = Ntexts) #Matrice che contiene le distanze mediate
  for (i in 1:Ntexts) {
    ctext <- corpus[[i]]
    ctext <- scan_tokenizer(ctext)
    if (length(ctext) < n + k + 1) 
      stop(paste("Text", i, "is too short for the selected sample size."))
  }
  for (i in 1:k) {
    for (j in 1:Ntexts) {
      ctext <- corpus[[j]]
      ctext <- scan_tokenizer(ctext)
      N <- length(ctext)
      if (i == 1) 
        (seed <- 1)
      else seed <- ((N - n)/k) * i
      ctext <- ctext[seed:(seed + n)]
      ctext <- paste(ctext, collapse = " ")
      if (j == 1) 
        testo_i <- ctext
      if (j > 1) 
        testo_i <- c(testo_i, ctext)
    }
    textnames <- names(corpus)
    corp <- Corpus(VectorSource(testo_i), readerControl = list(reader = readPlain))
    matrice_cont <- as.matrix(TermDocumentMatrix(corp, control = list(
      tolower = F, removePunctuation = F, 
      bounds = list(local = c(1, Inf)), wordLengths = c(1, Inf))))
    colnames(matrice_cont) <- textnames
    matrice_dist <- as.matrix(IntDist(matrice_cont, th))
    for (x in 1:Ntexts) {
      for (y in 1:Ntexts) {
        distcube[x, y, i] <- matrice_dist[x, y]
      }
    }
  }
  for (p in 1:Ntexts) {
    for (q in 1:Ntexts) {
      for (z in 1:k) {
        flatdistmatrix[p, q] <- mean(distcube[p, q, ])
      }
    }
  }
  rownames(flatdistmatrix) <- textnames
  colnames(flatdistmatrix) <- textnames
  # matrice3d <- distcube
  matricedist <- as.dist(flatdistmatrix)
  risultato <- list(BasicDistanceMatrices=distcube, FinalDistanceMatrix=flatdistmatrix)
  return(risultato)
}
