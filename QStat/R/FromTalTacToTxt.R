#' TalTac files conversion
#'
#' Convert TalTac-like corpora to plain txt files
#'
#' @param ttcorpus a character schalar or vector (e.g. you may want to import a TalTac Corpus using readLines)
#' @return a list, comprehensive of a character vector (the texts) and, optionally, the metadata matrix.
#' @seealso \code{\link{FromTxtToTalTac}}
#' @details You can use this function to convert a TalTac-like formatted corpus to a number of texts and the relative metadata matrix.
#' Please note that you can use TalTac to perform this conversion, if you have a TalTac license ;).
#' Please also note that importing the TalTac corpus with readLines command is the only tested approach, as it grants a new "endline" charachter for each text.
#' @examples 
#' library(tm)
#' TalTacCorpus <- file("TalTacCorpus.data","w")
#' cat("****T1 *V1=a *V2=b", file = TalTacCorpus,  sep = "\n")
#' cat("This is a text, with 9 words in total.", file = TalTacCorpus, sep = "\n")
#' cat("****T2 *V1=c *V2=d", file = TalTacCorpus, sep = "\n")
#' cat(" This is another text, this time with 11 words in total", file = TalTacCorpus,  sep = "\n")
#' close(TalTacCorpus)
#' x <- readLines("TalTacCorpus.data")
#' unlink("TalTacCorpus.data")
#' txtCorpus <- FromTalTacToTxt(TalTacCorpus)
#' corpus <- VCorpus(VectorSource(txtCorpus))
#' @import tm
#' 
FromTalTacToTxt<-function(ttcorpus){
  
  #Tiro fuori i vettori con i titoli
  vtitoli<-grep("\\*\\*\\*",ttcorpus)
  nc<-length(vtitoli)
  
  #Inizializzo il vettore per le variabili categoriali
  varcatfile<-character()
  
  finaltvector <- c()
  finaltitles <- c()
  
  #ciclo sul numero di titoli
  for(i in 1:nc){
    # cat(paste("estraggo articolo",i,"\n"))
    varcat<-ttcorpus[vtitoli[i]] 
    varcat<-gsub(" ","",varcat)
    varcat<-gsub("\\*\\*\\*\\*","",varcat)
    varcatlist<-strsplit(varcat,"\\*")
    tit<-varcatlist[[1]][1]
    
    #Parsing delle variabili categoriali
    ll <- varcatlist[[1]][-1]
    
    #carico il testo i-esimo e creo il file
    if(i == nc)
      tn<-ttcorpus[(vtitoli[i]+1):length(ttcorpus)]
    else 
      tn<-ttcorpus[(vtitoli[i]+1):(vtitoli[(i+1)]-1)]
    
    finaltvector <- c(finaltvector,tn)
    finaltitles <- c(finaltitles,tit)
    
    #Generazione metadati
    variables <- strsplit(varcatlist[[1]][-1],"=")
    if(i == 1) {
      variablenames <- unlist(lapply(X = variables, FUN = function(x) x[[1]]))
      metadata <- as.data.frame(matrix(ncol = length(variablenames)))
      variablevalues <- unlist(lapply(X = variables, FUN = function(x) x[[2]]))
      metadata <- rbind(metadata,variablevalues)
      metadata <- metadata[-1,]
    } else {
      variablevalues <- unlist(lapply(X = variables, FUN = function(x) x[[2]]))
      metadata <- rbind(metadata,variablevalues)
    }
    
  }
  rownames(metadata) <- finaltitles
  names(finaltvector) <- finaltitles
  result <- list(texts = finaltvector, metadata = metadata)
  return(result)
}