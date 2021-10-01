texts <- c("This is a text for group A","This is another text for group A",
"This is the third text for group A","This is the last text for group A",
"This is a text for group B","This is another text for group B",
"This is the third text for group B","This is the last text for group B")
corpus <- VCorpus(VectorSource(texts))
group <- c("A","A","A","A","B","B","B","B")
mt <- MergeTexts(corpus, group =group, gStyle = "SameNumber", gSize = 2)
mt[[1]]$content
mt[[2]]$content
mt[[3]]$content
mt[[4]]$content



l <- c(100,50,20,10,30,60,90,120)
g <- c(rep("a",4),rep("b",4))

data.frame(freq = l, group = g)



mt <- function(x, gStyle, gFactor = NULL, gSize = NULL, gNumber = NULL){
  Ntexts <- length(x)
  gNames <- unique(gFactor)
  gn <- length(unique(gFactor))
  metadata <- character()
  
  ## Fissato il numero di macro-testi che si vogliono ottenere, alloca automaticamente i testi in modo da bilanciare le lunghezze
  if(gStyle == "SameNumber") {
    
  }
}


n <- 4
x <- c(10,20,30,40)#,50,60,70,80,90,100)
nx <- length(x)

n^nx-n*(n-1)^nx-n*(n-2)^nx-n*(n-3)^nx

ncol(combn(nx-1,n-1))
ncol(combn(nx,n))
ncol(combn(nx+n-1,n-1))


cc <- combn(x = x, m = n)
ccf <- combn(x = x, m = n, FUN = mean)
cc[,88]
cc - mean(x)

cc[,1:3]
