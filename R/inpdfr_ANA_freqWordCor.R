#' Returns most frequent words.
#'
#' Returns most frequent words and plots their frequencies per document.
#'
#' @param wordF The data.frame containing word occurrences.
#' @param numWords The number of words to be returned.
#' @param getPlot If \code{TRUE}, save a scatter plot in the RESULTS directory.
#' @param mwidth The width of the plot in pixels.
#' @param mheight The height of the plot in pixels.
#' @return The \code{numWords} most frequent words.
#' @examples
#' \dontrun{
#' getMostFreqWord(wordF=myDF,numwords=5)
#' }
#' @export
getMostFreqWord<-function(wordF,numWords,getPlot=TRUE,mwidth=1024,mheight=800){

  subDir <- "RESULTS"
  dir.create(file.path(getwd(), subDir), showWarnings = FALSE)

  mostFreqWords<-NULL
  if(is.numeric(numWords)){
    if(numWords>nrow(wordF)){numWords<-nrow(wordF)}
    mostFreqWords<-wordF[1:numWords,1]
    mostFreqWords<-as.character(mostFreqWords)
  }
  mXwords<-wordF[1:numWords,2:ncol(wordF)]

  if(getPlot==TRUE){
    grDevices::png(filename=paste0('RESULTS/MostFreqWords_',numWords,'.png'),width = mwidth, height = mheight)
      graphics::plot(1,type="n",xlim=c(1,ncol(mXwords)),ylim=c(0,max(mXwords)),ylab="Occurrences",xlab="",axes=FALSE)
      graphics::axis(1,at=1:ncol(mXwords),labels=names(wordF[2:ncol(wordF)]),las=2)
      graphics::axis(2)
      mycol<-1:numWords
      sapply(1:nrow(mXwords),function(i){graphics::points(x=1:ncol(mXwords),y=as.vector(mXwords[i,]),type="o",col=mycol[i],lwd=2)})
      graphics::legend("topright",legend=as.character(wordF[1:numWords,1]),lty=1,col=mycol,lwd=2)
    grDevices::dev.off()
  }

  return(mostFreqWords)
}

#' Test for correlation between the most frequent words.
#'
#' @param wordF The data.frame containing word occurrences.
#' @param numWords The number of words to be returned.
#' @param getPlot A vector with two logical values. If \code{plots[1]==TRUE},
#'    an image of the correlation matrix is saved in the RESULTS directory.
#'    If \code{plots[2]==TRUE}, the image of the p-value matrix associated
#'    with the correlation is saved in the RESULTS directory.
#' @param getTextSink If \code{TRUE}, save the correlation matrix and the
#'    associated p-values in a text file in the RESULTS directory.
#' @param mwidth The width of the plot in pixels.
#' @param mheight The height of the plot in pixels.
#' @return A list with the correlation matrix and the p-value matrix.
#' @examples
#' \dontrun{
#' getMostFreqWordCor(wordF=myDF,numwords=5)
#' }
#' @export
getMostFreqWordCor<-function(wordF,numWords,getPlot=c(TRUE,TRUE),getTextSink=TRUE,mwidth=1024,mheight=1024){ # correlation between words

  subDir <- "RESULTS"
  dir.create(file.path(getwd(), subDir), showWarnings = FALSE)

  if(numWords>nrow(wordF)){numWords<-nrow(wordF)}
  M<-wordF[,2:ncol(wordF)]
  matCOR<-matrix(NA,ncol=numWords,nrow=numWords)
  for (i in 1:numWords){
    for (j in 1:numWords){
      matCOR[i,j]<-stats::cor(as.vector(unlist(M[i,])),as.vector(unlist(M[j,])))
    }
  }
  colnames(matCOR)<-wordF[1:numWords,1]
  rownames(matCOR)<-wordF[1:numWords,1]

  matCORtest<-matrix(NA,ncol=numWords,nrow=numWords)
  for (i in 1:numWords){
    for (j in 1:numWords){
      matCORtest[i,j]<-stats::cor.test(as.vector(unlist(M[i,])),as.vector(unlist(M[j,])))$p.value
    }
  }
  colnames(matCORtest)<-wordF[1:numWords,1]
  rownames(matCORtest)<-wordF[1:numWords,1]
  # matCORtest[matCORtest>0.05]<-NA

  if(getPlot[2]==TRUE){
    grDevices::png(filename=paste0('RESULTS/MostFreqWordsCorPvalue_',numWords,'.png'),width = mwidth, height = mheight)
      graphics::par(mar=c(7,7,1,1))
      graphics::image(matCORtest,axes=FALSE,col=grDevices::heat.colors(5))
      graphics::axis(1,at=seq(0,1,length=numWords),labels=colnames(matCOR),las=2)
      graphics::axis(2,at=seq(0,1,length=numWords),labels=colnames(matCOR),las=1)
    grDevices::dev.off()
  }

  matCorSign<-as.data.frame(matCOR)
  matCorSign[matCOR<= 1]<-"****"
  matCorSign[matCOR< 0.999]<-"***"
  matCorSign[matCOR< 0.75]<-"**"
  matCorSign[matCOR< 0.50]<-"*"
  matCorSign[matCOR< 0.25]<-"."
  matCorSign[matCOR< 0.10]<-""
  matCorSign[matCOR<(-0.15)]<-""
  matCorSign[matCOR<(-0.25)]<-"(*)"
  matCorSign[matCOR<(-0.50)]<-"(**)"
  matCorSign[matCOR<(-0.75)]<-"(***)"

  if(getTextSink==TRUE){
    sink(paste0('RESULTS/MostFreqWordsCor_',numWords,'.txt'))
    cat('\n#######################\n### RAW             ###\n#######################\n')
    try(print(matCOR),silent=TRUE)
    cat('\n#######################\n### SIGN            ###\n#######################\n')
    cat('# -1;-0.75		(***)\n# -0.75;-0.5	(**)\n# -0.5;-0.25	(*)\n# -0.25;-0.10	(.)\n# -0.10;0.10	\n# 0.10;0.25		.\n# 0.25;0.5		*\n# 0.5;0.75		**\n# 0.75;0.999	***\n# 0.999;1		****\n\n')
    try(print(matCorSign),silent=TRUE)
    cat('\n#######################\n### PVALUE          ###\n#######################\n')
    try(print(matCORtest),silent=TRUE)
    sink()
  }

  if(getPlot[1]==TRUE){
    grDevices::png(filename=paste0('RESULTS/MostFreqWordsCor_',numWords,'.png'),width = mwidth, height = mheight)
      graphics::par(mar=c(7,7,1,1))
      graphics::image(abs(matCOR),axes=FALSE,col=rev(grDevices::heat.colors(5)))
      graphics::axis(1,at=seq(0,1,length=numWords),labels=colnames(matCOR),las=2)
      graphics::axis(2,at=seq(0,1,length=numWords),labels=colnames(matCOR),las=1)
    grDevices::dev.off()
  }

  return(list(cor=matCOR,pval=matCORtest))
}

#' Returns most frequent words
#'
#' @param wordF The data.frame containing word occurrences.
#' @param occuWords The minimum number of occurrences for words to be returned.
#' @return A vector with most frequent words.
#' @examples
#' \dontrun{
#' getXFreqWord(wordF=myDF,occuWords=5)
#' }
#' @export
getXFreqWord<-function(wordF,occuWords){
  xFreqWords<-NULL
  if(is.numeric(occuWords)){
    datasetSum<-apply(wordF[,2:ncol(wordF)],MARGIN=1,FUN=sum)
    xFreqWords<-wordF[,1][datasetSum>=occuWords]
    xFreqWords<-as.character(xFreqWords)
  }
  return(xFreqWords)
}




