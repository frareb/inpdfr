#' Perform a barplot with the number of unique words per document
#'
#' Perform a barplot with the number of unique words per document using \code{\link[graphics]{barplot}} function.
#' @param wordF The data.frame containing word occurrences.
#' @param getPlot If \code{TRUE}, save the bar plot in the RESULTS directory.
#' @param mwidth The width of the plot in pixels.
#' @param mheight The height of the plot in pixels.
#' @param ... Additional arguments from \code{barplot} function.
#' @return The number of unique words per document.
#' @examples
#' \dontrun{
#' getSummaryStatsBARPLOT(wordF=myDF)
#' }
#' @export
getSummaryStatsBARPLOT<-function(wordF,getPlot=TRUE,mwidth=800,mheight=800,...){

  subDir <- "RESULTS"
  dir.create(file.path(getwd(), subDir), showWarnings = FALSE)

  getBarPlot<-function(mFreq,seqBar){
    mFreqSup1<-mFreq ; mFreqSup1[mFreqSup1<=max(mFreq)/seqBar[1]]<-0
    mFreqSup2<-mFreq ; mFreqSup2[mFreqSup2<=max(mFreq)/seqBar[2]]<-0
    mFreqSup3<-mFreq ; mFreqSup3[mFreqSup3<=max(mFreq)/seqBar[3]]<-0
    mFreqSup3Count<-sapply(1:ncol(mFreqSup3),function(i){length(mFreqSup3[,i][mFreqSup3[,i]!=0])})
    mFreqSup2Count<-sapply(1:ncol(mFreqSup2),function(i){length(mFreqSup2[,i][mFreqSup2[,i]!=0])})
    mFreqSup1Count<-sapply(1:ncol(mFreqSup1),function(i){length(mFreqSup1[,i][mFreqSup1[,i]!=0])})
    mFreqSup0Count<-sapply(1:ncol(mFreq),function(i){length(mFreq[,i][mFreq[,i]!=0])})
    barPlot1<-rbind(
      mFreqSup3Count,
      mFreqSup2Count-mFreqSup3Count,
      mFreqSup1Count-mFreqSup2Count,
      mFreqSup0Count-mFreqSup1Count,
      ...
    )
    return(barPlot1)
  }
  mFreq<-matrix(as.matrix(wordF[,2:length(wordF[1,])]),ncol=(length(wordF[1,])-1),dimnames=list(as.vector(wordF[,1]),names(wordF)[2:length(wordF[1,])]))
  barPlot1<-getBarPlot(mFreq=mFreq,seqBar=c(8,3,2))

  if(getPlot==TRUE){
    grDevices::png(filename=paste0("RESULTS/BARPLOT_numWords",".png"),width = mwidth, height = mheight)
      graphics::par(mar=c(10,4,1,1))
      try(graphics::barplot(barPlot1,las=2,ylab="Number of unique words",col=c(grDevices::grey(0.3),grDevices::grey(0.5),grDevices::grey(0.7),grDevices::grey(0.9)),names=colnames(mFreq)),silent=TRUE)
      try(graphics::legend("topright",fill=c(grDevices::grey(0.3),grDevices::grey(0.5),grDevices::grey(0.7),grDevices::grey(0.9)),legend=c(paste0("Words used more than: ",floor(max(mFreq)/2)),paste0("Words used more than: ",floor(max(mFreq)/3)),paste0("Words used more than: ",floor(max(mFreq)/8)),paste0("Words used less than or equal to: ",floor(max(mFreq)/8)))),silent=TRUE)
    grDevices::dev.off()
  }

  return(apply(barPlot1,MARGIN=2,FUN=sum,na.rm=TRUE))
}
