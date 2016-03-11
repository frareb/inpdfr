#' Performs a correspondance analysis on the basis of the word-occurrence data.frame.
#'
#' Performs a correspondance analysis on the basis of the word-occurrence data.frame
#'    using \code{\link[ca]{ca}} function.
#'
#' @param wordF The data.frame containing word occurrences.
#' @param getPlot If \code{TRUE}, save the \code{\link[ca]{ca}} plot in the RESULTS
#'   directory.
#' @param mwidth The width of the plot in pixels.
#' @param mheight The height of the plot in pixels.
#' @param ... Additional arguments from the \code{\link[ca]{ca}} function.
#' @return The results of the \code{\link[ca]{ca}} function.
#' @examples
#' \dontrun{
#' doCA(wordF=myDF)
#' }
#' @export
doCA<-function(wordF,getPlot=TRUE,mwidth=800,mheight=800,...){
  if(ncol(wordF)>3){
    freqWord<-0
    mergedDClean<-wordF[,2:length(wordF[1,])][colSums(wordF[,2:length(wordF[1,])])>freqWord]
    mergedDClean<-as.data.frame(cbind(as.character(wordF[,1]),mergedDClean))
    mergedDClean[,1]<-as.character(mergedDClean[,1])
    mergedDClean<-mergedDClean[which(rowSums(mergedDClean[,2:length(mergedDClean[1,])])>freqWord),]
    fitCa <- ca::ca(obj=as.matrix(mergedDClean[,2:length(mergedDClean[1,])],colnames=mergedDClean[,1]),nd=NA,...)

    if(getPlot==TRUE){
      grDevices::png(filename="RESULTS/CorAnalysis.png",width = mwidth, height = mheight)
        try(graphics::plot(fitCa,labels=c(0,2)),silent=TRUE)
      grDevices::dev.off()
    }
    return(fitCa)
  }
}
