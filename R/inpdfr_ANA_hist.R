#' Plot an histogram with the number of words excluding stop words
#'
#' Plot a histogram with the number of words excluding stop words using \code{\link[graphics]{hist}} function.
#' @param wordF The data.frame containing word occurrences.
#' @param mwidth The width of the plot in pixels.
#' @param mheight The height of the plot in pixels.
#' @param ... Additional arguments from \code{hist} function.
#' @return NULL
#' @examples
#' \dontrun{
#' getSummaryStatsHISTO(wordF=myDF)
#' }
#' @export
getSummaryStatsHISTO<-function(wordF,mwidth=800,mheight=800,...){

  subDir <- "RESULTS"
  dir.create(file.path(getwd(), subDir), showWarnings = FALSE)

  mFreq<-matrix(as.matrix(wordF[,2:length(wordF[1,])]),ncol=(length(wordF[1,])-1),dimnames=list(as.vector(wordF[,1]),names(wordF)[2:length(wordF[1,])]))
  grDevices::png(filename=paste0("RESULTS/HISTO_numWords",".png"),width = mwidth, height = mheight)
    try(graphics::hist(apply(mFreq,MARGIN=2,FUN=sum),main="",xlab="Number of words excluding stop words",col=grDevices::grey(0.5),...),silent=TRUE)
  grDevices::dev.off()

  return(NULL)
}
