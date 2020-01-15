#' Plot an histogram with the number of words excluding stop words
#'
#' Plot a histogram with the number of words excluding stop words using \code{\link[graphics]{hist}} function.
#' @param wordF The data.frame containing word occurrences.
#' @param getPlot If \code{TRUE}, save the plot in the RESULTS directory.
#' @param mwidth The width of the plot in pixels.
#' @param mheight The height of the plot in pixels.
#' @param formatType The format for the output file ("eps", "pdf", "png", "svg", "tiff", "jpeg", "bmp").
#' @param ... Additional arguments from \code{hist} function.
#' @return NULL
#' @examples
#' data("wordOccuDF")
#' getSummaryStatsHISTO(wordF = wordOccuDF, getPlot = FALSE)
#' @export
getSummaryStatsHISTO <- function(wordF, getPlot = TRUE, mwidth = 800, mheight = 800, formatType = "png", ...){
  if(getPlot == TRUE){
    subDir <- "RESULTS"
    dir.create(file.path(getwd(), subDir), showWarnings = FALSE)
  }
  mFreq <- matrix(as.matrix(wordF[,2:length(wordF[1,])]), ncol = (length(wordF[1,])-1), 
    dimnames = list(as.vector(wordF[,1]), names(wordF)[2:length(wordF[1,])]))
  if(getPlot == TRUE){
    R.devices::devEval(type = formatType, name = "HISTO_numWords",
     aspectRatio = mheight / mwidth,
     scale = do.call(function(){if((mheight / mwidth) <= 1) {
       x <- max(mheight / 480, mwidth / 480)} else {
         x <- min(mheight / 480, mwidth / 480)}
       return(x)}, list())
     , path = file.path(getwd(), subDir), {
      try(graphics::hist(apply(mFreq, MARGIN = 2, FUN = sum), main = "", 
        xlab = "Number of words excluding stop words", col = grDevices::grey(0.5),...), 
        silent = TRUE)
     }
    )
  }
  return(mFreq)
}
