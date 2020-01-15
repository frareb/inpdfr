# #' Performs a correspondance analysis on the basis of the word-occurrence data.frame.
# #'
# #' Performs a correspondance analysis on the basis of the word-occurrence data.frame
# #'    using \code{\link[ca]{ca}} function.
# #'
# #' @param wordF The data.frame containing word occurrences.
# #' @param getPlot If \code{TRUE}, save the \code{\link[ca]{ca}} plot in the RESULTS
# #'   directory.
# #' @param mwidth The width of the plot in pixels.
# #' @param mheight The height of the plot in pixels.
# #' @param formatType The format for the output file ("eps", "pdf", "png", "svg", "tiff", "jpeg", "bmp").
# #' @param ... Additional arguments from the \code{\link[ca]{ca}} function.
# #' @return The results of the \code{\link[ca]{ca}} function.
# #' @examples
# #' data("wordOccuDF")
# #' doCA(wordF = wordOccuDF, getPlot = FALSE)
# #' @export
# doCA <- function(wordF, getPlot = TRUE, mwidth = 800, mheight = 800, formatType = "png", ...){
#   ## create RESULTS folder
#   if(getPlot == TRUE){
#     subDir <- "RESULTS"
#     dir.create(file.path(getwd(), subDir), showWarnings = FALSE)
#   }
#   ## make ca
#   if(ncol(wordF) > 3){
#     freqWord <- 0
#     mergedDClean <- wordF[,2:length(wordF[1,])][colSums(wordF[,2:length(wordF[1,])]) > freqWord]
#     mergedDClean <- as.data.frame(cbind(as.character(wordF[,1]), mergedDClean))
#     mergedDClean[,1] <- as.character(mergedDClean[,1])
#     mergedDClean <- mergedDClean[which(rowSums(mergedDClean[,2:length(mergedDClean[1,])]) > freqWord),]
#     fitCaMatrix <- as.matrix(mergedDClean[,2:length(mergedDClean[1,])]) #, colnames = mergedDClean[,1])
#     rownames(fitCaMatrix) <- mergedDClean[,1]
#     fitCa <- ca::ca(obj = fitCaMatrix, nd = NA, ...) 
#     
#     if(getPlot == TRUE){
#       R.devices::devEval(type = formatType, name = "CorAnalysis",
#        aspectRatio = mheight / mwidth,
#        scale = do.call(function(){if((mheight / mwidth) <= 1) {
#          x <- max(mheight / 480, mwidth / 480)} else {
#            x <- min(mheight / 480, mwidth / 480)}
#          return(x)}, list())
#        , path = file.path(getwd(), subDir), {
#         try(graphics::plot(fitCa,labels = c(0, 2)), silent = TRUE)
#        }
#       )
#     }
#     return(fitCa)
#   }
# }
