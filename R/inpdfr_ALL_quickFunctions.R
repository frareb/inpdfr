#' A quick way to obtain the word-occurrence data.frame from a set of documents.
#'
#' @param mywd A character variable containing the working directory.
#' @param language The language used ("French", "English", "Spanish").
#' @return A single word-occurrrence data.frame.
#' @examples
#' \dontrun{
#' getwordOccuDF(mywd = getwd(), language = "English")
#' }
#' @export
getwordOccuDF <- function(mywd, language = "English"){
  listFilesExt <- getListFiles(mywd)
  wordFreqPDF <- getPDF(myPDFs = listFilesExt$pdf)
  wordFreqTXT <- getTXT(myTXTs = listFilesExt$txt)
  wordFreq <- append(wordFreqPDF, wordFreqTXT)
  wordFreq <- excludeStopWords(wordF = wordFreq, lang = language)
  wordFreq <- truncNumWords(maxWords = Inf, wordF = wordFreq)
  mergedD <- mergeWordFreq(wordF = wordFreq)
  return(mergedD)
}

#' A quick way to compute a set of analysis from the word-occurrence data.frame.
#'
#' @param dataset A single word-occurrrence data.frame.
#' @return A set of analyses available from the \code{inpdfr} package.
#' @examples
#' \dontrun{
#' getAllAnalysis(dataset = myDF)
#' }
#' @export
getAllAnalysis <- function(dataset){
  makeWordcloud(wordF = dataset, wcminFreq = 50, wcmaxWords = Inf, 
    wcRandOrder = FALSE, wcCol = RColorBrewer::brewer.pal(8, "Dark2"), 
    getPlot = c(FALSE, TRUE))
  getSummaryStatsBARPLOT(wordF = dataset)
  getSummaryStatsHISTO(wordF = dataset)
  getSummaryStatsOCCUR(wordF = dataset)
  getMostFreqWord(wordF = dataset, numWords = 5)
  getMostFreqWord(wordF = dataset, numWords = 50)
  getMostFreqWord(wordF = dataset, numWords = 100)
  getXFreqWord(wordF = dataset, 50)
  doCA(wordF = dataset)
  doCluster(wordF = dataset, myMethod = "ward.D2", gp = FALSE, nbGp = 5)
  doKmeansClust(wordF = dataset, nbClust = 4, nbIter = 10, algo = "Hartigan-Wong")
  doMetacomEntropart(wordF = dataset)
  doMetacomMetacom(wordF = dataset, numSim = 10, limit = "Inf")
}
