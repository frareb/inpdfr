#' Performs a metacomunity analysis.
#'
#' Use the package \code{\link[metacom]{Metacommunity}} to analyse the word-occurrence data.frame,
#'    considering words as species and documents as communities.
#'
#' @param wordF The data.frame containing word occurrences.
#' @param numSim Number of simulated null matrices, see \code{\link[metacom]{Metacommunity}}.
#' @param limit An integer to limit the number of words to use in the analysis.
#' @param getPlot If \code{TRUE}, save the plot in the RESULTS directory.
#' @param getTextSink If \code{TRUE}, save the console output in the RESULTS directory.
#' @param mwidth The width of the plot in pixels.
#' @param mheight The height of the plot in pixels.
#' @return An object of class \code{\link[metacom]{Metacommunity}}.
#' @examples
#' \dontrun{
#' doMetacomMetacom(wordF=myDF)
#' }
#' @export
doMetacomMetacom<-function(wordF,numSim=10,limit="Inf",getPlot=TRUE,getTextSink=TRUE,mwidth=800,mheight=800){

  subDir <- "RESULTS"
  dir.create(file.path(getwd(), subDir), showWarnings = FALSE)

  if(is.numeric(limit) && limit!=Inf && limit!="Inf"){
    wordF<-wordF[1:limit,]
  }

  metacomDB<-wordF[,2:length(wordF[1,])]
  metacomDB[metacomDB>=1]<-1
  rownames(metacomDB)<-wordF[,1]
  metacomDB<-t(metacomDB)
  metaCom<-metacom::Metacommunity((metacomDB),sims=numSim, allow.empty=TRUE)

  if(getPlot==TRUE){
    grDevices::png(filename="RESULTS/metacom_Metacommunity.png",width = mwidth, height = mheight)
    try(metacom::Imagine((metacomDB), col=c(0,grDevices::grey(0.6))),silent=TRUE)
    grDevices::dev.off()
  }

  if(getTextSink==TRUE){
    sink('RESULTS/metacom_Metacommunity.txt')
    cat('\n#######################\n### STRUCTURE       ###\n#######################\n')
    try(print(metacom::IdentifyStructure(metaCom)),silent=TRUE)
    cat('\n#######################\n### SUMMARY         ###\n#######################\n')
    try(print(summary(metaCom)),silent=TRUE)
    cat('\n#######################\n### RESULTS         ###\n#######################\n')
    try(print(metaCom),silent=TRUE)
    sink()
  }

  try(print(paste0("Identified community structure: ",metacom::IdentifyStructure(metaCom))),silent=TRUE)

  metaComPkg<-metaCom
  return(metaComPkg)
}