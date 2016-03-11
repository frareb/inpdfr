#' Performs a cluster analysis on the basis of the word-occurrence data.frame.
#'
#' Performs a cluster analysis on the basis of the word-occurrence data.frame
#'   using \code{\link[stats]{hclust}} function.
#'
#' @param wordF The data.frame containing word occurrences.
#' @param myMethod The method to compute distances, see \code{\link[stats]{dist}}
#'   function.
#' @param gp A logical to specify if groups should be made.
#' @param nbGp An intger to specify the number of groups. Ignored if \code{gp=FALSE}.
#' @param getPlot If \code{TRUE}, save the cluster plot in the RESULTS directory.
#' @param mwidth The width of the plot in pixels.
#' @param mheight The height of the plot in pixels.
#' @param ... Additional arguments from the \code{\link[stats]{hclust}} function.
#' @return An object of class \code{\link[stats]{hclust}}.
#' @examples
#' \dontrun{
#' doCluster(wordF=myDF,myMethod="ward.D2")
#' }
#' @export
doCluster<-function(wordF,myMethod="ward.D2",gp=FALSE,nbGp=5,getPlot=TRUE,mwidth=800,mheight=800,...){
  if(ncol(wordF)>3){
    fitClust <-  stats::hclust(stats::dist(t(as.matrix(wordF[,2:length(wordF[1,])]))),method=myMethod,...) # ,colnames=fileNames

    if(getPlot==TRUE){
      if (gp==TRUE){
        grDevices::png(filename="RESULTS/HCLUST.png",width = mwidth, height = mheight)
          try(graphics::plot(fitClust,hang=-1),silent=TRUE)
          groups <- try(stats::cutree(fitClust, k=nbGp),silent=TRUE)
          try(stats::rect.hclust(fitClust, k=nbGp, border="red"),silent=TRUE)
        grDevices::dev.off()

      }else{

        grDevices::png(filename="RESULTS/HCLUST.png",width = mwidth, height = mheight)
          try(graphics::plot(fitClust),silent=TRUE)
        grDevices::dev.off()

      }
    }
    return(fitClust)
  }
}

#' Performs a k-means cluster analysis on the basis of the word-occurrence data.frame.
#'
#' Performs a k-means cluster analysis on the basis of the word-occurrence data.frame
#'   using \code{\link[stats]{kmeans}} function.
#'
#' @param wordF The data.frame containing word occurrences.
#' @param nbClust The number of clusters.
#' @param nbIter The number of iterations allowed.
#' @param algo The algoritm used (see \code{\link[stats]{kmeans}}).
#' @param getPlot If \code{TRUE}, save the k-means cluster plot in the RESULTS directory.
#' @param mwidth The width of the plot in pixels.
#' @param mheight The height of the plot in pixels.
#' @param ... Additional arguments from the \code{\link[stats]{kmeans}} function.
#' @return An object of class kmeans (see \code{\link[stats]{kmeans}}).
#' @examples
#' \dontrun{
#' doKmeansClust(wordF=myDF)
#' }
#' @export
doKmeansClust<-function(wordF,nbClust=4,nbIter=10,algo="Hartigan-Wong",getPlot=TRUE,mwidth=800,mheight=800,...){
  if(ncol(wordF)>3){
    dd <-(stats::dist(t(as.matrix(wordF[,2:length(wordF[1,])])), method="euclidian"))# ,colnames=fileNames
    kfit <- stats::kmeans(x=dd, centers=nbClust, iter.max=nbIter, algorithm=algo,...)

    if(getPlot==TRUE){
      grDevices::png(filename="RESULTS/KMEANCLUST.png",width = mwidth, height = mheight)
        try(cluster::clusplot(as.matrix(dd), kfit$cluster, color=T, shade=T, labels=2, lines=0),silent=TRUE)
      grDevices::dev.off()
    }

    return(kfit)
  }
}
