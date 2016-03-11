#' Truncate the word-occurrence data.frame.
#'
#' @param wordF The data.frame containing word occurrences.
#' @param maxWords The maximum number of words in the data.frame.
#' @return The data.frame containing word occurrences.
#' @examples
#' \dontrun{
#' truncNumWords(wordF=myWordOccurrenceDF,maxWords=50)
#' }
#' @export
truncNumWords<-function(wordF,maxWords){
  try(if((!is.na(maxWords) || maxWords!="")==TRUE){
    for(i in 1:length(wordF)){
      numWords<-length(wordF[[i]][[1]][,1])
      if(numWords<maxWords){maxWords_i<-numWords}else{maxWords_i<-maxWords}
      wordF[[i]][[1]]<-wordF[[i]][[1]][1:maxWords_i,]
    }
  },silent=TRUE)
  return(wordF)
}

#' Merge word-occurrence data.frames into a single data.frame.
#'
#' @param wordF The data.frame containing word occurrences.
#' @return A single word-occurrrence data.frame with each column corresponding to a text file.
#' @examples
#' \dontrun{
#' mergeWordFreq(wordF=myWordOccurrenceDF)
#' }
#' @export
mergeWordFreq<-function(wordF){
  fileNames<-sapply(wordF, "[[", 2)
  fileWordFreq<-lapply(wordF, "[[", 1)
  words<-unique(as.vector(unique(unlist(sapply(fileWordFreq,function(i){unique(i$word)})))))
  mydbWords<-data.frame(word=words)
  for(k in 1:length(fileWordFreq)){
    mydbWords<-try(merge(mydbWords,fileWordFreq[[k]][,c(1,3)],by.x=1,by.y=2,all=TRUE,suffixes = c(paste0(".x",k),paste0(".y",k))),silent=TRUE)
  }
  colnames(mydbWords)<-c("word",fileNames)
  mydbWords[is.na(mydbWords)]<-0

  if(length(fileNames)>1){
    ### stem over the incidence-matrix
    dd<-NULL
    # Stem words
    dd$stem <- SnowballC::wordStem(as.character(mydbWords[,1]), language = "english") ###
    dd$word <- as.character(mydbWords[,1])
    dd$freq <- mydbWords[,2:ncol(mydbWords)]

    agg_freq <- sapply(1:ncol(dd$freq),function(i){stats::aggregate(freq[,i] ~ stem, data = dd, sum)}) ### a optimiser
    agg_freq <- cbind(data.frame(unlist(agg_freq[1,1])),data.frame(agg_freq[2,]))
    agg_word <- stats::aggregate(word ~ stem, data = dd, function(x) x[1]) ###
    dd <- cbind(word = agg_word[,2], agg_freq[,2:ncol(agg_freq)]) ###
    names(dd)<-names(mydbWords)
    mydbWords<-dd
    ### end stem
  }
  if(length(fileNames)>1){
    mydbWords<-mydbWords[order(apply(mydbWords[,2:ncol(mydbWords)],MARGIN=1,FUN=sum), decreasing = T), ] # order matrix by incidence
  }
  return(mydbWords)
}
