feature.extraction <- function(word,bkt.num,tag,min.sup ) { # tag 第幾個標籤
  threshold <- bkt.num* min.sup
  if ( threshold <= 1 ) {
    threshold <- 2
  }
  word <- frequecesequnce( word ,threshold )
  write.csv(word,paste( "beforeduplicate.deletion" , tag ,"-",bkt.num, ".csv" ,sep = "") )
  word <- duplicate.deletion(word,threshold) # Get the true frequent of each word
  write.csv(word,paste( "afterduplicate.deletion" , tag ,"-",bkt.num, ".csv" ,sep = "") )
  word <- as.data.table( cbind( as.character(word$word),as.numeric( word$sum ) ) )
  colnames(word) <- c( "word","freq")


  
  return(word)
}
