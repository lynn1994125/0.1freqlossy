duplicate.deletion <- function(wordseqdataframe,threshold) {
  first <-proc.time()
  index = 1
  wordseqdataframe <- wordseqdataframe[order(wordseqdataframe$wordnum,decreasing = TRUE),]
  row.names(wordseqdataframe) <- NULL
  wordseqdataframe$judge <- FALSE
  while( nrow(wordseqdataframe) >= index  ) {
  
    head = 1
    temp <- wordseqdataframe[index,]  # the current item  
    tempnum <- temp$wordnum # The last char position
    tempword <- temp$word
    tail <- temp$wordnum- 1 #Before the last char position
    
    wordseqdataframe$judge[index] <-  TRUE

    while ( tail <= tempnum ) {
      tempword1 <- substr(tempword,start=head,stop=tail)
      if ( length(  which ( grepl( tempword1, wordseqdataframe$word) ) )  == 0 ) {
        
      } else if ( grepl( tempword1, "") )  {
        
      } else if ( length(  which ( grepl( tempword1, wordseqdataframe$word) ) ) != 0 &&  length( wordseqdataframe[which(wordseqdataframe$word %in%tempword1),]$judge == FALSE ) != 0 &&   wordseqdataframe[which(wordseqdataframe$word %in%tempword1),]$judge == FALSE  ) {
        #if tmpeword1 have the same word in dictionary then minus the count 
        
        wordseqdataframe[which(wordseqdataframe$word %in%tempword1),]$sum <- 2*wordseqdataframe[which(wordseqdataframe$word %in%tempword1),]$sum - sum( wordseqdataframe[which ( grepl( tempword1, wordseqdataframe$word) ) ,]$sum )
        
        wordseqdataframe[which(wordseqdataframe$word %in%tempword1),]$judge <- TRUE
      }
      
      head = head + 1 
      tail = tail + 1 
    }
    
    
    
    index = index + 1 
  }
  
  wordseqdataframe <- wordseqdataframe[which(wordseqdataframe$sum >= threshold),]
  #wordseqdataframe1 <- wordseqdataframe[-which(wordseqdataframe$num < (support*nrow(data)) ) ,]
  #wordseqdataframe1 <- wordseqdataframe[-which(wordseqdataframe$freq < (support*nrow(data)) ) ,]
  #row.names(wordseqdataframe1) <- NULL
  first1 <-proc.time()
  time <- first1 - first
  #write.csv(data.frame( time[1],time[2],time[3] ),"duptime1000.csv")

  return(wordseqdataframe)
}

