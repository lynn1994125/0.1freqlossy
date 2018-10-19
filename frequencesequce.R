frequecesequnce <- function( word ,threshold) {
  firsttime <-proc.time()
  stop <- FALSE 
  row.names(word) <- NULL 
  x <- 1
  first <- 0
  success <- FALSE
  word1 <- data.table(word$word,as.numeric(( word$position) ) )
  colnames(word1) <- c("word","position")
 
  if ( is.null( word1 ) ) {
    stop = TRUE
    
  } else if (  nrow( word1 ) >= 0  ){
    x <- 1 
    first <- 0 
    word1count <- as.data.table( table(word1$word) )
    colnames(word1count) <- c("word","sum") 
    word1count$wordnum <- nchar(as.character(word1count$word))
    wordseqdataframe<- word1count 
    colnames( wordseqdataframe) <- c("word","sum","wordnum") 
    if ( length(which ( wordseqdataframe$sum < threshold )) >= 1  ) {
      delete <- wordseqdataframe[ which ( wordseqdataframe$sum < threshold ), ]
      wordseqdataframe <- wordseqdataframe[-which(wordseqdataframe$sum < threshold),]
      word1 <- word1[-which(word1$word %in% delete$word ),]
    }

    remove(word1count)
  }


  
  if ( is.null( word1 ) ) {
    stop = TRUE
    
  } else if (  nrow( word1 ) >= 0  ) {
    x <- 1 
    first <- 0 

    length <-  nrow(word1)
   
    while (  x < length )  { 
 
      
      word1$nextword <- match(word1$position+1 , word1$position )
      word2 <- data.table(paste( word1$word,substr( as.character(word1$word[word1$nextword ]), nchar(as.character(word1$word[1]) ),nchar(as.character(word1$word[1]) ) ),sep = ""),word1$position[word1$nextword] ) 
      colnames(word2) <- c("word","position")
      word2 <- word2[-which(is.na( word2$position) ),]
      
      if ( is.null( word2 ) ) {
        stop = TRUE
        
      } else if (  nrow( word2) > 0  ){
        x <- 1 
        first <- 0 
 
        word2count <- as.data.table( table(word2$word) )
        colnames(word2count) <- c("word","sum") 
        # if ( length(which( word2count$sum < threshold ) ) != 0  ) {
        #   word2count <- word2count[-which( word2count$sum < threshold ) ,]
        # }

        word2count$wordnum <- nchar(as.character(word2count$word))
        wordseqdataframe<- rbind( wordseqdataframe, word2count ) 
        colnames( wordseqdataframe) <- c("word","sum","wordnum") 
        
      }
      word1 <- word2

      
      x = 1 
      
      first <- 0 
   
      if ( length(which ( wordseqdataframe$sum < threshold )) >= 1  ) {
        delete <- wordseqdataframe[ which ( wordseqdataframe$sum < threshold ), ]
        wordseqdataframe <- wordseqdataframe[-which(wordseqdataframe$sum < threshold),]
        word1 <- word1[-which(word1$word %in% delete$word ),]
        
      }

      length <-  nrow(word1)
     
    }
    

  }
  
   stopword <- as.data.table( readLines("stop_words.utf8",encoding ="UTF-8") )
   colnames(stopword) <- "word"
   if ( length(which(wordseqdataframe$word %in% stopword$word) ) != 0 ) {
     wordseqdataframe <- wordseqdataframe[ -which(wordseqdataframe$word %in% stopword$word),]
   }
  

  
  
  
  
  
  
  
  
  first1 <-proc.time()
  time1 <- first1 - firsttime
  #write.csv(data.table( time[1],time[2],time[3] ),"freseqtime4000.csv")
  return( wordseqdataframe)
}
