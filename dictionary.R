dictionary <- function(keyword1,keyword2,keyword3,keyword4,keyword5, keyword6,type ) {
  temp <- NULL
  num1 <- 0 
  num2 <- 0 
  num3 <- 0 
  num4 <- 0
  num5 <- 0
  num6 <- 0
  
  
  if ( length(keyword1) != 0 )keyword1$freq <- as.numeric( as.character(keyword1$freq) )
  if ( length(keyword2) != 0 )keyword2$freq <- as.numeric( as.character(keyword2$freq) )
  if ( length(keyword3) != 0 )keyword3$freq <- as.numeric( as.character(keyword3$freq) )
  if ( length(keyword4) != 0 )keyword4$freq <- as.numeric( as.character(keyword4$freq) )
  if ( length(keyword5) != 0 )keyword5$freq <- as.numeric( as.character(keyword5$freq) )
  if ( length(keyword6) != 0 )keyword6$freq <- as.numeric( as.character(keyword6$freq) )
  
  if ( length(keyword1) != 0 )num1 <- sum(keyword1$freq) 
  if ( length(keyword2) != 0 )num2 <- sum(keyword2$freq) 
  if ( length(keyword3) != 0 )num3 <- sum(keyword3$freq) 
  if ( length(keyword4) != 0 )num4 <- sum(keyword4$freq) 
  if ( length(keyword5) != 0 )num5 <- sum(keyword5$freq) 
  if ( length(keyword6) != 0 )num6 <- sum(keyword6$freq) 
  
  if ( length(keyword1) != 0 || !is.null(nrow(keyword1 )) ) {
    if (is.null(temp) ) {
      temp <- keyword1
    } else {
      temp <- rbind( temp,keyword1)
    }
    
  }
  if ( length(keyword2) != 0 || !is.null(nrow(keyword2 ))) {
    if (is.null(temp) ) {
      temp <- keyword2
    } else {
      temp <- rbind( temp,keyword2)
    }
    
  }
  if ( length(keyword3) != 0 || !is.null(nrow(keyword3 ))) {
    if (is.null(temp) ) {
      temp <- keyword3
    } else {
      temp <- rbind( temp,keyword3)
    }
    
  }
  if ( length(keyword4) != 0|| !is.null(nrow(keyword4 )) ) {
    if (is.null(temp) ) {
      temp <- keyword4
    } else {
      temp <- rbind( temp,keyword4)
    }
    
  }
  if ( length(keyword5) != 0|| !is.null(nrow(keyword5 )) ) {
    if (is.null(temp) ) {
      temp <- keyword5
    } else {
      temp <- rbind( temp,keyword5)
    }
    
  }
  if ( length(keyword6) != 0|| !is.null(nrow(keyword6 )) ) {
    if (is.null(temp) ) {
      temp <- keyword6
    } else {
      temp <- rbind( temp,keyword6)
    }
    
  }
  killed_dfx <- unique( temp ) # combine the class1 and class2 word
  
  
  
  killed_dfx1 <- data.frame(unique( killed_dfx$word) )
  colnames(killed_dfx1 ) <- "word"
  remove(killed_dfx)
  commentwordfreq <- NULL 
  e = exp(1)
  killed_dfx1$weight1 = 0 
  killed_dfx1$weight2 = 0
  killed_dfx1$weight3 = 0
  killed_dfx1$weight4 = 0 
  killed_dfx1$weight5 = 0 
  killed_dfx1$weight6 = 0 
  killed_dfx1$num1 = 0 
  killed_dfx1$num2 = 0 
  killed_dfx1$num3 = 0 
  killed_dfx1$num4 = 0 
  killed_dfx1$num5 = 0 
  killed_dfx1$num6 = 0 
  
  killed_dfx1$classnum = 0
  for( i in 1:nrow(killed_dfx1) ) { #compute the keyword's weight
    a = 0 
    
    num = 0 
    if ( length(keyword1) != 0 ) {
      if ( length(which(keyword1$word %in% killed_dfx1[i,1]) ) != 0  ) { #class1 have this word
        num = num + 1 ; 
        killed_dfx1$num1[i] <- keyword1$freq[which(keyword1$word %in% killed_dfx1[i,1] )]
      } 
    }
    
    
    if ( length(keyword2) != 0 ) {
      if ( length(which(keyword2$word %in% killed_dfx1[i,1] ) )  != 0 ) { #class1 have this word
        num = num + 1 ;
        killed_dfx1$num2[i] <- keyword2$freq[which(keyword2$word %in% killed_dfx1[i,1] )]
        
      } 
    }
    
    if ( length(keyword3) != 0 ) {
      if ( length(which(keyword3$word %in% killed_dfx1[i,1] ) )  != 0 ) { #class1 have this word
        num = num + 1 ;
        killed_dfx1$num3[i] <- keyword3$freq[which(keyword3$word %in% killed_dfx1[i,1] )]
        
      } 
    }
    if ( length(keyword4) != 0 ) {
      if ( length(which(keyword4$word %in% killed_dfx1[i,1] ) )  != 0 ) { #class1 have this word
        num = num + 1 ;
        killed_dfx1$num4[i] <- keyword4$freq[which(keyword4$word %in% killed_dfx1[i,1] )]
        
      } 
    }
    if ( length(keyword5) != 0 ) {
      if ( length(which(keyword5$word %in% killed_dfx1[i,1] ) )  != 0 ) { #class1 have this word
        num = num + 1 ;
        killed_dfx1$num5[i] <- keyword5$freq[which(keyword5$word %in% killed_dfx1[i,1] )]
        
      } 
    }
    if ( length(keyword6) != 0 ) {
      if ( length(which(keyword6$word %in% killed_dfx1[i,1] ) )  != 0 ) { #class1 have this word
        num = num + 1 ;
        killed_dfx1$num6[i] <- keyword6$freq[which(keyword6$word %in% killed_dfx1[i,1] )]
        
      } 
    }
    killed_dfx1$classnum[i] <- num
    
    IDW <- log( 6/(num) + 0.1 )  # IDW
    if (length(keyword1) != 0)killed_dfx1$weight1[i] <- log( killed_dfx1$num1[i]/num1 + 1) * IDW # class1 weight
    if (length(keyword2) != 0)killed_dfx1$weight2[i] <- log( killed_dfx1$num2[i]/num2 + 1) * IDW # class2 weight
    if (length(keyword3) != 0)killed_dfx1$weight3[i] <- log( killed_dfx1$num3[i]/num3 + 1) * IDW # class1 weight
    if (length(keyword4) != 0)killed_dfx1$weight4[i] <- log( killed_dfx1$num4[i]/num4 + 1) * IDW # class2 weight    
    if (length(keyword5) != 0)killed_dfx1$weight5[i] <- log( killed_dfx1$num5[i]/num5 + 1) * IDW # class1 weight
    if (length(keyword6) != 0)killed_dfx1$weight6[i] <- log( killed_dfx1$num6[i]/num6 + 1) * IDW # class1 weight

    
  }
  
  if (length(which(killed_dfx1$classnum > type )) > 0 ) {
    killed_dfx1 <- killed_dfx1[-which(killed_dfx1$classnum > type ) ,]
  }
  
  
  return(killed_dfx1)
  
  
  
}
