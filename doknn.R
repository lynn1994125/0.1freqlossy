
do <- function( data1,data0,min.sup,type,min.del) {
  first <- TRUE # The first time to initial Temporary table 
  keyword1 <- NULL
  keyword2 <- NULL
  keyword3 <- NULL
  keyword4 <- NULL
  keyword5 <- NULL
  keyword6 <- NULL
  bkt.done = TRUE
  index = 1 
  train <- data1
  test <- data0
  
  wordall1 <- keyword1
  wordall2 <- keyword2
  wordall3 <- keyword3
  wordall4 <- keyword4
  wordall5 <- keyword5
  wordall6 <- keyword6
  word1 <- NULL
  word2 <- NULL
  word3 <- NULL
  word4 <- NULL
  word5 <- NULL
  word6 <- NULL
  train1 <- train[which(train$label == 1 ),] #training data's class 1 
  train2 <- train[which(train$label == 2 ),] #training data's class 2 
  train3 <- train[which(train$label == 3 ),] #training data's class 2 
  train4 <- train[which(train$label == 4 ),] #training data's class 2 
  train5 <- train[which(train$label == 5 ),] #training data's class 2 
  train6 <- train[which(train$label == 0 ),] #training data's class 2 
  
  temp1 <- NULL
  temp2 <- NULL
  temp3 <- NULL
  temp4 <- NULL
  temp5 <- NULL
  temp6 <- NULL
  
  message("pre.processing " )
  if ( nrow(train1) != 0 ) word1 <- feature.pre.processing(train1) #pre-process single word the position in the article
  if ( nrow(train2) != 0 ) word2 <- feature.pre.processing(train2)
  if ( nrow(train3) != 0 ) word3 <- feature.pre.processing(train3)
  if ( nrow(train4) != 0 ) word4 <- feature.pre.processing(train4)
  if ( nrow(train5) != 0 ) word5 <- feature.pre.processing(train5)
  if ( nrow(train6) != 0 ) word6 <- feature.pre.processing(train6)
  
  bkt.num1 <- nrow( train1 )
  bkt.num2 <- nrow( train2 )
  bkt.num3 <- nrow( train3 )
  bkt.num4 <- nrow( train4 )
  bkt.num5 <- nrow( train5 )
  bkt.num6 <- nrow( train6 )
  message("feature.extraction" )
  z = 1 
  if ( bkt.num1 != 0 ) keyword1 <- feature.extraction(word1,bkt.num1,z,min.sup) 
  z = z + 1 
  if ( bkt.num2 != 0 ) keyword2 <- feature.extraction(word2,bkt.num2,z,min.sup)
  z = z + 1 
  if ( bkt.num3 != 0 ) keyword3 <- feature.extraction(word3,bkt.num3,z,min.sup) 
  z = z + 1 
  if ( bkt.num4 != 0 ) keyword4 <- feature.extraction(word4,bkt.num4,z,min.sup) 
  z = z + 1 
  if ( bkt.num5 != 0 ) keyword5 <- feature.extraction(word5,bkt.num5,z,min.sup) 
  z = z + 1 
  if ( bkt.num6 != 0 ) keyword6 <- feature.extraction(word6,bkt.num6,z,min.sup) 
  write.csv(keyword1,"區間處理1.csv")
  message("lossy.counting" )
  if ( bkt.num1 != 0 )wordall1 <- lossy.counting(wordall1,keyword1,bkt.num1,min.del,bkt.num1,train1 )
  if ( bkt.num2 != 0 )wordall2 <- lossy.counting(wordall2,keyword2,bkt.num2,min.del,bkt.num2,train2 )
  if ( bkt.num3 != 0 )wordall3 <- lossy.counting(wordall3,keyword3,bkt.num3,min.del,bkt.num3,train3 )
  if ( bkt.num4 != 0 )wordall4 <- lossy.counting(wordall4,keyword4,bkt.num4,min.del,bkt.num4,train4 )
  if ( bkt.num5 != 0 )wordall5 <- lossy.counting(wordall5,keyword5,bkt.num5,min.del,bkt.num5,train5 )
  if ( bkt.num6 != 0 )wordall6 <- lossy.counting(wordall6,keyword6,bkt.num6,min.del,bkt.num6,train6 )
  write.csv(wordall1,"潛在字典更新1.csv")
  message("lossy.counting.delete" )
  if ( bkt.num1 != 0 )keyword1 <- lossy.counting.delete( wordall1,bkt.num1,min.sup,min.del,2 )
  if ( bkt.num2 != 0 )keyword2 <- lossy.counting.delete( wordall2,bkt.num2,min.sup,min.del,2 )
  if ( bkt.num3 != 0 )keyword3 <- lossy.counting.delete( wordall3,bkt.num3,min.sup,min.del,2 )
  if ( bkt.num4 != 0 )keyword4 <- lossy.counting.delete( wordall4,bkt.num4,min.sup,min.del,2 )
  if ( bkt.num5 != 0 )keyword5 <- lossy.counting.delete( wordall5,bkt.num5,min.sup,min.del,2 )
  if ( bkt.num6 != 0 )keyword6 <- lossy.counting.delete( wordall6,bkt.num6,min.sup,min.del,2 )
  
  write.csv(keyword1,"特徵字典更新1.csv")
  
  
  z = 1 
  test <- test[order(test$month,decreasing = F),]
  test$weight1<- 0
  test$weight2<- 0
  test$weight3<- 0
  test$weight4<- 0
  test$weight5<- 0
  test$weight6<- 0
  train$ans <- 0
  train$buketnum <- 0
  test$buketnum <- 0
  test$ans <- 0 
  zzz = 1
  killed_dfx <- dictionary(keyword1,keyword2,keyword3,keyword4,keyword5,keyword6,type) # Generate a dictionary
  write.csv( killed_dfx, paste( "dictionary",zzz , ".csv",sep = "") )
 # linearmodel <- model( killed_dfx, train )
  updatenum <- 1 
  
  while ( z <= nrow( test )  ) {
    message(paste( "now is  : ", z ,sep = "") )
    #temptest <- feature(killed_dfx, test[z,])
    #temptest1 <- temptest[,c("hashtag","titlenum","articlenum","exclamations","question","link" )]
    #temptest2 <- temptest[,(ncol(temptest)-nrow(killed_dfx) + 1):ncol(temptest)]
    #temptest <- cbind( temptest1,temptest2)
    update <- 0
    test[z,] <-  test.weight.calculation(test[z,],killed_dfx) # testing data weight calculation 
    #---------------------------------------------------------------minweight ------------------------------------------
    # class1 <- test1[  which(test1$weight1 >= minweight$minweight[1] ) ,]#classification1
    # class2 <- test1[  which(test1$weight2 >= minweight$minweight[2] ) ,]
    # class3 <- test1[  which(test1$weight3 >= minweight$minweight[3] ) ,]
    # class4 <- test1[  which(test1$weight4 >= minweight$minweight[4] ) ,]
    # class5 <- test1[  which(test1$weight5 >= minweight$minweight[5] ) ,]
    # class6 <- test1[  which(test1$weight6 >= minweight$minweight[6] ) ,]
    # class7 <- test1[  which(test1$weight7 >= minweight$minweight[7] ) ,]
    # class8 <- test1[  which(test1$weight8 >= minweight$minweight[8] ) ,]
    #---------------------------------------------------------------minweight ------------------------------------------
    #test$ans[z] <- as.character( predict(linearmodel,newdata =  temptest,type = "class") )
    
    test$ans[z] <-  which.max(test[z,21:26])
    if ( (z %% 100) == 0 || nrow(test) == z ) {
      
      message("result.table" )
      if ( nrow(test) != z) {
        eva <- test[(z-99):z,]
        
      } else if ( nrow(test) == z){
        eva <- test[ (z-(z %% 100)+1) :z,] 
      }
      result.table <- evaluation(eva,killed_dfx) 
      colnames(result.table) <- c("all", "accuracy")
      if ( first ) {
        temp.table <- NULL # Temporary table 
      } 
      
      first <- FALSE
      result.table <- rbind( temp.table, result.table) # combine the previous table and the current table
      result.table <- as.data.table((result.table))
      temp.table <- result.table
    } 
    
    
    
    
    
    if ( test$label[z] == 1) {
      test$buketnum[z] <- as.integer(bkt.num1 / 100) + 1 
      bkt.num1 <- bkt.num1 + 1
      if ( is.null(temp1) ) {
        temp1 <- test[z,]
      } else {
        temp1 <- rbind(temp1,test[z,])
      }
      if ( nrow(temp1) == 100 ) {
        
        word1 <- feature.pre.processing( temp1) #pre-process single word the position in the article
        
        
        keyword1 <- feature.extraction(word1,nrow(temp1),z,min.sup) 
        #wordall1 <- potential.dic(wordall1,keyword1, min.sup,bkt.num1 )

        
        
        wordall1 <- lossy.counting(wordall1,keyword1,bkt.num1,min.del,nrow(temp1),temp1 )
   
        #keyword1 <- feature.dic( wordall1,bkt.num1,min.sup,keyword1,temp1)
        wordall1 <- lossy.counting.delete( wordall1,bkt.num1,min.sup,min.del,1 ) # yword = dictionary     wordall1 = all
   
        keyword1 <- lossy.counting.delete( wordall1,bkt.num1,min.sup,min.del,2 ) # yword = dictionary     wordall1 = all

        temp1 <- NULL
        update <- 1

      }
    }
    
    
    
    
    
    if ( test$label[z] == 2) {
      test$buketnum[z] <- as.integer(bkt.num2 / 100) + 1 
      bkt.num2 <- bkt.num2 + 1
      
      if ( is.null(temp2) ) {
        temp2 <-  test[z,]
      } else {
        temp2 <- rbind(temp2,test[z,])
      }
      if ( nrow(temp2) == 100 ) {
        
        word2 <- feature.pre.processing(temp2)
        
        keyword2 <- feature.extraction(word2,nrow(temp2),z,min.sup) 
        #bkt.num2 <- bkt.num2 + nrow(temp2)
        #wordall2 <- potential.dic(wordall2,keyword2, min.sup,bkt.num2 )
        
        
        wordall2 <- lossy.counting(wordall2,keyword2,bkt.num2,min.del,nrow(temp2),temp2 )
        #keyword2 <- feature.dic( wordall2,bkt.num2,min.sup)
        wordall2 <- lossy.counting.delete( wordall2,bkt.num2,min.sup,min.del,1 ) # yword = dictionary     wordall1 = all
        keyword2 <- lossy.counting.delete( wordall2,bkt.num2,min.sup,min.del,2 ) # yword = dictionary     wordall1 = all
        temp2 <- NULL
        update <- 1
      }
    }
    
    
    
    
    
    if ( test$label[z] == 3) {
      test$buketnum[z] <- as.integer(bkt.num3 / 100) + 1 
      bkt.num3 <- bkt.num3 + 1
      if ( is.null(temp3) ) {
        temp3 <- test[z,]
      } else {
        temp3 <- rbind(temp3,test[z,])
      }
      if ( nrow(temp3) == 100 ) {
        
        word3 <- feature.pre.processing(temp3)
        
        keyword3 <- feature.extraction(word3,nrow(temp3),z,min.sup) 
        #bkt.num3 <- bkt.num3 + nrow(temp3)
        #wordall3 <- potential.dic(wordall3,keyword3 , min.sup,bkt.num3)
        
        
        wordall3 <- lossy.counting(wordall3,keyword3,bkt.num3,min.del,nrow(temp3),temp3 )
        #keyword3 <- feature.dic( wordall3,bkt.num3,min.sup)
        wordall3 <- lossy.counting.delete( wordall3,bkt.num3,min.sup,min.del,1 ) # yword = dictionary     wordall1 = all
        keyword3 <- lossy.counting.delete( wordall3,bkt.num3,min.sup,min.del,2 ) # yword = dictionary     wordall1 = all
        temp3 <- NULL
        update <- 1
      }
    }
    
    
    if ( test$label[z] == 4) {
      test$buketnum[z] <- as.integer(bkt.num4 / 100) + 1 
      bkt.num4 <- bkt.num4 + 1
      if ( is.null(temp4) ) {
        temp4 <- test[z,]
      } else {
        temp4 <- rbind(temp4,test[z,])
      }
      if ( nrow(temp4) == 100 ) {
        
        word4 <- feature.pre.processing(temp4)
        
        keyword4 <- feature.extraction(word4,nrow(temp4),z,min.sup) 
        #bkt.num4 <- bkt.num4 + nrow(temp4)
        #wordall4 <- potential.dic(wordall4,keyword4, min.sup,bkt.num4 )
        
        #keyword4 <- feature.dic( wordall4,bkt.num4,min.sup)
        wordall4 <- lossy.counting(wordall4,keyword4,bkt.num4,min.del ,nrow(temp4),temp4)
        wordall4 <- lossy.counting.delete( wordall4,bkt.num4,min.sup,min.del,1 ) # yword = dictionary     wordall1 = all
        keyword4 <- lossy.counting.delete( wordall4,bkt.num4,min.sup,min.del,2 ) # yword = dictionary     wordall1 = all
        temp4 <- NULL
        update <- 1
      }
    }
    
    
    
    if ( test$label[z] == 5) {
      test$buketnum[z] <- as.integer(bkt.num5 / 100) + 1 
      bkt.num5 <- bkt.num5 + 1
      if ( is.null(temp5) ) {
        temp5 <- test[z,]
      } else {
        temp5 <- rbind(temp5,test[z,])
      }
      if ( nrow(temp5) == 100 ) {
        
        word5 <- feature.pre.processing(temp5)
        
        keyword5 <- feature.extraction(word5,nrow(temp5),z,min.sup) 
        #bkt.num5 <- bkt.num5 + nrow(temp5)
        #wordall5 <- potential.dic(wordall5,keyword5 , min.sup,bkt.num5)
        
        #keyword5 <- feature.dic( wordall5,bkt.num5,min.sup)
        wordall5 <- lossy.counting(wordall5,keyword5,bkt.num5,min.del,nrow(temp5),temp5 )
        
        wordall5 <- lossy.counting.delete( wordall5,bkt.num5,min.sup,min.del,1 ) # yword = dictionary     wordall1 = all
        
        keyword5 <- lossy.counting.delete( wordall5,bkt.num5,min.sup,min.del,2 ) # yword = dictionary     wordall1 = all
        temp5 <- NULL
        update <- 1
      }
      
    }
    
    if ( test$label[z] == 0 ) {
      test$buketnum[z] <- as.integer(bkt.num6 / 100) + 1 
      bkt.num6 <- bkt.num6 + 1
      if ( is.null(temp6) ) {
        temp6 <- test[z,]
      } else {
        temp6 <- rbind(temp6,test[z,])
      }
      if ( nrow(temp6) == 100 ) {
        
        word6 <- feature.pre.processing(temp6)
        
        keyword6 <- feature.extraction(word6,nrow(temp6),z,min.sup) 
        #bkt.num6 <- bkt.num6 + nrow(temp6)
        #wordall6 <- potential.dic(wordall6,keyword6 , min.sup,bkt.num6)
        
        #keyword6 <- feature.dic( wordall6,bkt.num6,min.sup)
        wordall6 <- lossy.counting(wordall6,keyword6,bkt.num6,min.del,nrow(temp6),temp6 )
        
        wordall6 <- lossy.counting.delete( wordall6,bkt.num6,min.sup,min.del,1 ) # yword = dictionary     wordall1 = all
        
        keyword6 <- lossy.counting.delete( wordall6,bkt.num6,min.sup,min.del,2 ) # yword = dictionary     wordall1 = all
        temp6 <- NULL
        update <- 1
      }
      
    }
    
    
    if ( update == 1 ) {
      
      zzz = zzz + 1 
      killed_dfx <- dictionary(keyword1,keyword2,keyword3,keyword4,keyword5,keyword6,type) # Generate a dictionary
      write.csv( killed_dfx, paste( "dictionary",zzz,".csv",sep = "") )
      
      #train <- rbind(train,test[updatenum:z,])
      
      #linearmodel <- model( killed_dfx, train )
      updatenum <- z+1
    }
    
    
    
    
    z = z + 1 
  }
  test$ans[which(test$ans== 6)] <- 0
  write.csv(wordall1, "請益潛在字典.csv")
  write.csv(wordall2, "情報潛在字典.csv")
  write.csv(wordall3, "閒聊潛在字典.csv")
  write.csv(wordall4, "心得潛在字典.csv")
  write.csv(wordall5, "徵求潛在字典.csv")
  write.csv(wordall6, "其他潛在字典.csv")
  write.csv(test, "test.csv")
  return(result.table)
}
