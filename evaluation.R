evaluation <- function(test,dic) { #evaluation the experiment 
 # table1 <- c(length(which(train$label == 1 ) ) ,length(which(train$label == 0) ),"class 1"  ,bkt.num,nrow( test[ which(test$label == 1),] ),nrow( class1 ),nrow( class1[ which(class1$label == 1),]), nrow(class1[ which(class1$label == 1),])/nrow( class1 ),nrow( class1[ which(class1$label == 1),])/nrow( test[ which(test$label == 1),] ))
 # table2 <- c(length(which(train$label == 1) ) ,length(which(train$label == 0 ) ) ,"class 2" ,bkt.num,nrow( test[ which(test$label == 0),] ),nrow( class2 ),nrow( class2[ which(class2$label == 0),]), nrow(class2[ which(class2$label == 0),])/nrow( class2 ),nrow( class2[ which(class2$label == 0),])/nrow( test[ which(test$label == 0),] ))
 # precision <- nrow(class1[ which(class1$label == 1),])/nrow( class1 )
 # recall <- nrow( class1[ which(class1$label == 1),])/nrow( test[ which(test$label == 1),] )
  
  test$ans[which(test$ans== 6)] <- 0
  accuarcy <- nrow( test[ which(test$label == test$ans ),])/nrow(test)
  #table1 <- c( nrow(test),nrow( test[ which(test$label == 1),] ),nrow( class1 ),nrow( class1[ which(class1$label == 1),]), precision,recall, ( 2*precision *recall ) / (precision  + recall),length( which( dic$score > 0 ) ),length( which( dic$score < 0 ) ),length( which( dic$score > 0 ) )/length( which( dic$score < 0 ) ))
  #table2 <- c(length(which(train$label == 1) ) ,length(which(train$label == 0 ) ) ,"class 2" ,nrow(test),nrow( test[ which(test$label == 0),] ),nrow( class2 ),nrow( class2[ which(class2$label == 0),]), nrow(class2[ which(class2$label == 0),])/nrow( class2 ),nrow( class2[ which(class2$label == 0),])/nrow( test[ which(test$label == 0),] ),nrow(dic1), nrow(dic2),nrow(which(dic1$word %in% dic2$word)))
  table1 <- c( nrow(test),accuarcy)
  table1 <- rbind(table1,table1) 
  

  return(table1)
}


