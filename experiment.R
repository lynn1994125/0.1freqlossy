setwd("C:\\Users\\user\\Desktop\\多類別分類")
source("test.weight.calculation.R", encoding = "UTF-8")
source("evaluation.R", encoding = "UTF-8")

experiment <- function( killed_dfx,minweight, test ) {

  
  test <-  test.weight.calculation(test,killed_dfx) # testing data weight calculation 
  test$ans <- 0
  i = 1 
  while( nrow(test) >= i ) {
    test$ans[i] <-  which.max(test[i,c("weight1","weight2","weight3","weight4","weight5","weight6","weight7","weight8")])
    i = i + 1
  }

  result.table <- evaluation(test,killed_dfx) 
  colnames(result.table) <- c("all", "accuracy")
  
  return(result.table)
  
  
  
  
  
}
y <- 1 
z <- 1 

while( y <= 2 ) {
  while( z <= 8 ) {
    killed_dfx <- read.csv(paste( ".\\dic\\dictionary",y,"000-5-",z,".csv",sep = "") )
    killed_dfx <- killed_dfx[,-1]
    killed_dfx$word <- as.character(killed_dfx$word )
    minweight <- read.csv( paste( ".\\weight\\minweight",y,"000-5-",z,".csv",sep = "") )
    x = 1 
    
    
    while ( x <= 10 ) {
      test <- read.csv(paste( ".\\graduate\\預設標籤\\graduate2017sample",x,".csv", sep = "") )
      test <- test[,-1]
      test <- test[,-1]
      test$label <- 0
      test$label[which(test$subclass %in% "請益" )] <- 1 
      test$label[which(test$subclass %in% "情報" )] <- 2 
      test$label[which(test$subclass %in% "討論" )] <- 3 
      test$label[which(test$subclass %in% "閒聊" )] <- 4 
      test$label[which(test$subclass %in% "心得" )] <- 5 
      test$label[which(test$subclass %in% "建議" )] <- 6 
      test$label[which(test$subclass %in% "徵求" )] <- 7 
      test$label[which(test$subclass %in% "公告" )] <- 8 
      
      colnames(test) <- c( "id"       ,     "title"    ,    "author"    ,   "time"    ,     "class"       , "excerpt"   ,   "url"        ,  "hashtag"     , "titlenum"     ,"articlenum",   "exclamations","question"  ,   "link" ,        "subclass"  ,   "month"   ,     "year"   ,      "date" ,        "ymd"      ,    "word"     ,    "label"    )
     
      
      do <- experiment( killed_dfx,minweight, test ) 
      do <- do[ -c ( 1 : (nrow(do)/2) ) * 2, ]  # delete the non classification's evaluation
      if ( x == 1 ) {
        eva <- do
      } else {
        eva <- rbind(eva,do)
      }
      message( paste("Do:", y, "-" , z , sep = "" ) )
      x = x + 1 
    }
    
    
    eva <- as.data.table((eva))
    colnames(eva) <- c("all", "label","pick","pickright","precision","recall","F-measure","dic1","dic2","dic1dic2intersect")
    row.names(eva) <- NULL
    write.csv(eva, paste(".\\result\\result",y,"000.0.",z,"-10.csv", sep = "") )
    
    
    
    
    z = z + 1
  }
  message( paste( " y :", y , " z : " , z))
  z = 1 
  y = y + 1 
}


