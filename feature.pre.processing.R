feature.pre.processing <- function( data ) {
  cut <- function( temp ) { 
    num =1 
    word <- data.table(num,substr(temp,start=num,stop = num))
    colnames(word) <- c("position","word")
    num =2 
    while ( nchar(temp) >= num ) {
      temprow <- data.table(num,substr(temp,start=num,stop = num)) # cut word 
      colnames(temprow) <- c("position","word") 
      word <- rbind(word,temprow)
      num = num + 1 
      
    }
    return(word)
  }
  

  
  first <-proc.time()
  num = 1
  index = 1 

  
  word <- lapply(as.character(data$word),cut)
  
  word <- do.call(rbind,word)
  word$word <- gsub("[A-Za-z0-9]","", word$word) #replace the non-Chinese characters with empty characters
  word$word <- gsub("[[:punct:]]","", word$word)
  word$word <- gsub("ˋ","", word$word)
  word$word <- gsub("～","", word$word)
  word$word <- gsub("▽","", word$word)
  word$word <- gsub("→","", word$word)
  word$word <- gsub("ω","", word$word)
  word$word <- gsub("ˊ","", word$word)
  word$word <- gsub("\n","",word$word)
  word$word <- gsub(" ","",word$word)
  word$word <- gsub("　","",word$word)
  word$word <- gsub("ˇ","",word$word)
  word$word <- gsub("⊙","",word$word)
  word$word <- gsub("＄","",word$word)
  word$word <- gsub("═","",word$word)
  word$word <- gsub("│","",word$word)
  word$word <- gsub("║","",word$word)
  word$word <- gsub("◤","",word$word)
  word$word <- gsub("◣","",word$word)
  word$word <- gsub("█","",word$word)
  word$word <- gsub("◎","",word$word)
  word$word <- gsub("╥","",word$word)
  word$word <- gsub("─","",word$word)
  word$word <- gsub("〒","",word$word)
  word$word <- gsub("♀","",word$word)
  word$word <- gsub("０","",word$word)
  word$word <- gsub("１","",word$word)
  word$word <- gsub("２","",word$word)
  word$word <- gsub("３","",word$word)
  word$word <- gsub("４","",word$word)
  word$word <- gsub("５","",word$word)
  word$word <- gsub("６","",word$word)
  word$word <- gsub("７","",word$word)
  word$word <- gsub("８","",word$word)
  word$word <- gsub("９","",word$word)
  word$word <- gsub("ｂ","",word$word)
  word$word <- gsub("ｆ","",word$word)
  word$word <- gsub("＝","",word$word)
  word$word <- gsub("＞","",word$word)
  word$word <- gsub("◥","",word$word)
  word$word <- gsub("＞","",word$word)
  word$word <- gsub("◢","",word$word)
  word$word <- gsub("●","",word$word)
  word$word <- gsub("▲","",word$word)
  word$word <- gsub("○","",word$word)
  word$word <- gsub("△","",word$word)
  word$word <- gsub("□","",word$word)
  word$word <- gsub("∩","",word$word)
  word$word <- gsub("∮","",word$word)
  word$word <- gsub("≡","",word$word)
  word$word <- gsub("≠","",word$word)
  word$word <- gsub("∫","",word$word)
  
  word$word <- gsub("▼","",word$word)
  word$word <- gsub("◆","",word$word)
  word$word <- gsub("≦","",word$word)
  word$word <- gsub("≧","",word$word)
  word$word <- gsub("＜","",word$word)
  word$word <- gsub("∕","",word$word)
  word$word <- gsub("＋","",word$word)
  word$word <- gsub("＄","",word$word)
  word$word <- gsub("￣","",word$word)
  word$word <- gsub("˙","",word$word)
  word$word <- gsub("€","",word$word)
  word$word <- gsub("↑","",word$word)
  word$word <- gsub("↓","",word$word)
  word$word <- gsub("←","",word$word)
  word$word <- gsub("↘","",word$word)
  word$word <- gsub("↗","",word$word)
  word$word <- gsub("←","",word$word)
  word$word <- gsub("ˍ","",word$word)
  word$word <- gsub("Ｘ","",word$word)
  word$word <- gsub("═","",word$word)
  word$word <- gsub("＜","",word$word)
  
  temp <- read.csv("removesymbol.csv")
  i <- 1
  
  if ( length(which("word"%in% temp$Var1) != 0 ) ) {


    word =   word[-which("word"%in% temp$Var1)]
  }
  # stopword <- as.data.table( readLines("stop_words.utf8",encoding ="UTF-8") )
  # colnames(stopword) <- "word"
  # if ( length(which(word$word %in% stopword$word) ) != 0 ) {
  #   word <- word[ -which(word$word %in% stopword$word),]
  # }
  # 
  word$position <- row.names(word)
  word <- word[ word != "" ] # delete the null word
  
  
  end <- proc.time()
  time <- end - first
  return(word)
}
  