fillResultat <- function(data,IDColumn,analysColumn,analysValue,resultColumn){
  
  rows <- which(data[,analysColumn]==analysValue)
  ID.value <- data[rows,c(IDColumn,resultColumn)]
  
  colnames.data <- colnames(data)
  data <- cbind(data,NA)
  colnames(data)<- c(colnames.data,analysValue)
  data[,analysValue]<-as.character(data[,analysValue])
  
  for (i in 1:dim(ID.value)[1]){
    fill.rows <- which(data[,IDColumn]==ID.value[i,1])
    data[fill.rows,analysValue]<-ID.value[i,2]
      }
  
  data <- data[-rows,]
  
  return(data)
  
}



eliminate.swedish.the.wrong.way <- function(x){
  x<- str_replace(x,"Ö","O")
  x<- str_replace(x,"Ä","A")
  x<- str_replace(x,"Å","A")
  x<- str_replace(x,"ö","o")
  x<- str_replace(x,"ä","a")
  x<- str_replace(x,"å","a")
  return(x)
}