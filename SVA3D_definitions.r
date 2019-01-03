run.local=F

sourcefiles <- ifelse(run.local==T,"I:/ESS/","/media/i/ESS/")



base.species <- c("Får","Get","Gris","Hund","Häst","Kalkon","Katt","Nötkreatur","Tamhöns")




eliminate.swedish.the.wrong.way <- function(x){
  x<- str_replace(x,"Ö","O")
  x<- str_replace(x,"Ä","A")
  x<- str_replace(x,"Å","A")
  x<- str_replace(x,"ö","o")
  x<- str_replace(x,"ä","a")
  x<- str_replace(x,"å","a")
  return(x)
}


fix.encoding <- function(df, originalEncoding = "latin1") {
  numCols <- ncol(df)
  df <- data.frame(df)
  for (col in 1:numCols)
  {
    if(class(df[, col]) == "character"){
      Encoding(df[, col]) <- originalEncoding
    }
    
    if(class(df[, col]) == "factor"){
      Encoding(levels(df[, col])) <- originalEncoding
    }
  }
  return(df)
}


