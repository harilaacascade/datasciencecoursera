filenames <- function(x) {
  x <- as.character(x)
  if (nchar(x) == 3) { 
    y <- paste(x,".csv",sep="")
  }
  else if (nchar(x) == 2) { 
    y <- paste("0",x,".csv",sep="") 
  }
  else if (nchar(x) == 1) { 
    y <- paste("00",x,".csv",sep="") 
  }
  #print(y)
}

fdir <- function(directory, id) {
  y <- vector("character", length = length(id))
  
  for (iditem in seq_along(id))
    y[iditem] <- filenames(id[iditem])
  return (y)
}


corr <- function(directory, threshold = 0) {
  
  if (!dir.exists(directory)) {
    print("ERR: Directory does not exist")
    return(NULL)
  }
  
  id <- c(1:332)
  
  filelist <- fdir(directory, id)
  x <- data.frame()
  if (!is.null(filelist)) {
    for (f in filelist) {
      flname <- paste(directory,f,sep="/")
      y <- read.csv(flname)
      x <- rbind(x,y)
    }
  }
  
  completeCases <- x[!is.na(x$sulfate) & !is.na(x$nitrate), ]
  #print(interm)
  tmp <- data.frame()
  final <- data.frame()
  
  for (iditem in id) {
    tmp <- data.frame(id = iditem, nobs = nrow(completeCases[completeCases["ID"] == iditem,]))
    
    final <- rbind(final, tmp)
  }
  
  threshdf <- final[final["nobs"] > threshold, ]
  
  abcor <- vector("numeric", length=0)
  uniqIDs <- unique(threshdf$id)
  #print(uniqIDs)
  for (iditem in uniqIDs) {
    a <- completeCases[completeCases["ID"] == iditem, ][["sulfate"]]
    b <- completeCases[completeCases["ID"] == iditem, ][["nitrate"]]
    abcor <- c(abcor,cor(a,b))
  }
  abcor
}
