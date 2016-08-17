read.SAScii3 <- 
  function( fn , sas_ri ){
      SASinput <- readLines(sas_ri)
  SASinput <- gsub("\t", " ", SASinput)
  SASinput <- SASinput[1:length(SASinput)]
  SASinput <- tolower(SASinput)
  firstline <- grep("input", SASinput)[1]
  a <- grep(";", toupper(SASinput))
  lastline <- min(a[a > firstline])
  FWFlines <- SASinput[firstline:lastline]
  input_word <- unlist(gregexpr("input", FWFlines[1], fixed = T))
  FWFlines[1] <- substr(FWFlines[1], input_word + 5, nchar(FWFlines[1]))
  semicolon <- unlist(gregexpr(";", FWFlines[length(FWFlines)], 
                               fixed = T))
  FWFlines[length(FWFlines)] <- substr(FWFlines[length(FWFlines)], 
                                       1, semicolon - 1)
  for (i in 1:length(FWFlines)) FWFlines[i] <- gsub("$", " $ ", 
                                                    FWFlines[i], fixed = T)
  for (i in 1:length(FWFlines)) FWFlines[i] <- gsub("-", " - ", 
                                                    FWFlines[i], fixed = T)
  FWFlines <- FWFlines[which(gsub(" ", "", FWFlines) != "")]
  Char=grepl("$",FWFlines)
  FWFlines <- gsub("$","",FWFlines,fixed=TRUE)
  FWFlines <- gsub(".","",FWFlines,fixed=TRUE)
  FWFlines <- gsub("@","",FWFlines)
  z <- t(sapply(FWFlines,function(x){unlist(strsplit(x,split =  "\\s+"))}))

y<-data.frame(varname=z[,2],start=z[,2],width=z[,3],char=Char)
    readr::read_fwf(fn,readr::fwf_widths(y$width,col_names=y$varname))}
