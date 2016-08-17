read.SAScii3 <- 
  function( fn , sas_ri ,sel=NULL,col_types=NULL){
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
  Char=grepl("$",FWFlines,fixed=TRUE)
  FWFlines <- gsub("$","",FWFlines,fixed=TRUE)
FWFlines <- gsub("\\.\\d+","",FWFlines)
FWFlines <- gsub("@","",FWFlines)
FWFlines <- gsub(".","",FWFlines,fixed=TRUE)
z <- t(sapply(FWFlines,function(x){unlist(strsplit(x,split =  "\\s+"))}))
if(!is.null(sel)){vars<-is.element(z[,2],sel)}else{vars=z[,2]}
y<-data.frame(varname=z[vars,2],start=strtoi(z[vars,1]),end=strtoi(z[vars,1])+strtoi(z[vars,3])-1,char=Char[vars],stringsAsFactors = FALSE)
col_types=col_types[z[,2][vars]]
X=readr::read_fwf(fn,readr::fwf_positions(start=y$start,end=y$end,col_names=y$varname),col_types = col_types)
        }
