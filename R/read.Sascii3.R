read.SAScii3 <- 
  function( fn , sas_ri ){
    x <- parse.SAScii3( sas_ri)
    readr::read_fwf(fn,readr::fwf_widths(y$width,col_names=y$varname))}
