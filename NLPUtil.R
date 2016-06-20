# Reverse input string
library(stringr)

str_rev <- function(string){
  paste(rev(str_split(string, "")[[1]]), collapse="")
}

gz_unzip <- function(gzf){
  fn1 <-  gzf
  fn2 <-  sub('\\.gz$', '', fn1)
  
  con <-  gzfile(fn1, 'rb')
  out <-  file(fn2, 'wb')
  while (length(buf <- readBin(con=con, what="raw", n=0x8000)) > 0){
    writeBin(buf, out)
  }
  close(con)
  close(out)
}