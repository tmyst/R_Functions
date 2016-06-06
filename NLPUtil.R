# Reverse input string
library(stringr)
str_rev <- function(string){
  paste(rev(str_split(string, "")[[1]]), collapse="")
}
