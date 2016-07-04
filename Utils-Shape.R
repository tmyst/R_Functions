# Utils_Shape.R
# 0704
# Author:
require(stringr)
require(plyr)
require(tidyr)
require(dplyr)

dat <- airquality
dat$Tempc <- dat$Temp %>%
  cut(breaks=c(50,60,70,80,90,100), labels=c('50-60', '60-70', '70-80', '80-90', '90-100')) %>%
  droplevels
# dat$Ozone %>% median(na.rm=T)
# [1] 31.5

ta <- with(data=dat, table(Ozone=Ozone>30, Month, exclude=NULL))
fta <- with(data=dat, ftable(Ozone=Ozone>30, Tempc, Month, exclude=NULL));fta

# Month  5  6  7  8  9 NA
# Ozone Tempc                         
# FALSE 50-60         4  0  0  0  0  0
# 60-70        12  1  0  0  7  0
# 70-80         3  3  3  4 12  0
# 80-90         0  2  2  3  1  0
# 90-100        0  0  0  0  0  0
# NA            0  0  0  0  0  0
# TRUE  50-60         0  0  0  0  0  0
# 60-70         3  0  0  0  0  0
# 70-80         3  1  0  5  1  0
# 80-90         1  2 19 10  4  0
# 90-100        0  0  2  4  4  0
# NA            0  0  0  0  0  0
# NA    50-60         4  0  0  0  0  0
# 60-70         1  1  0  0  0  0
# 70-80         0 14  0  2  1  0
# 80-90         0  4  4  2  0  0
# 90-100        0  2  1  1  0  0
# NA            0  0  0  0  0  0

# set labels to table's columns, rows
setLabs_table <- function(tab, xlab, ylab){
  names(dimnames(tab))[1] <- xlab
  names(dimnames(tab))[2] <- ylab
  return(tab)
} 
setLabs_ftable <- function(ftab, xlabs, ylabs){
  n_rowvars <- attributes(ftab)[['row.vars']] %>% length
  n_colvars <- attributes(ftab)[['col.vars']] %>% length
  if((n_rowvars!=length(xlabs))|(n_colvars!=length(ylabs))){
    message('Error, incompatible length of vars')
    return(NA)
  }else{
    names(attributes(x = ftab)[['row.vars']]) <- xlabs
    names(attributes(x = ftab)[['col.vars']]) <- ylabs
  }
  return(ftab)
}
setLabs <- function(tab, xlabs, ylabs){
  if('ftable' %in% class(tab)){
    setLabs_ftable(tab, xlabs, ylabs)
  }else if('table' %in% class(tab)){
    setLabs_table(tab, xlabs, ylabs)
  }else{
    message('Error, incompatible type of input table.')
    return(NA)
  }
}

# table or ftable to data.frame
tab2df_table <- function(tab){
  temprownm <- names(attributes(tab)[['dimnames']])[1]
  tempcolnm <- names(attributes(tab)[['dimnames']])[2]
  newdf <- as.data.frame(tab, stringsAsFactors = F) %>% 
    tidyr::spread_(key=tempcolnm, value='Freq') 
  attributes(newdf)$colnm_spread <- tempcolnm
  attributes(newdf)$colnm_spr_len <- ncol(tab)
  return(newdf)
}
tab2df_ftable <- function(ftab){
  temprownm <- names(attributes(ftab)[['row.vars']])
  tempcolnm <- names(attributes(ftab)[['col.vars']])
  newdf <- as.data.frame(ftab, stringsAsFactors = F) %>% 
    tidyr::spread_(key=tempcolnm, value="Freq")
  attributes(newdf)$colnm_spread <- tempcolnm
  attributes(newdf)$colnm_spr_len <- ncol(ftab)
  return(newdf)
}
tab2df <- function(tab){
  if('ftable' %in% class(tab)){
    tab2df_ftable(tab)
  }else if('table' %in% class(tab)){
    tab2df_table(tab)
  }else{
    message('Error, incompatible type of input table.')
    return(NA)
  }
}

# add prefix in column names if 'colnm_spread' found
addSpreadLab <- function(df, sep='_', pos='forward', prefix=NULL){
  if(all(c('colnm_spread', 'colnm_spr_len') %in% names(attributes(df)))){
    tempn <- seq(ncol(df)-attributes(df)$colnm_spr_len+1, ncol(df), 1)
    tempc <- ifelse(is.null(prefix), attributes(df)$colnm_spread, prefix)
    if(pos=='forward'){
      names(df)[tempn] <- paste0(tempc, sep, names(df)[tempn])
    }else if(pos=='back'){
      names(df)[tempn] <- paste0(names(df)[tempn], sep, tempc)
    }
    return(df)
  }else{
    message('Error, no spreaded columns')
    return(NA)
  }
}

# data.frame foramat conversion for output
unlabel_rep <- function(vec){
  vec <- as.character(vec)
  vec[which(dplyr::lag(vec)==vec)] <- ''
  vec
}
replaceNA <- function(vec, repstr="NA"){
  vec <- as.character(vec)
  vec[is.na(vec)] <- repstr
  vec
}
lapply_wospread <- function(df, func){
  if(!is.null(attributes(df)$colnm_spr_len)){
    lapply(df[1:(ncol(df)-attributes(df)$colnm_spr_len)], func)
  }
}
mutate_wospread <- function(df, func){
  if(!is.null(attributes(df)$colnm_spr_len)){
    for(i in 1:(ncol(df)-attributes(df)$colnm_spr_len)){
      df[[i]] <- func(df[[i]])
    }
  }
  df
}
decol_table <- function(df){
  if(all(c('colnm_spread', 'colnm_spr_len') %in% names(attributes(df)))){
    c_s <- attributes(df)$colnm_spread
    c_s_l <- attributes(df)$colnm_spr_len
    tempn <- seq(ncol(df)-attributes(df)$colnm_spr_len+1, ncol(df), 1)
    
    row2 <- names(df)
    row1 <- c(rep('', ncol(df)-length(tempn)), rep(attributes(df)$colnm_spread, length(tempn)))
    bod <- df %>% as.matrix
    
    newdf <- as.data.frame(rbind(row1,row2,bod), stringsAsFactors=F)
    attributes(newdf)$colnm_spread <- c_s
    attributes(newdf)$colnm_spr_len <- c_s_l
    newdf <- mutate_wospread(newdf, replaceNA)
    newdf <- mutate_wospread(newdf, unlabel_rep)
    return(newdf)
  }else{
    message('Error, no spread columns')
    return(NA)
  }
}

# example
fta %>% tab2df %>% addSpreadLab(pos='back', prefix='gatsu', sep='') %>% 
  decol_table %>% 
  write.table(sep='\t', col.names=F, row.names=F, file="clipboard")