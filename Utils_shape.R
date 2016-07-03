# 0703 2331
# ftab <- with(data=x, ftable(a, b, flag))
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
  if(class(tab)=='ftable'){
    setLabs_ftable(tab, xlabs, ylabs)
  }else if(class(tab)=='table'){
    setLabs_table(tab, xlabs, ylabs)
  }else{
    message('Error, incompatible type of input table.')
    return(NA)
  }
}

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
  if(class(tab)=='ftable'){
    tab2df_ftable(tab)
  }else if(class(tab)=='table'){
    tab2df_table(tab)
  }else{
    message('Error, incompatible type of input table.')
    return(NA)
  }
}
# df <- tab2df(ftab)

refLabs_spreaded <- function(df, sep='_'){
  if(all(c('colnm_spread', 'colnm_spr_len') %in% names(attributes(df)))){
    tempn <- seq(ncol(df)-attributes(df)$colnm_spr_len+1, ncol(df), 1)
    tempc <- attributes(df)$colnm_spread
    names(df)[tempn] <- paste0(tempc, sep, names(df)[tempn])
    attributes(df)$colnm_spr_len <- NULL
  }else{
    message('Error, no spreaded columns')
    return(NA)
  }
}

decol_table <- function(df){
  if(all(c('colnm_spread', 'colnm_spr_len') %in% names(attributes(df)))){
    tempn <- seq(ncol(df)-attributes(df)$colnm_spr_len+1, ncol(df), 1)
    row2 <- names(df)
    row1 <- c(rep('', ncol(df)-length(tempn)), rep(attributes(df)$colnm_spread, length(tempn)))
    bod <- df %>% as.matrix
    rbind(row1,row2,bod) %>% as.data.frame
  }else{
    message('Error, no spread columns')
    return(NA)
  }
}