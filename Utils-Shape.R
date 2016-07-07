# Filename   : Utils-Shape.R
# Last update: 20160707
# Author     : tmyst

# ---------Preparation---------
# Load library
require(stringr)
require(lazyeval)
require(openxlsx)
require(plyr)
require(tidyr)
require(dplyr)

  # dat <- airquality
  # dat$Tempf <- as.integer(dat$Temp > median(dat$Temp))
  # dat$Windf <- as.integer(dat$Wind > median(dat$Wind))
  # dat$Tempc <- cut(dat$Temp, breaks=c(50,60,70,80,90,100), labels=c('50-60', '60-70', '70-80', '80-90', '90-100')) %>% droplevels
  # dat$Ozonc <- cut(dat$Ozone, breaks = c(0,20,40,80,160, 320), labels = c('0-20', '20-40', '40-80', '80-160', '160-')) %>% droplevels
  # dat$Windc <- dat$Wind %>% optsplit2(split=4)
  # dat$Tempc <- dat$Temp %>% optsplit2(split=4)
  # dat$Solar.Rc <- dat$Solar.R %>% optsplit2(split=4)
  # save(dat,file = "airq.xdr")

  # load(file = "airq.xdr")
  # tab <- with(data=dat, table(Ozonc, Tempc));tab
  # tab_01 <- with(data=dat, table(Ozonc,Tempf));tab_01
  # ftab <- with(data=dat, ftable(Ozone=factor(Ozone>30, exclude=NULL), Tempc=factor(Solar.Rc,exclude=NULL), Month));ftab
  # ftab_01 <- with(data=dat, ftable(Windf=factor(Windf, exclude=NULL), Month,Tempf=factor(Tempf, exclude=NULL)));ftab_01
# ---------End


# ---------Function "setLabs"---------
# Sub   : set labels to table's columns, rows
# Input : table
# Output: table
setLabs_table <- function(tab, xlab, ylab){
  names(dimnames(tab))[1] <- xlab
  names(dimnames(tab))[2] <- ylab
  return(tab)
}
# Sub   : set labels to table's columns, rows
# Input : ftable
# Output: ftable
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
# Main  : set labels to ftable's or table's columns, rows
# Input : table, ftable
# Output: table, ftable
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
# ---------End


# ---------Function "tab2df"---------
# Subsub: convert table to data.frame
# Input : table
# Output: data.frame (with attributes colnm_spread, colnm_spr_len, colnm_spr_pos)
tab2df_table_2d <- function(tab){
  xlab <- names(attributes(tab)[['dimnames']])[1]
  ylab <- names(attributes(tab)[['dimnames']])[2]
  rowv <- attributes(tab)[['dimnames']][[xlab]] 
  colv <- attributes(tab)[['dimnames']][[ylab]]
  rowv_ <- rowv
  newdf <- as.data.frame(tab, stringsAsFactors = F) %>% 
    tidyr::spread_(key=ylab, value='Freq') 
  for(i in 1:length(rowv)){
    if(!is.na(rowv[i])){
      rowv_[i] <- which(rowv[i]==newdf[[xlab]])
    }else{
      rowv_[i] <- which(is.na(newdf[[xlab]]))
    }
  }
  attributes(newdf)$colnm_spread <- ylab
  attributes(newdf)$colnm_spr_len <- ncol(tab)
  attributes(newdf)$colnm_spr_pos <- min(which(names(newdf) %in% colv))
  return(newdf[rowv_, , drop=F])
}
# Sub   : convert ftable to data.frame
# Input : ftable
# Output: data.frame (with attributes colnm_spread, colnm_spr_len, colnm_spr_pos)
tab2df_ftable <- function(ftab){
  xlabs_list  <- attributes(ftab)[['row.vars']]
  ylab_list   <- attributes(ftab)[['col.vars']]
  xlabs <- names(xlabs_list)
  ylab  <- names(ylab_list)
  ylevs <- unlist(ylab_list[1])
  orderdf <- dplyr::arrange_(expand.grid(xlabs_list), xlabs)
  reordermat <- matrix(FALSE, length(unlist(orderdf[1])), length(xlabs))
  order <- rep(0, length(unlist(orderdf[1])))
  colnames(reordermat) <- xlabs
  newdf <- as.data.frame(ftab, stringsAsFactors = F) %>% 
    tidyr::spread_(key=ylab, value="Freq")
  for(i in 1:length(unlist(orderdf[1]))){
    for(xlab in xlabs){
      if(!is.na(orderdf[i, xlab])){
        reordermat[,xlab] <- orderdf[i, xlab]==newdf[[xlab]]
      }else{
        reordermat[,xlab] <- is.na(newdf[[xlab]])
      }
    }
    order[i] <- which(apply(reordermat, FUN = all, MARGIN = 1))
  }
  attributes(newdf)$colnm_spread <- ylab
  attributes(newdf)$colnm_spr_len <- ncol(ftab)
  attributes(newdf)$colnm_spr_pos <- min(which(names(newdf) %in% ylevs))
  return(newdf[order, , drop=F])
}
# Sub   : convert table to data.frame
# Note  : multi dimension(>2) table implicitly transformed to ftable before throughing into main func.
# Input : table
# Output: data.frame (with attributes colnm_spread, colnm_spr_len, colnm_spr_pos)
tab2df_table <- function(tab){
  labs <- names(attributes(tab)[['dimnames']])
  if(length(labs)==2){
    tab2df_table_2d(tab)
  }else if(length(labs)>2){
    message('Note, table implicitly transformed to ftable.')
    tab2df_ftable(ftable(tab))
  }else{
    message('Error, invalid dimension of input object.')
    return(NA)
  }
}
# Main  : convert ftable, table to data.frame
# Input : ftable, table
# Output: data.frame (with attributes colnm_spread, colnm_spr_len, colnm_spr_pos)
tab2df <- function(tab){
  if('ftable' %in% class(tab)){
    tab2df_ftable(tab)
  }else if('table' %in% class(tab)){
    tab2df_table(tab)
  }else{
    message('Error, invalid type of input object.')
    return(NA)
  }
}
# ---------End


# ---------Function "addSpreadLab"---------
# Main  : add prefix in column names if 'colnm_spread' found
# Input : data.frame (with attributes colnm_spread, colnm_spr_len, colnm_spr_pos)
# Output: data.frame (with attributes colnm_spread, colnm_spr_len, colnm_spr_pos)
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
# ---------End


# ---------Function "deco_tabdf"---------
# Sub   : data.frame foramat conversion for output
# Input : vector
# Output: vector
unlabel_rep <- function(vec){
  vec <- as.character(vec)
  vec[which(dplyr::lag(vec)==vec)] <- ''
  return(vec)
}
# Sub   : replace NA in vector for user defined variable
# Input : vector
# Output: vector
replaceNA <- function(vec, repstr='NA'){
  vec <- as.character(vec)
  vec[is.na(vec)] <- repstr
  return(vec)
}
# Sub   : apply functions to spreaded columns
# Input : data.frame (with attributes colnm_spread, colnm_spr_len, colnm_spr_pos), function
# Output: list
lapply_wospread <- function(df, func){
  if(!is.null(attributes(df)$colnm_spr_len)){
    lapply(df[1:(ncol(df)-attributes(df)$colnm_spr_len)], func)
  }
}
# Sub   : data.frame foramat conversion for output
# Input : data.frame (with attributes colnm_spread, colnm_spr_len, colnm_spr_pos), function
# Output: data.frame (with attributes colnm_spread, colnm_spr_len, colnm_spr_pos)
mutate_wospread <- function(df, func){
  if(!is.null(attributes(df)$colnm_spr_len)){
    for(i in 1:(ncol(df)-attributes(df)$colnm_spr_len)){
      df[[i]] <- func(df[[i]])
    }
  }
  return(df)
}
# Main  : data.frame cross table decoration for output
# Input : data.frame (with attributes colnm_spread, colnm_spr_len, colnm_spr_pos)
# Output: data.frame (with attributes colnm_spread, colnm_spr_len, colnm_spr_pos)
deco_tabdf <- function(df){
  if(all(c('colnm_spread', 'colnm_spr_len') %in% names(attributes(df)))){
    c_s <- attributes(df)$colnm_spread
    c_s_l <- attributes(df)$colnm_spr_len
    c_s_p <- attributes(df)$colnm_spr_pos
    tempn <- seq(ncol(df)-attributes(df)$colnm_spr_len+1, ncol(df), 1)
    
    row2 <- names(df)
    row1 <- c(rep('', ncol(df)-length(tempn)), rep(attributes(df)$colnm_spread, length(tempn)))
    bod <- df %>% as.matrix
    
    newdf <- as.data.frame(rbind(row1,row2,bod), stringsAsFactors=F)
    attributes(newdf)$colnm_spread <- c_s
    attributes(newdf)$colnm_spr_len <- c_s_l
    attributes(newdf)$colnm_spr_pos <- c_s_p
    newdf <- mutate_wospread(newdf, replaceNA)
    newdf <- mutate_wospread(newdf, unlabel_rep)
    return(newdf)
  }else{
    message('Error, no spread columns')
    return(NA)
  }
}
# ---------End


# ---------Function "addmargins_ftable"---------
# Sub   : ftable ver of function addmargins
# Input : ftable
# Output: ftable
addmargins_ftable <- function(ftab, ...){
  newtab <- ftable(addmargins(as.table(ftab), ...))
  newtab
}
# ---------End


# ---------Function "addlogit"---------
# Main  : add proportion(positive only) & logit columns
# Input : data.frame (with attributes colnm_spread, colnm_spr_len, colnm_spr_pos)
# Output: data.frame (with attributes colnm_spread, colnm_spr_len, colnm_spr_pos)
addlogit <- function(df, colname1="p", colname2="logit(p)"){
  if(is.null(attributes(df)$colnm_spread)){
    message("Warning, attribute colnm_spread is null, last 2 columns used instead.")
    sprlabs <- names(df)[(length(names(df))-1):length(names(df))]
    sprlen <- 2
  }else{
    sprcol <- attributes(df)$colnm_spread
    sprlen <- attributes(df)$colnm_spr_len
    sprpos <- attributes(df)$colnm_spr_pos
    if(sprlen!=2){
      message("Warning, attribute column_spr_len is not 2, last 2 columns used instead.")
      sprlabs <- names(df)[(length(names(df))-1):length(names(df))]
    }else{
      sprlabs <- names(df)[sprpos:(sprpos+sprlen-1)]
    }
  }
  expt1 <- paste0("~`", sprlabs[2], "`/(`", sprlabs[1], "`+`", sprlabs[2], "`)")
  expt2 <- paste0("~", "log(", colname1, "/(1-", colname1,"))")
  expr1 <- setNames(list(formula(expt1)), nm=colname1)
  expr2 <- setNames(list(formula(expt2)), nm=colname2)
  newdf <- df %>% dplyr::mutate_(.dots=c(expr1, expr2)) 
  attributes(newdf)$colnm_spread <- sprcol
  attributes(newdf)$colnm_spr_len <- sprlen*2
  attributes(newdf)$colnm_spr_pos <- min(which(names(newdf) %in% sprlabs))
  attributes(newdf)
  return(newdf)
}
# ---------End

# ---------Function "tab2df_addlogit"---------
# Sub   : convert table to df, add proportion(positive only) & logit columns
# Input : table
# Output: data.frame (with attributes colnm_spread, colnm_spr_len, colnm_spr_pos)
tab2df_addlogit_table <- function(tab, colname1="p", colname2="logit(p)"){
  ylab <- names(attributes(tab)$dimnames[2])
  ylevs <- sort(attributes(tab)$dimnames[[2]])
  expt1 <- paste0("~`", ylevs[2], "`/(`", ylevs[1], "`+`", ylevs[2], "`)")
  expt2 <- paste0("~", "log(", colname1, "/(1-", colname1,"))")
  expr1 <- setNames(list(formula(expt1)), nm=colname1)
  expr2 <- setNames(list(formula(expt2)), nm=colname2)
  newdf <- tab2df(tab) %>% dplyr::mutate_(.dots=c(expr1, expr2)) 
  attributes(newdf)$colnm_spread <- ylab
  attributes(newdf)$colnm_spr_len <- length(ylevs)*2
  attributes(newdf)$colnm_spr_pos <- min(which(names(newdf) %in% ylevs))
  return(newdf)
}
# Sub   : convert ftable to df, add proportion(positive only) & logit columns
# Input : ftable 
# Output: data.frame (with attributes colnm_spread, colnm_spr_len, colnm_spr_pos)
tab2df_addlogit_ftable <- function(ftab, colname1="p", colname2="logit(p)"){
  xlabs_list <- attributes(ftab)[['row.vars']]
  ylab_list <- attributes(ftab)[['col.vars']]
  xlabs <- names(xlabs_list)
  ylab <- names(ylab_list)
  ylevs <- unlist(ylab_list[1])
  expt1 <- paste0("~`", ylevs[2], "`/(`", ylevs[1], "`+`", ylevs[2], "`)")
  expt2 <- paste0("~", "log(", colname1, "/(1-", colname1,"))")
  expr1 <- setNames(list(formula(expt1)), nm=colname1)
  expr2 <- setNames(list(formula(expt2)), nm=colname2)
  newdf <- tab2df(ftab) %>% dplyr::mutate_(.dots=c(expr1, expr2))
  attributes(newdf)$colnm_spread <- ylab
  attributes(newdf)$colnm_spr_len <- length(ylevs)*2
  attributes(newdf)$colnm_spr_pos <- min(which(names(newdf) %in% ylevs))
  return(newdf)
}
# Main  : convert ftable, table to df, add proportion(positive only) & logit columns
# Input : ftable, table
# Output: data.frame (with attributes colnm_spread, colnm_spr_len, colnm_spr_pos)
tab2df_addlogit <- function(tab){
  if(dim(tab)[2]==2){
    if('ftable' %in% class(tab)){
      tab2df_addlogit_ftable(tab)
    }else if('table' %in% class(tab)){
      tab2df_addlogit_table(tab)
    }else{
      message('Error, invalid type of input object.')
      return(NA)
    }
  }else{
    message('Error, invalid y levels of input tables (must be 2).')
    return(NA)
  }
}
# ---------End


# ---------Function "addprop"---------
# Main  : add proportion columns to df (df msut have attributes 'colnm_spread', 'colnm_spr_len') 
# Input : data.frame (with attributes colnm_spread, colnm_spr_len, colnm_spr_pos) 
# Output: data.frame (with attributes colnm_spread, colnm_spr_len, colnm_spr_pos)
addprop <- function(df, suffix="(prop)"){
  if(is.null(attributes(df)$colnm_spread)){
    message("Error, input table has no attributes 'colnm_spread'")
    return(NA)
  }else{
    sprcol <- attributes(df)$colnm_spread
    sprlen <- attributes(df)$colnm_spr_len
    sprlabs <- names(df)[-1:-(length(names(df))-sprlen)]
    expt <- paste0(paste0("~`",sprlabs,"`"), paste0("/(`", paste(sprlabs, collapse="`+`"), "`)"))
    expt_fm <- lapply(expt, formula)
    expr <- setNames(expt_fm, nm=paste0(sprlabs, suffix))
    newdf <- dplyr::mutate_(df, .dots=expr)
    attributes(newdf)$colnm_spread <- sprcol
    attributes(newdf)$colnm_spr_len <- sprlen*2
    attributes(newdf)$colnm_spr_pos <- min(which(names(newdf) %in% sprlabs))
    return(newdf)
  }
}
# ---------End


# ---------Function "tab2df_addprop"---------
# Sub   : convert table to df, add proportion columns
# Input : table
# Output: data.frame (with attributes colnm_spread, colnm_spr_len, colnm_spr_pos)
tab2df_addprop_table <- function(tab, margin=1, prefix="(prop)"){
  ylab <- names(attributes(tab)$dimnames[2])
  ylevs <- sort(attributes(tab)$dimnames[[2]])
  expt <- paste0(paste0("~`",ylevs,"`"), paste0("/(`", paste(ylevs, collapse="`+`"), "`)"))
  expt_fm <- lapply(expt, formula)
  expr <- setNames(expt_fm, nm=paste0(ylevs, prefix))
  newdf <- tab2df(tab) %>% 
    dplyr::mutate_(.dots=expr)
  attributes(newdf)$colnm_spread <- ylab
  attributes(newdf)$colnm_spr_len <- length(ylevs)*2
  attributes(newdf)$colnm_spr_pos <- min(which(names(newdf) %in% ylevs))
  return(newdf)
}
# Sub   : convert table to df, add proportion columns
# Input : ftable
# Output: data.frame (with attributes colnm_spread, colnm_spr_len, colnm_spr_pos)
tab2df_addprop_ftable <- function(ftab, margin=1, prefix="(prop)"){
  xlab_list <- attributes(ftab)[['row.vars']]
  ylab_list <- attributes(ftab)[['col.vars']]
  xlabs <- names(xlab_list)
  ylab <- names(ylab_list)
  ylevs <- unlist(ylab_list[1])
  df <- tab2df(ftab)
  df_prop <- tab2df(prop.table(ftab, margin=1))
  names(df_prop)[names(df_prop) %in% unlist(ylab_list)] <- paste0(unlist(ylab_list), prefix)
  newdf <- dplyr::inner_join(df, df_prop, by=c(xlabs))
  attributes(newdf)$colnm_spread <- ylab
  attributes(newdf)$colnm_spr_len <- length(unlist(ylab_list))*2
  attributes(newdf)$colnm_spr_pos <- min(which(names(newdf) %in% ylevs))
  return(newdf)
}
# Main  : convert table, ftable to df, add proportion columns
# Input : ftable, table
# Output: data.frame (with attributes colnm_spread, colnm_spr_len, colnm_spr_pos)
tab2df_addprop <- function(tab){
  if('ftable' %in% class(tab)){
    tab2df_addprop_ftable(tab)
  }else if('table' %in% class(tab)){
    tab2df_addprop_table(tab)
  }else{
    message('Error, invalid type of input object.')
    return(NA)
  }
}
# ---------End


# ---------Function "change_sum_NA"---------
# Main  : replace last & 2nd last rows of df
# Input : data.frame
# Output: data.frame
change_sum_NA <- function(dat){
  dat[c(setdiff(1:nrow(dat), nrow(dat)-1), nrow(dat)-1),]
}
# ---------End


# ---------Function "coeffs_all"---------
# Main  : make multiple cross tables & coefficients(phi, chi square, cramer's V, contingency)
# Input : data.frame, names of explanatory variable columns, name of target variable column
# Output: list of [list of coefficient] x 4, [list of tables]
coeffs_all <- function(dat, vars, vlen, tg, tlen, x_categorize=T, y_categorize=F, useNA=T, include.lowest_v=T, include.lowest_t=T, right_v=F, right_t=F){
  tablelist <- list()
  cramlist <- data.frame(matrix(NA, length(vars), 2), stringsAsFactors=F) %>%
    setNames(nm=c("variable", "Cramer's V"))
  contlist <- data.frame(matrix(NA, length(vars), 2), stringsAsFactors=F) %>%
    setNames(nm=c("variable", "Contingency"))
  philist <- data.frame(matrix(NA, length(vars), 2), stringsAsFactors=F) %>%
    setNames(nm=c("variable", "Phi"))
  chilist <- data.frame(matrix(NA, length(vars), 2), stringsAsFactors=F) %>%
    setNames(nm=c("variable", "Chi Square"))
  
  n_tlev <- nlevels(as.factor(dat[[tg]]))
  # tlen_ <- ifelse(n_tlev <= tlen, n_tlev, tlen)
  if((y_categorize==T)&(n_tlev>tlen)){
    tg_c <- if(is.numeric(dat[[tg]])){optsplit2(x = dat[[tg]], split = tlen, include.lowest = include.lowest_t, right = right_t)}else{dat[[tg]]}
  }else{
    tg_c <- dat[[tg]]
  }
  j <- 1
  for(var in vars){
    n_vlev <- nlevels(as.factor(dat[[var]]))
    # vlen_ <- ifelse(n_vlev <= vlen, n_vlev, vlen)
    if((x_categorize==T)&(n_vlev>vlen)){
      var_c <- if(is.numeric(dat[[var]])){optsplit2(x = dat[[var]], split = vlen, include.lowest = include.lowest_v, right = right_v)}else{dat[[var]]}
    }else{
      var_c <- dat[[var]]
    }
    if(useNA==T){
      tx <- paste0("table(", var, "=factor(var_c, exclude=NULL), ", tg, "=factor(tg_c)) %>% assocstats")
    }else{
      tx <- paste0("table(", var, "=factor(var_c), ", tg, "=factor(tg_c)) %>% assocstats")
    }
    assoc <- eval(parse(text=tx))
    philist[j,1] <- var
    philist[j,2] <- as.numeric(assoc[[3]])
    contlist[j,1] <- var
    contlist[j,2] <- as.numeric(assoc[[4]])
    cramlist[j,1] <- var
    cramlist[j,2] <- as.numeric(assoc[[5]])
    chilist[j,1] <- var
    chilist[j,2] <- as.numeric(assoc[[2]][2])
    tablelist[[j]] <- assoc[[1]]
    j <- j+1
  }
  tablelist <- setNames(tablelist, nm=vars)
  philist <- dplyr::arrange(philist, desc(Phi))
  contlist <- dplyr::arrange(contlist, desc(Contingency))
  cramlist <- dplyr::arrange(cramlist, desc(`Cramer's V`))
  chilist <- dplyr::arrange(chilist, desc(`Chi Square`))
  list("phi"=philist, "contingency"=contlist, "cramersV"=cramlist, "chisq"=chilist, "tables"=tablelist)
}
# ---------End

# ---------Function "tables_toExcel"---------
# Sub   : write decorated data.frame (cross table) into workbook, styles for body, head can be applied.
#         nedd data.frame with attributes colnm_spread, colnm_spr_len, colnm_spr_pos
# Input : workbook object, data.frame, sheetname, start colum number, start row number, styles, borders
# Output: null (operation only)
table_toExcel_deco <- function(wb, x, sheet, startCol=2, startRow=2, headStyle=NULL, bodyStyle=NULL, borders="surrounding"){
  sprlen <- attributes(x)$colnm_spr_len
  x_head <- x[1:2, -1, drop=F]
  x_rown <- x[-1, 1:(ncol(x)-sprlen), drop=F]
  x_body <- x[-1:-2 , -1:-(ncol(x)-sprlen), drop=F] %>% apply(.,MARGIN=2, FUN=as.numeric)
  openxlsx::writeData(wb = wb, sheet = sheet, x = x_head, 
                      startCol=startCol+ncol(x_rown), startRow = startRow, 
                      borders=borders, colNames = F, rowNames = F, headerStyle =  )
  openxlsx::writeData(wb = wb, sheet = sheet, x = x_rown, 
                      startCol=startCol, startRow = startRow+1, 
                      borders=borders, colNames = F, rowNames = F)
  openxlsx::writeData(wb = wb, sheet = sheet, x= x_body, 
                      startCol=startCol+ncol(x_rown), startRow=startRow+2, 
                      borders=borders, colNames = F, rowNames = F)
  if(!is.null(headStyle)){
    openxlsx::addStyle(wb, sheet, style=headStyle, 
                       cols=(startCol+ncol(x_rown)):(startCol+ncol(x)-1),
                       rows=(startRow:(startRow+1)),
                       gridExpand=T, stack=F)
    openxlsx::addStyle(wb, sheet, style=headStyle, 
                       cols=startCol:(startCol+ncol(x_rown)-1),
                       rows=(startRow+1):(startRow+nrow(x_rown)),
                       gridExpand=T, stack=F)    
  }
  if(!is.null(bodyStyle)){
    openxlsx::addStyle(wb, sheet, style=bodyStyle, 
                       cols=(startCol+ncol(x_rown)):(startCol+ncol(x)-1),
                       rows=(startRow+2):(startRow+nrow(x)-1),
                       gridExpand=T, stack=F)
  }
}
# Main  : write multiple data.frames (cross tables) into workbook, if decorated==T, styles for body, head can be applied.
# Input : workbook object, data.frame, sheetname, start colum number, start row number, others(styles)
# Output: null (operation only)
tables_toExcel <- function(wb, tablelist, sheet="sheet1", decorated=T, borders="surrounding", startRow=2, startCol=2, ...){
  tablenames <- names(tablelist)
  startR <- startRow
  startC <- startCol
  if(decorated==T){
    for(nm in tablenames){
      daf <- tablelist[[nm]]
      table_toExcel_deco(wb=wb, x=daf, sheet=sheet, startCol=startC, startRow =startR, ...)
      startR <- startR+dim(daf)[1]+1
      print(nm)
    }
  }else{
    for(nm in tablenames){
      table_body <- tablelist[[nm]]
      table_rown <- dimnames(tablelist[[nm]]) %>% names %>% `[`(1)
      table_coln <- dimnames(tablelist[[nm]]) %>% names %>% `[`(2)
      openxlsx::writeData(wb = wb, sheet = sheet, x = table_rown, startCol=startC, startRow = startR+2)
      openxlsx::writeData(wb = wb, sheet = sheet, x = table_coln, startCol=startC+2, startRow = startR)
      startR <- startR+1
      openxlsx::writeData(wb = wb, sheet = sheet, x = table_body, startCol = startC+1, startRow = startR, borders = borders, colNames = T, rowNames = T)
      startR <- startR+dim(table_body)[1]+1
      print(nm)
    }
  }
}

# ---------End



# ---------Example---------
# example
# tab %>% tab2df 
# tab %>% tab2df %>% addprop
# tab %>% addmargins(margin=1) %>% tab2df 
# tab %>% addmargins(margin=1) %>% tab2df %>% addprop
# tab_01 %>% tab2df %>% addlogit(colname1 = "a", colname2 = "logit")
# tab_01 %>% addmargins(margin=1) %>% tab2df_addlogit
# 
# ftab %>% tab2df %>% addprop
# ftab %>% addmargins_ftable(margin=2) %>% tab2df
# ftab %>% addmargins_ftable(margin=2) %>% tab2df %>% addprop(suffix = "_p")
# ftab_01 %>% tab2df %>% addlogit
# ftab_01 %>% addmargins_ftable(margin=2) %>% tab2df_addlogit %>% decol_tabdf %>% 
#   write.table(sep='\t', col.names=F, row.names=F, file="clipboard")