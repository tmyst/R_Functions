# ------------function---------------------------------------
require(lazyeval)
require(plyr)
require(tidyr)
require(dplyr)

# ---------Function "invlogit"---------
invlogit = function(x){exp(x)/(1+exp(x))}

# ---------Function "addOrderByKey"---------
# Main  : Order and group data.frame by value of key, add new column that stands for group number
# Input :
# Output:
addOrderByKey <- function(x, key, split=10){
  mutate_call <- lazyeval::interp(~ cume_dist(desc(a)), a = as.name(key))
  x <- x %>% arrange_(lazyeval::interp(~desc(var), var = as.name(key))) %>% 
    dplyr::mutate_(.dots = setNames(list(mutate_call), "cume_dist")) %>% 
    dplyr::mutate(groupid = split+1)
  for(i in 1:split){
    frac <- (1/split)*i
    cmd <- x$cume_dist
    trow <- max(which(abs(cmd-frac)==min(abs(cmd-frac)))) 
    x[seq(1, trow, by=1),]$groupid <- x[seq(1:trow),]$groupid - 1
  }
  return(x)
}

# Deprecated Func
splitdf <- function(x, key, split=10){
  message("Deprecated, use addOrderByKey.\nReturn NULL.")
}

# ---------Function "setTrain"---------
setTrain <- function(x, train=7, test=3){
  numb <- round(nrow(x)*train/(train+test))
  idx <- sample(1:nrow(x), numb, replace=F)
  is.train <- seq(1:nrow(x)) %in% idx
  train <- x[is.train,]
  test <- x[!is.train,]
  return(list(train, test))
}
setTrain_rough <- function(x, split=3){
  idx <- sample(1:split, nrow(x), replace = TRUE)
  is.train <- idx!=1
  train <- x[is.train,]
  test <- x[!is.train,]
  return(list(train, test))
}

# Deprecated Func
settrain <- function(x, train=7, test=3){
  message("Deprecated, use setTrain.\nReturn NULL.")
}
settrain2 <- function(x, train=7, test=3){
  message("Deprecated, use setTrain.\nReturn NULL.")
}
settrain3 <- function(x, train=7, test=3){
  message("Deprecated, use setTrain.\nReturn NULL.")
}

# ---------Function "countInf"---------
countInf <- function(a){
  lapply(a, function(x) sum(is.infinite(x)))
}

# ---------Function "countNA"---------
countNA <- function(a){
  lapply(a, function(x) sum(is.na(x)))
}

# ---------Function "countNan"---------
countNan <- function(a){
  lapply(a, function(x) sum(is.nan(x)))
}

# ---------Function "chiall"---------
# Main  : chi square test for explanetory variables & target variable
# Input : data.frame, variable colums, target column
# Output: list
chiall <- function(dat, vars, tg){
  results <- cbind(expand.grid(vars), matrix(NA, length(vars),4))
  ctlist <- c("chisq", "df", "pval", "method")
  colnames(results) <- c("x", ctlist)
  results <- results %>% dplyr::arrange(x)
  iter <- 1
  for(var in vars){
    ct <- chisq.test(x=dat[[var]], y=dat[[tg]])
    results[iter,2:5] <- ct[1:4]
    iter <- iter+1
  }
  return(results %>% dplyr::arrange(desc(chisq)))
}

# Deprecated Func
chiall2 <- function(dat, tgt, exclude){
  message("Deprecated, use chiall.\nReturn NULL.")
}

# ---------Function "bd2age"---------
# Main  : Convert birthday to age
# Input : vector (date as integer), vector (birthday as integer)
# Output: vector (age as numeric)
bd2age <- function(day, birthday){
  floor((day - birthday)/10000)
}
# Deprecated Func
toage <- function(day, birthday){
  message("Deprecated, use bd2age.\nReturn NULL.")
}

# ---------Function "n2nClass"---------
# Main  : Categorize age with interval 10
# Input : vector (numeric)
# Output: vector (factor)
age2ageClass_10 <- function(age, drop=F){
  age_cut <- cut(age, breaks=c(0, 10, 20, 30, 40, 50, 60, 100), 
                 labels=c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-"), 
                 include.lowest = T, right = F)
  if(drop==T){droplevels(age_cut)}else{age_cut}
}
# Main  : Categorize age vector with user defined interval
# Input : vector (numeric) , interval, threshold (high side only)
# Output: vector (factor)
n2nClass <- function(n, range, thres, drop=F, include.lowest=T, right=F){
  n_cl <- thres %/% range
  r_chr <- as.numeric(range)
  upperV <- c(as.character(seq(range-1, thres-1, range)), "")
  lowerV <- seq(0, thres, range) %>% as.character
  lab <- paste0(lowerV, "-", upperV)
  if(drop==F){
    cut(n, breaks=c(seq(0, thres, range), Inf), labels=lab, 
        include.lowest=include.lowest, right=right)
  }else{
    droplevels(cut(n, breaks=c(seq(0, thres, range), Inf), labels=lab, 
                   include.lowest=include.lowest, right=right))
  }
}

# Deprecated Func
toageclass_10 <- function(age){
  message("Deprecated, use age2ageClass_10.\nReturn NULL.")
}
toageclass_2 <- function(age, range, thres){
  message("Deprecated, use n2nClass.\nReturn NULL.")
}

# ---------Function "n2nClass_div"---------
# zan classification
n2nClass_div <- function(n, div=60, range=20, thres=80, include.lowest=T, right=F){
  n_h <- n/div
  n2nClass(n_h, range=range, thres=thres, include.lowest=include.lowest, right=right)
}

# Deprecated Func
tozanclass <- function(zan){
  message("Deprecated, use n2nClass_div.\n2nClass_div called instead.")
  n2nClass_div(zan)
}

# ---------Function "temp_Class"---------
# Deprecated
# point classification
temp_Class <- function(p){
  cut(p, breaks=c(0, 2000, 5000, 10000, 20000, 50000, 100000, Inf), 
      labels=c("-1999", "2000-4999", 
               "5000-9999","10000-19999", 
               "20000-49999", "50000-99999", 
               "100000-"), 
      include.lowest = T, right=F
  )
}

# Deprecated Func
tohokenpclass <- function(p){
  message("Deprecated, use temp_Class.\nReturn NULL.")
}

# ---------Function "replcol"---------
# replace columns in data.frame
# Input : data.frame, target column name, leader column name
# Output: data.frame
replcol <- function(x, targetcol, leadercol=""){
  if(leadercol==""){
    return(x[c(targetcol, setdiff(colnames(x), targetcol))])
  }else{
    leadercol_pos <- which(colnames(x)==leadercol)
    headcols <- colnames(x)[1:leadercol_pos]
    tailcols <- setdiff(colnames(x), c(targetcol, headcols))
    return(x[c(headcols, targetcol, tailcols)])
  }
}

# ---------Function "aggrByKey"---------
# Main  :Make table like data.frame from data.frame, key, aggregation target
# Input :data.frame, key column, target column, aggregation function(sum, mean, median, ...)
# Output:list of data.frame
aggrBykey <- function(dat, keys, vars, funs){
  iskey <- sapply(keys, function(x)which(colnames(dat)==x))   
  dat <- dat[c(iskey, setdiff(1:ncol(dat), iskey))]
  if(funs=="count"){
    txt <- "dat %>% dplyr::group_by_(.dots=dots) %>% dplyr::summarize(count=n())"
  }else{
    txt <- paste0("dat %>% dplyr::group_by_(.dots=dots) %>% dplyr::summarize_each_(funs(", funs, "), vars)")
  }
  y <- list()
  for(i in 1:length(keys)){             # no. of key in keys
    n <- dim(combn(keys, i))[2]         # no. of key combinations when choosing i keys
    for(j in 1:n){
      grp_cols <- combn(keys,i)[,j]     # j of all key combinations
      dots <- lapply(grp_cols, as.symbol)
      y <- append(y, list(eval(parse(text=txt))))
    }
  }
  return(y)
}

# Exapmle
# dfsum(dat, keys=c("sex", "age"), vars="flag", funs = "sum(.)+9")

# Deprecated Name
dfsum <- function(dat, keys, vars, funs){
  message("Deprecated, use aggrByKey.\nReturn NULL.")
}

# ---------Function "crossdfList"---------
# Main  : 
# Input : use listwise=T if na.omit
# Output:
crossdfList <- function(dat, vars, tg, useNA="both", fill=0, margin=1, nameforNA="NA", sep="", suffix="(%)", option=NULL){
  y <- list()
  if(useNA %in% c("target", "both")){
    ncoltg <- levels(factor(dat[[tg]], exclude=NULL))
    ncoltg[is.na(ncoltg)] <- nameforNA
  }else{
    ncoltg <- levels(factor(dat[[tg]]))
  }
  for(i in 1:length(vars)){         # no. of var in vars
    n <- dim(combn(vars, i))[2]     # no. of var combinations when choosing i vars
    for(j in 1:n){
      vcomb <- combn(vars,i)[,j]    # j of all var combinations
      dat_ <- dat[c(vcomb, tg)]
      grp_cols <- c(vcomb, tg)
      dots <- lapply(grp_cols, as.symbol)
      newdf <- dat_ %>% 
        dplyr::group_by_(.dots=dots) %>% 
        dplyr::count_(dots) %>% 
        ungroup()
      if(useNA=="both"){
        for(v in vcomb){
          newdf[[v]][is.na(newdf[[v]])] <- nameforNA
        }
        newdf[[tg]][is.na(newdf[[tg]])] <- nameforNA
        newdf <- na.omit(newdf) %>% tidyr::spread_(tg, "n", fill=fill)
      }else if(useNA=="var"){
        for(v in vcomb){
          newdf[[v]][is.na(newdf[[v]])] <- nameforNA
        }
        newdf <- na.omit(newdf) %>% tidyr::spread_(tg, "n", fill=fill)
      }else if(useNA=="target"){
        newdf[[tg]][is.na(newdf[[tg]])] <- nameforNA
        newdf <- na.omit(newdf) %>% tidyr::spread_(tg, "n", fill=fill)
      }else{
        newdf <- na.omit(newdf) %>% tidyr::spread_(tg, "n", fill=fill)
      }
      if(margin==1){
        Sum <- apply(newdf[,ncoltg], 1, sum)
        newdf <- cbind(newdf[, vcomb], Sum, newdf[, ncoltg])
      }else if(margin==2){
        Sum_body <- apply(newdf[,ncoltg], 2, sum)
        Sum_row <- as.data.frame(lapply(vcomb, function(x)"Sum")) %>% setNames(nm=vcomb)
        newdf <- cbind(rbind(newdf[, vcomb], Sum_row), rbind(newdf[, ncoltg], Sum_body))
      }
      attributes(newdf)$colnm_spread <- tg
      attributes(newdf)$colnm_spr_len <- length(ncoltg)
      attributes(newdf)$colnm_spr_pos <- min(which(names(newdf) %in% ncoltg))
      if(is.null(option)){
        
      }else if(option=="prop"){
        newdf <- addProp(newdf)
      }else if(option=="logit"){
        newdf <- addLogit(newdf) 
      }
      y[stringr::str_c(vcomb, collapse="_")] <- list(newdf)
    }
  }
  return(y)
}

# Deprecated Func
crossdf <- function(){
  message("Deprecated, use crossdfList.\nReturn NULL.")
}

# ---------Function "crossdf_1"---------
# !!Bug!! tidyr::spread fails if NA in key column
# Input : data.frame, target column, variable columns
# Output: list of data.frames
crossdf_1 <- function(dat, vars, tg){
  message("Deprecated, use crossdfList.\nReturn NULL.")
}

# ---------Function "crossdf_01_p"---------
# Main  : 
# Input : 
# Output:
crossdf_01_p <- function(dat, keys, tg){
  message("Deprecated, use crossdfList.\nReturn NULL.")
}

# ---------Function "crossdf_01"---------
crossdf_01 <- function(dat, keys, tg){
  message("Deprecated, use crossdfList.\nReturn NULL.")
}
# crossdf_01 <- function(dat, keys, tg){
#   y <- list()
#   if(sum(!is.element(as.character(dat[[tg]]), c("0","1")))==0){
#     iskey <- sapply(keys, function(x)which(colnames(dat)==x))   
#     dat <- dat[c(iskey, setdiff(1:ncol(dat), iskey))]
#     ncoltg <- levels(as.factor(dat[[tg]]))
#     for(i in 1:length(keys)){     
#       n <- dim(combn(keys, i))[2] 
#       for(j in 1:n){
#         k <- combn(keys,i)[,j]
#         grp_cols <- c(k, tg) 
#         dots <- lapply(grp_cols, as.symbol)
#         a <- dat %>% 
#           dplyr::group_by_(.dots=dots) %>% 
#           count_(dots) %>% 
#           spread_(tg, "n", fill=0)
#         aa <- bind_rows(a, lapply(a, sum))
#         d <- aa %>% 
#           transmute(p=`1`/(`0`+`1`), `p(%)`= p*100, `logit(p)`=log(p/(1-p))) %>% 
#           dplyr::select(-p)
#         Sum <- data.frame(sum=apply(aa[,ncoltg],1,sum))
#         c <- bind_cols(aa[,k], Sum, aa[,names(aa)!=k], d)
#         y <- append(y, list(c))
#       }
#     }
#   }
#   return(y)
# }

# ---------Function "optsplit"---------
# Main  : Categorize numeric vector towards equal frequency categorization
# Input : numeric, number of category, etc
# Output: factor
optsplit <- function(x, split, include.lowest=TRUE, right=FALSE){
  k <- x %>% data.frame %>% names
  mutate_call <- lazyeval::interp(~ cume_dist(var), var = as.name(k))
  mutate_call2 <- lazyeval::interp(~a+1, a=split)
  temp <- x %>% 
    data.frame %>% 
    dplyr::arrange_(lazyeval::interp(~var, var = as.name(k))) %>% 
    dplyr::mutate_(.dots = setNames(list(mutate_call), "cume_dist")) %>% 
    dplyr::mutate_(.dots= setNames(list(mutate_call2), "groupid"))
  for(i in 1:split){
    frac <- (1/split)*i
    cmd <- temp$cume_dist
    trow <- max(which(abs(cmd-frac)==min(abs(cmd-frac), na.rm=TRUE)), na.rm=TRUE) 
    temp[seq(1, trow, by=1),]$groupid <- temp[seq(1, trow, by=1),]$groupid - 1
  }
  cuts <- temp %>%
    dplyr::group_by(groupid) %>% 
    dplyr::filter(row_number()==ifelse(right==TRUE, n(), 1)) %>% 
    dplyr::select_(k) %>% `[[`(2) %>% as.vector
  if(right==TRUE){
    if(min(cuts, na.rm=TRUE)!=min(x, na.rm=TRUE)){
      xcut <- cut(x, breaks = c(min(x, na.rm=TRUE), cuts), include.lowest = include.lowest, right=right)
    }else{
      xcut <- cut(x, breaks = c(-Inf, cuts), include.lowest = include.lowest, right=right)
    }
  }else{
    if(max(cuts, na.rm=TRUE)!=max(x, na.rm=TRUE)){
      xcut <- cut(x, breaks = c(cuts, max(x, na.rm=TRUE)), include.lowest = include.lowest, right=right)
    }else{
      xcut <- cut(x, breaks = c(cuts, Inf), include.lowest = include.lowest, right=right)
    }
  }
  return(xcut)
}
# Deprecated Func
optsplit2 <- function(x, split=10, include.lowest=TRUE, right=FALSE){
  message("'optsplit2' is deprecated, use optsplit.\noptsplit called instead.")
  optsplit(x=x, split=split, include.lowest=include.lowest, right=right)
}

# ---------Function "opcross11"---------
# Main  : Make crosstables for 1 variable & multiple target
# Input : dataframe or tbl_df
# Output: 
opcross11 <- function(dat, xsplit, ysplit, vars, tg){
  message("'opcross11' is deprecated, use 'coeffs_all' next time.")  
  y <- list()
  if(ysplit > 0){dat[tg] <- lapply(dat[[tg]], optsplit, split=ysplit)}
  if(xsplit > 0){dat[vars] <- lapply(dat[vars], optsplit, split=xsplit)}
  for(var in vars){
    if(nlevels(factor(as.numeric(dat[[tg]]))) > 5){
      warning("Width of the cross table may be too wide.")
    }
    y[var] <- crossdfList(dat, var, tg)
  }
  y
}

# ---------Function "opcrossAll"---------
# Main  : Make crosstable list
# Input : dataframe or tbl_df, no. of var&target categories, variable names, target name
# Output: 
crossdfList_categorize <- function(dat, vars, tg, vlen, tlen, include.lowest_t=T, include.lowest_v=T, right_t=F, right_v=F, ...){
  y <- list()
  n_tlev <- nlevels(as.factor(dat[[tg]]))
  if(n_tlev>tlen){
    dat[[tg]] <- if(is.numeric(dat[[tg]])){optsplit(x = dat[[tg]], split = tlen, include.lowest = include.lowest_t, right = right_t)}else{dat[[tg]]}
  }
  for(var in vars){
    n_vlev <- nlevels(as.factor(dat[[var]]))
    if(n_vlev>vlen){
      dat[[var]] <- if(is.numeric(dat[[var]])){optsplit(x = dat[[var]], split = vlen, include.lowest = include.lowest_v, right = right_v)}else{dat[[var]]}
    }
  }
  # if(xsplit > 0){dat[vars] <- lapply(dat[vars], optsplit, split=xsplit)}
  crossdfList(dat, vars = vars, tg=tg, ...)
}

# ---------Function "crossoutput"---------
# Note  : !!Deprecated
# Main  : crosstables to Excel
# Input :
# Output:
# Note  :
#   need
#     NewWb <- createWorkbook(creator = "Autor")
#   before
#   neeed
#     saveWorkbook(wb = NewWb, file=filename, overwrite = TRUE)
#   after
crossoutput <- function(tablelist, sheetname="crosstables"){
  message("Deprecated, use toExcel_dfs.\nReturn NULL.")
}

# ---------Function "tcross"---------
# Note  : !!Deprecated
# Main  : Make symble cross table
# Input : data.frame
# Output: table
tcross <- function(x, key, tg){
  tab <- xtabs(data=x, formula = paste0("~", key, "+", tg)) %>% addmargins(1)
  factornames <- names(dimnames(a))
  newtab <- cbind(addmargins(a, 2), prop.table(a, 1) ) 
  names(dimnames(newtab)) <- factornames
  return(newtab)
}

# ---------Function "round_5up"---------
# Main  : Round values always away from 0
#         normal R's "round" round values towards&away from zero statistically equally
# Input : numeric
# Output: numeric
round_5up <- function(x, digits=0){
  if(floor(x*10^(digits+1)) %% 10 == 5){
    round(x+sign(x)*1*10^(-1*(digits+1)), digits)
  }else{
    round(x, digits)
  }
}

# ---------Function "readData"---------
# Sub   : xlsx file Reader
# Input : file name, folder name, encoding
# Output: 
readxlsheets <- function(filename, foldername){
  message("Deprecated, use readXLSheets next time.\nreadXLSheets called instead.")
  readXLSheets(filename, foldername)
}
readXLSheets <- function(filename, foldername, startSheet=2){
  i <- startSheet
  y <- list()
  while(1){
    datasheet <- tryCatch(read_excel(paste0(foldername, "/", filename), sheet = i),
                          error=function(e)return("e"), silent=TRUE)
    if(datasheet=="e"){break}
    y[[i]] <- datasheet
    i <- i+1
  }
  do.call(bind_rows, y)
}
# Main  : File Reader.
# Input : file name, folder name, encoding
# Output: data.frame
readData <- function(filename, foldername, encode="CP932"){
  fileext <- rev(unlist(str_split(string=filename, "\\.")))[1]
  if(!fileext %in% c("csv", "txt", "xlsx")){
    stop(paste0("Invalid extention - ", "filename"))
  }else if(fileext=="xlsx"){
    out <- readXLSheets(filename=filename, foldername=foldername)
  }else if(fileext=="csv"){
    out <- read_csv(file=paste0(foldername, "/", filename), locale=locale(encoding=encode))
  }else if(fileext=="txt"){
    out <- read_tsv(file=paste0(foldername, "/", filename), locale=locale(encoding=encode))
  }
  out
}
readdata <- function(filename, foldername, encode="CP932"){
  message("Deprecated, use readData next time.\nreadData called instead.")
  readData(filename, foldername, encode)
}
