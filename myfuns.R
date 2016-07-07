# ------------function---------------------------------------
require(lazyeval)
require(plyr)
require(tidyr)
require(dplyr)

# ---------Function "invlogit"---------
invlogit = function(x){exp(x)/(1+exp(x))}

# ---------Function "addGroupLab"---------
splitdf <- function(x, key, split=10){
  message("Deprecated, use addGroupLab")
}
# keyの値の大きい順にgroupidを振る
# splitが分割数だが、順位タイのものは
# splitの実現にできるだけ近づけるようにどちらかのグループに寄せる
addGroupLab <- function(x, key, split=10){
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

# ---------Function "settrain"---------
# deprecated,
settrain <- function(x, train=7, test=3){
  message("Deprecated, use setTrain")
}
settrain2 <- function(x, train=7, test=3){
  message("Deprecated, use setTrain")
}
settrain3 <- function(x, train=7, test=3){
  message("Deprecated, use setTrain")
}
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

# ---------Function "suminf"---------
# データ中の無限大(inf)の個数を数える
# naはsum(is.na(data))で大丈夫。
suminf <- function(a){
  l <- 0
  for(b in names(a)){
    l <- l+sum(is.infinite(a[,b]))
  }
  return(l)
}

# ---------Function "sumnan"---------
# データ中のnanの個数を数える
sumnan <- function(a){
  l <- 0
  for(b in names(a)){
    l <- l+sum(is.na(a[,b]))
  }
  return(l)
}

# ---------Function "chiall"---------
chiall2 <- function(dat, tgt, exclude){
  message("Deprecated, use chiall")
}
# Main  : chi square test for explanetory variables & target variable
# Input : 
# Output: 
chiall <- function(dat, vars, tgt){
  results <- cbind(expand.grid(vars), matrix(NA, length(vars),4))
  ctlist <- c("chisq", "df", "pval", "method")
  colnames(results) <- c("x", ctlist)
  results <- results %>% dplyr::arrange(x)
  iter <- 1
  for(v in vars){
    ct <- chisq.test(x=dat[[v]], y=dat[[tgt]])
    results[iter,2:5] <- ct[1:4]
    iter <- iter+1
  }
  return(results %>% dplyr::arrange(desc(chisq)))
}

# ---------Function "toage"---------
toage <- function(day, birthday){
  message("Deprecated, use bd2age")
}
# Main  : Convert birthday to age
# Input : vector (date as integer), vector (birthday as integer)
# Output: vector (age as numeric)
bd2age <- function(day, birthday){
  floor((day - birthday)/10000)
}

# ---------Function "toageclass"---------
# 10歳区切り
toageclass_10 <- function(age){
  message("Deprecated, use age2ageClass_10")
}
toageclass_2 <- function(age, range, thres){
  message("Deprecated, use age2ageClass")
}
# Main  : Categorize age with interval 10
# Input : vector (numeric)
# Output: vector (factor)
age2ageClass_10 <- function(age){
  cut(age, breaks=c(0, 10, 20, 30, 40, 50, 60, 100), 
      labels=c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-"), 
      include.lowest = T, right = F)
}
# Main  : Categorize age vector with user defined interval
# Input : vector (numeric) , interval, threshold (high side only)
# Output: vector (factor)
n2nClass <- function(n, range, thres, include.lowest=T, right=F){
  n_cl <- thres %/% range
  r_chr <- as.numeric(range)
  upperV <- c(as.character(seq(range-1, thres-1, range)), "")
  lowerV <- seq(0, thres, range) %>% as.character
  lab <- paste0(lowerV, "-", upperV)
  cut(n, breaks=c(seq(0, thres, range), Inf), labels=lab, include.lowest=include.lowest, right=right)
}

# ---------Function "settrain"---------
# zan classification
tozanclass <- function(zan){
  message("Deprecated, use n2nClass_div")
}
n2nClass_div <- function(n, div=60, range=20, thres=80, include.lowest=T, right=F){
  n_h <- n/div
  n2nClass(n_h, range=range, thres=thres, include.lowest=include.lowest, right=right)
}

# ---------Function "settrain"---------
# point classification
tohokenpclass <- function(p){
  message("Deprecated, use ")
}
temp_Class <- function(p){
  cut(p, breaks=c(0, 2000, 5000, 10000, 20000, 50000, 100000, Inf), 
      labels=c("-1999", "2000-4999", "5000-9999","10000-19999", "20000-49999", "50000-99999", "100000-"), 
      include.lowest = T, right=F
  )
}

# ---------Function "replcol"---------
# replace columns in data.frame
# 
# 
replcol <- function(x, targetcol, formercol=""){
  if(formercol==""){
    return(x[c(targetcol, setdiff(colnames(x), targetcol))])
  }else{
    formercol_pos <- which(colnames(x)==formercol)
    headcols <- colnames(x)[1:formercol_pos]
    tailcols <- setdiff(colnames(x), c(targetcol, headcols))
    return(x[c(headcols, targetcol, tailcols)])
  }
}

# ---------Function "dfsum"---------
# データフレームdatとkeysに関して、keys中のすべての組み合わせをでgoup_by→集約。
# 結果はデータフレームのリスト。
# datにデータ、varsには集計対称の変数、funsはcount, sum, mean, sdなど。
dfsum <- function(dat, keys, vars, funs){
  iskey <- sapply(keys, function(x)which(colnames(dat)==x))   
  dat <- dat[c(iskey, setdiff(1:ncol(dat), iskey))]   # 集計後の列の順番を整理する。
  if(funs=="count"){
    txt <- "dat %>% dplyr::group_by_(.dots=dots) %>% dplyr::summarize(count=n())"
  }else{
    txt <- paste0("dat %>% dplyr::group_by_(.dots=dots) %>% dplyr::summarize_each_(funs(", funs, "), vars)")
  }
  y <- list()
  for(i in 1:length(keys)){     # keys中のkeyの個数
    n <- dim(combn(keys, i))[2] # keys中からi個のkeyを選ぶときのcombinationの個数
    for(j in 1:n){
      grp_cols <- combn(keys,i)[,j] # keys中からi個のkeyを選ぶときのcombinationのj番目
      dots <- lapply(grp_cols, as.symbol)
      y <- append(y, list(eval(parse(text=txt))))
    }
  }
  return(y)
}

# ---------Function "crossdf"---------
# !!Bug!! Spread fails if NA in key column
# Input : data.frame, target column, variable columns
# Output: list of data.frames
crossdf <- function(dat, keys, tg){
  message("deprecated, use other function")
}

# ---------Function "crossdf_1"---------
# crossdf with dropping rows including NA value (listwise)
crossdf_1 <- function(dat, keys, tg){
  y <- list()
  iskey <- sapply(keys, function(x)which(colnames(dat)==x))   
  dat <- dat[c(iskey, setdiff(1:ncol(dat), iskey))]   # 集計後の列の順番を整理する。
  ncoltg <- levels(as.factor(dat[[tg]]))
  for(i in 1:length(keys)){     # keys中のkeyの個数
    n <- dim(combn(keys, i))[2] # keys中からi個のkeyを選ぶときのcombinationの個数
    for(j in 1:n){
      k <- combn(keys,i)[,j]
      grp_cols <- c(k, tg) # keys中からi個のkeyを選ぶときのcombinationのj番目
      dots <- lapply(grp_cols, as.symbol)
      a <- dat %>% 
        dplyr::group_by_(.dots=dots) %>% 
        dplyr::count_(dots) %>% 
        na.omit %>% 
        spread_(tg, "n", fill=0)
      b <- data.frame(prop.table(as.matrix(a[,ncoltg]), margin=1)*100) %>% round(digits=1)
      names(b) <- paste(levels(as.factor(dat[[tg]])), "(%)", sep="")
      Sum <- apply(a[,ncoltg],1,sum)
      c <- cbind(a[,k], Sum, a[,ncoltg], b)
      y[stringr::str_c(k, collapse="_")] <- list(c)
      # y <- append(y, list(c))
    }
  }
  return(y)
}

# ---------Function "crossdf_01_p"---------
# 
crossdf_01_p <- function(dat, keys, tg){
  y <- list()
  iskey <- sapply(keys, function(x)which(colnames(dat)==x))   
  dat <- dat[c(iskey, setdiff(1:ncol(dat), iskey))]   # 集計後の列の順番を整理する。
  ncoltg <- levels(as.factor(dat[[tg]]))
  for(i in 1:length(keys)){     # keys中のkeyの個数
    n <- dim(combn(keys, i))[2] # keys中からi個のkeyを選ぶときのcombinationの個数
    for(j in 1:n){
      k <- combn(keys,i)[,j]
      grp_cols <- c(k, tg) # keys中からi個のkeyを選ぶときのcombinationのj番目
      dots <- lapply(grp_cols, as.symbol)
      a <- dat %>% 
        dplyr::group_by_(.dots=dots) %>% 
        dplyr::count_(dots) %>% 
        na.omit %>% 
        spread_(tg, "n", fill=0)
      b <- data.frame(prop.table(as.matrix(a[,ncoltg]), margin=1)) %>% round(digits=3)
      names(b) <- paste(levels(as.factor(dat[[tg]])), "(%)", sep="")
      Sum <- apply(a[,ncoltg],1,sum)
      c <- cbind(a[,k], Sum, a[,ncoltg], b)
      y[stringr::str_c(k, collapse="_")] <- list(c)
      # y <- append(y, list(c))
    }
  }
  return(y)
}

# ---------Function "crossdf_01"---------
# 1である確率をpとしたlogitをつける。
# ------------------注：st$future_seisinfはベクタ形式だが、
# st[,]などはlist形式となるため、is.elementなどの集合演算の結果は異なる。わかるかそんなん。泣ける。
# unlistか、たぶんst[["a1"]]などを使えばできる。
crossdf_01 <- function(dat, keys, tg){
  y <- list()
  if(sum(!is.element(as.character(dat[[tg]]), c("0","1")))==0){
    iskey <- sapply(keys, function(x)which(colnames(dat)==x))   
    dat <- dat[c(iskey, setdiff(1:ncol(dat), iskey))]   # 集計後の列の順番を整理する。
    ncoltg <- levels(as.factor(dat[[tg]]))
    for(i in 1:length(keys)){     # keys中のkeyの個数
      n <- dim(combn(keys, i))[2] # keys中からi個のkeyを選ぶときのcombinationの個数
      for(j in 1:n){
        k <- combn(keys,i)[,j]
        grp_cols <- c(k, tg) # keys中からi個のkeyを選ぶときのcombinationのj番目
        dots <- lapply(grp_cols, as.symbol)
        a <- dat %>% 
          dplyr::group_by_(.dots=dots) %>% 
          count_(dots) %>% 
          spread_(tg, "n", fill=0)
        aa <- bind_rows(a, lapply(a, sum))
        d <- aa %>% 
          transmute(p=`1`/(`0`+`1`), `p(%)`= p*100, `logit(p)`=log(p/(1-p))) %>% 
          dplyr::select(-p)
        Sum <- data.frame(sum=apply(aa[,ncoltg],1,sum))
        c <- bind_cols(aa[,k], Sum, aa[,names(aa)!=k], d)
        y <- append(y, list(c))
      }
    }
  }
  return(y)
}

# ---------Function "optsplit2"---------
# !old optsplit2を使うべし
# 入力split数に応じて、なるべく件数が均等になるようにvectorをカテゴライズする。
# input
# x:vector, key:character, split:integer
# output
# vector, type:factor
optsplit <- function(x, split=10, include.lowest=TRUE, right=FALSE){
  k <- x %>% data.frame %>% names
  mutate_call <- lazyeval::interp(~ cume_dist(desc(var)), var = as.name(k))
  temp <- x %>% 
    data.frame %>% 
    arrange_(lazyeval::interp(~var, var = as.name(k))) %>% 
    mutate_(.dots = setNames(list(mutate_call), "cume_dist")) %>% 
    mutate(groupid = split+1)
  for(i in 1:split){
    frac <- (1/split)*i
    cmd <- temp$cume_dist
    trow <- max(which(abs(cmd-frac)==min(abs(cmd-frac)))) 
    temp[seq(1, trow, by=1),]$groupid <- temp[seq(1:trow),]$groupid - 1
  }
  cuts <- temp %>%
    dplyr::group_by(groupid) %>% 
    dplyr::filter(row_number()==n()) %>% 
    select_(k) %>% 
    `[[`(2) %>% 
    as.vector
  if(min(cuts)!=min(x)){
    aa <- cut(x, breaks = c(min(x), cuts), include.lowest = include.lowest, right=right)
  }else{
    aa <- cut(x, breaks = c(cuts), include.lowest = include.lowest, right=right)
  }
  return(aa)
}

optsplit2 <- function(x, split, include.lowest=TRUE, right=FALSE){
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

# ---------Function "opcross11"---------
# １つの目的変数、複数の説明変数でクロス集計表リストを作成
# dataframe data (tbl.dfでも可)
# int xplist, ysplit (クロス集計用のカテゴリ分割数)
# charactor vars (クロス集計の表側に来る変数(複数可))
# charactor tg (クロス集計の表頭に来る変数)
opcross11 <- function(dat, xsplit, ysplit, vars, tg){
  y <- list()
  if(ysplit > 0){dat[tg] <- lapply(dat[tg], optsplit2, split=ysplit)}
  if(xsplit > 0){dat[vars] <- lapply(dat[vars], optsplit2, split=xsplit)}
  for(var in vars){
    if(nlevels(factor(as.numeric(dat[[tg]]))) > 5){
      warning("Width of the cross table may be too wide.")
    }
    y[var] <- crossdf_1_p(dat, var, tg)
  }
  y
}

# ---------Function "opcrossAll"---------
# １つの目的変数、複数の説明変数でクロス集計表リストを作成
# ただし、複数説明変数の全組み合わせに対してもクロス集計を作る。
# dataframe data (tbl.dfでも可)
# int xplist, ysplit (クロス集計用のカテゴリ分割数)
# charactor vars (クロス集計の表側に来る変数(複数可))
# charactor tg (クロス集計の表頭に来る変数
opcrossAll <- function(dat, xsplit, ysplit, vars, tg){
  y <- list()
  if(ysplit > 0){dat[tg] <- lapply(dat[tg], optsplit2, split=ysplit)}
  if(xsplit > 0){dat[vars] <- lapply(dat[vars], optsplit2, split=xsplit)}
  z <- crossdf_1_p(dat, vars, tg)
}

# ---------Function "crossoutput"---------
# クロス集計表リストをエクセルにはりつける
# 直前に例えば NewWb <- createWorkbook(creator = "TomoyaSaito")
# 直後に例えば saveWorkbook(wb = NewWb, file=filename, overwrite = TRUE) が必要
crossoutput <- function(tablelist, sheetname="crosstables"){
  require("openxlsx")
  tablenames <- names(tablelist)
  addWorksheet(wb = NewWb, sheetName = sheetname , gridLines = T)
  startR <- 2
  for(nm in tablenames){
    ydf <- tablelist[[nm]]
    writeData(wb = NewWb, sheet = sheetname, x = ydf, startCol = 2, startRow = startR, borders = "surrounding")
    startR <- startR+dim(ydf)[1]+1
    print(nm)
  }
}

# ---------Function "tcross"---------
# テーブル型クロス集計表を作る
# Input : data.frame
# Output: table
tcross <- function(x, key, tg){
  a <- xtabs(data=x, formula = paste0(key, "+", tg)) %>% addmargins(1)
  factornames <- dimnames(a) %>% names
  b <- a %>% addmargins(2)
  c <- a %>% prop.table(1) 
  d <- cbind(b,c) 
  names(dimnames(d)) <- factornames
  return(d)
}

# ---------Function "round_5up"---------
# 通常の四捨五入関数
round_5up <- function(x, digits=0){
  if(floor(x*10^(digits+1)) %% 10 == 5){
    round(x+sign(x)*1*10^(-1*(digits+1)), digits)
  }else{
    round(x, digits)
  }
}

# ---------Function "readdata"---------
# ファイル読み込み関数。warningなどは無視し、localeも固定している。
readdata <- function(filename, foldername){
  fileext <- rev(unlist(str_split(string=filename, "\\.")))[1]
  if(!fileext %in% c("csv", "txt", "xlsx")){
    stop(paste0("Invalid extention - ", "filename"))
  }else if(fileext=="xlsx"){
    out <- readxlsheets(filename=filename, foldername=foldername)
  }else if(fileext=="csv"){
    out <- read_csv(file=paste0(foldername, "/", filename), locale=locale(encoding="CP932"))
  }else if(fileext=="txt"){
    out <- read_tsv(file=paste0(foldername, "/", filename), locale=locale(encoding="CP932"))
  }
  out
}
# ---------Function "readxlsheets"---------
readxlsheets <- function(filename, foldername){
  i <- 2
  y <- list()
  while(1){
    datasheet <- tryCatch(read_excel(paste0(foldername, "/", filename), sheet = i), error=function(e)return("e"), silent=TRUE)
    if(datasheet=="e"){break}
    y[[i]] <- datasheet
    i <- i+1
  }
  do.call(bind_rows, y)
}

