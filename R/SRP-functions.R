## 青木先生function
# 
# http://aoki2.si.gunma-u.ac.jp/R/src/make.dummy.R
# アイテムデータをカテゴリーデータに変換する
make.dummy <- function(dat)
{
  ncat <- ncol(dat)
  dat[, 1:ncat]  <- lapply(dat, function(x) {
    if (is.factor(x)) {
      return(as.integer(x))
    }
    else {
      return(x)
    }
  })
  mx <- sapply(dat, max)
  start <- c(0, cumsum(mx)[1:(ncat-1)])
  nobe <- sum(mx)
  retv <- t(apply(dat, 1,
                  function(obs)
                  {
                    zeros <- numeric(nobe)
                    zeros[start+obs] <- 1
                    zeros
                  }
  ))
  return(retv)
}

# 中１男、中１女　....　高３男、高３女 
gakusex <- paste(rep(c(paste(c(rep("中",3),rep("高",3)),c("１","２","３"),sep="")),each=2),
                 c("男","女"),sep="")


#SRP2013 用 functions
#=====================
#データ確認用のfunctionを以下に定義する。

makeSAtable0 <- function(nun,.dd){
  .tbl1 <- table(.dd$性別,.dd[,nun],exclude=NULL)
  .tbl1 <- .tbl1[1:nrow(.tbl1)-1,]
  .tbl2 <- table(.dd$学年,.dd[,nun],exclude=NULL)
  .tbl2 <- .tbl2[1:nrow(.tbl2)-1,]
  .tbl3 <- table(.dd$学校,.dd[,nun],exclude=NULL)
  .tbl3 <- .tbl3[1:nrow(.tbl3)-1,]
  .tbl4 <- table(.dd[,nun],exclude=NULL)
  .tbl5 <- rbind(.tbl1,.tbl2,.tbl3,.tbl4)
  SUM <- margin.table(.tbl5,1)
  .tbl6 <- cbind(.tbl5,SUM)
  rownames(.tbl6)[length(rownames(.tbl6))] <- "全体"
  return(.tbl6)
}

makeSAtable <- function(start,end,.dd){
  for(i in start:end){
    print(names(.dd)[i])
    .tbl1 <- table(.dd$性別,.dd[,i],exclude=NULL)
    #      print (margin.table(.tbl1))
    .tbl2 <- table(.dd$学年,.dd[,i],exclude=NULL)
    #      print (margin.table(.tbl2))
    .tbl3 <- table(.dd$学校,.dd[,i],exclude=NULL)
    #      print (margin.table(.tbl3))
    .tbl4 <- table(.dd[,i],exclude=NULL)
    #      print (margin.table(.tbl4))
    .tbl5 <- rbind(.tbl1,.tbl2,.tbl3,.tbl4)
    SUM <- margin.table(.tbl5,1)
    .tbl6 <- cbind(.tbl5,SUM)
    #print(.tbl6)
  }
  return(.tbl6)
}

makeSAtable2 <- function(nun,.dd){# result はlist [1] 度数テーブル、[2]％テープル
  .tbl1 <- makeSAtable0(nun,.dd)
  #  print(.tbl)
  .tbl2 <- round(100*prop.table(.tbl1[,1:length(colnames(.tbl1))-1],1),2)### <- check
  res <- list(.tbl1,.tbl2)
}

makeMAtable1 <- function(colstart,colend,.dd){
  print(paste(names(.dd)[colstart],"〜",names(.dd)[colend]))
  .tbl <- NULL
  for(i in colstart:colend){
    .tbl <- rbind(.tbl,table(.dd[,i],exclude=NULL))
    .tbl <- .tbl[1:length(names(.tbl))-1,]
  }
  SUM <- margin.table(.tbl,1)
  .tbl <- cbind(.tbl,SUM)
  print(.tbl)
}
makeMAtable2 <- function(colstart,colend,.dd){
#  print(paste(names(.dd)[colstart],"〜",names(.dd)[colend]))
  .tbl <- NULL
  .tblb <- NULL
  for(i in colstart:colend){
    .tbla <- table(.dd$性別,.dd[,i])
    .tbla <- rbind(.tbla,table(.dd$学年,.dd[,i]))
    .tbla <- rbind(.tbla,table(.dd$学校,.dd[,i]))
    .tbla <- rbind(.tbla,table(.dd[,i]))
    SUM <- margin.table(.tbla,1)
    .tbl <- cbind(.tbla,SUM)
    .tblb <- cbind(.tblb,.tbl[,2])
    #    print(.tbl)
  }
  TOTAL <- margin.table(.tblb,1)
  cat_TOTAL <- c(table(.dd$性別),table(.dd$学年),table(.dd$学校),nrow(.dd))
  .tblc <- cbind(.tblb,cat_TOTAL,TOTAL)
  colnames(.tblc) <- c(1:(colend-colstart+1),"属性計","回答者数")
  rownames(.tblc)[length(rownames(.tblc))] <- "全体"
  return(.tblc)
}

grey_step <- function(n){
  return(grey(c(1:n)*1/n))
}

#-------
# 2014-01-17
makeSAtable01 <- function(qname,.dd){ # nn には変数名 ex:"設問１"
  #  .tbl1 <- table(.dd$性別,.dd[,nun],exclude=NULL)
  eval(parse(text=paste(".tbl1 <- with(.dd,table(性別,",qname,",exclude=NULL))")))
  .tbl1 <- .tbl1[1:nrow(.tbl1)-1,]
  
  #  .tbl2 <- table(.dd$学年,.dd[,nun],exclude=NULL)
  eval(parse(text=paste(".tbl2 <- with(.dd,table(学年,",qname,",exclude=NULL))")))
  .tbl2 <- .tbl2[1:nrow(.tbl2)-1,]
  
  #  .tbl3 <- table(.dd$学校,.dd[,nun],exclude=NULL)
  eval(parse(text=paste(".tbl3 <- with(.dd,table(学校,",qname,",exclude=NULL))")))
  .tbl3 <- .tbl3[1:nrow(.tbl3)-1,]
  
  #  .tbl4 <- table(.dd[,nun],exclude=NULL)
  eval(parse(text=paste(".tbl4 <- with(.dd,table(",qname,",exclude=NULL))")))
  
  .tbl5 <- rbind(.tbl1,.tbl2,.tbl3,.tbl4)
  SUM <- margin.table(.tbl5,1)
  .tbl6 <- cbind(.tbl5,SUM)
  rownames(.tbl6)[length(rownames(.tbl6))] <- "全体"
  return(.tbl6)
}

makeSAtableGS <- function(qname,.dd){#学年別性別クロス
  eval(parse(text=paste(".tbl <- ftable(
                        with(.dd,table(学年,性別,",qname,")))",sep="")))
  rownames(.tbl) <- paste(paste(rep(c("中","高"),each=6),
                                rep(c("１","２","３"),each=2),sep=""
  ),c("男","女"),sep="")
  return (.tbl)
}

