#
# Tools to get Table
# 
# 2015-12-28 (c)kazuo fujimoto kazuo.fujimoto2007@gmail.com

#
# よく使うベクトル
#

gakunen <- c("中１","中２","中３","高１","高２","高３")
seibetsu <- c("男","女")
gaku_sei <- paste(rep(gakunen,each=2),rep(seibetsu,6),sep="")
# 中１男、中１女、.......

#
# 学年、学校、性別、全体に対する各設問のクロスをとる
# 無効回答による列名をexclude で除去するが、そのチェックは、
# chkcolnames() で行う。
# 以下のパラメータで実行
#　　.d0 <- .dd
#　　Qn <- "設問4"
#　　ex_ptn <- c("","1|2")

chkcolnames <- function(.d0,Qn){
  return(names(eval(parse(text= paste("with(.d0,table(",Qn,"))")))))
}

tableall <- function(.d0,Qn,ex_ptn){
  text1 <- paste("with(.d0,table(学年1,",Qn,",exclude=ex_ptn))",sep="")
  text2 <- paste("with(.d0,table(学校,",Qn,",exclude=ex_ptn))",sep="")
  text3 <- paste("with(.d0,table(性別1,",Qn,",exclude=ex_ptn))",sep="")
  
  .tbl.all <- rbind(eval(parse(text=text1)),
                    eval(parse(text=text2)),
                    eval(parse(text=text3)))
#  .tbl.all2 <- addmargins(.tbl.all,2)
  
  .tbl.all3 <- rbind(.tbl.all,margin.table(.tbl.all[1:6,],2))
  rownames(.tbl.all3)[11] <- "合計"
#  colnames(.tbl.all3)[ncol(.tbl.all3)] <- "合計"
  return(.tbl.all3)
}

tableall2 <- function(.d0,n,cnames=NULL){
  ex_ptn <- names(table(.d0[,n])[grep("^$|[1-9].[1-9]*",
                                      names(table(.d0[,n])) )])
  .tbl.all <- rbind(table(.d0$学年1,.d0[,n],exclude = ex_ptn),
                    table(.d0$学校,.d0[,n],exclude = ex_ptn),
                    table(.d0$性別1,.d0[,n],exclude = ex_ptn)
                    )
  .tbl.all3 <- rbind(.tbl.all,margin.table(.tbl.all[1:6,],2))#学年を合計
  rownames(.tbl.all3)[11] <- "合計"
  if(!is.null(cnames)){
    colnames(.tbl.all3) <- cnames
  }
  return(.tbl.all3)
}

#for (i in 15:42){
#  print(names(.dd)[i])
#  print(tableform(tableall2(.dd,i)))
#}

#
# 引数に表データ。
# 周辺度数を加え、行％を一行下に追記してフォーマットする。
#
tableform_stack <- function(.tbl){
  .tbl.rowmar <- margin.table(.tbl,1)
  .tbl.colmar <- margin.table(.tbl,2)
  .tbl.prop <- prop.table(.tbl,1)
  .tbl2 <- rbind(.tbl,margin.table(.tbl,2))
  rownames(.tbl2)[nrow(.tbl2)] <- "合計"
  .tbl.tmp <- NULL
  for(i in 1:nrow(.tbl2)){
    .tbl.tmp <- rbind(.tbl.tmp,.tbl2[i,],
                      round(100*prop.table(.tbl2,1),2)[i,])
  }
  newrownames <- NULL
  for(i in 1:length(rownames(.tbl2))){
    newrownames <-append(
      newrownames,
      c(rownames(.tbl2)[i],paste(rownames(.tbl2)[i],"%")))
  }
  rownames(.tbl.tmp) <- newrownames
  
  newcolmargins <- NULL
  for(i in 1:length(rownames(.tbl2))){
    newcolmargins <- append(newcolmargins,
                            c(margin.table(.tbl2,1)[i],"100")
    )
  }
  合計 <- newcolmargins
  return(cbind(.tbl.tmp,合計))#表示用なので、文字列になっている。
}

#
# 表の整形 度数にたいして、行％表示を横にテーブルとして追加
# 値の属性は、数値のまま
tableform_wide <- function(.tbl){
  .tbl.rowmar <- margin.table(.tbl,1)#行周辺度数
  合計 <- .tbl.rowmar # 合計という名前にする
  .tbl1 <- cbind(.tbl,合計)# .tbl1 に合計の列を付ける。
  .tbl.colmar <- margin.table(.tbl,2)# 列周辺度数
  c合計 <- .tbl.colmar# 列周辺度数にc合計という名前をつける
  .tbl1a <- .tbl1
#  .tbl1a <- rbind(.tbl1,c(c合計,sum(c合計)))# 列合計の行を加える
#  rownames(.tbl1a)[nrow(.tbl1a)] <- "合計"#列合計の行を、合計にする★
  
  .tbl.prop <- prop.table(.tbl1a[,-ncol(.tbl1a)],1)
  .tbl2 <- round(100*.tbl.prop,2)
  colnames(.tbl2) <- paste(colnames(.tbl),"%",sep="_")
  return(cbind(.tbl1a,.tbl2))
}
