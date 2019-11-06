library(dplyr)
library(readr)
library(openxlsx)
library(readxl)


parse_txt<-function(txt){
  if(grepl("4600001433", txt)){
    df<-read.delim(txt,skip=1,row.names = NULL,colClasses = 'character')%>%
      select(2:10)
    colnames(df)<-c('CCode','Plnt','Purch.ord.','Item','Order date','Order qty','unit', 'value', 'currency')
    df[,c(6,8)]<-lapply(df[,c(6,8)],parse_number)
    }else{
    df<-read.delim(txt,skip=1,row.names = NULL,colClasses = 'character')%>%
      select(2:8)
    colnames(df)<-c('PO','Item','Order_date','Order_qty','unit','value','currency')
    df[,c(4,6)]<-lapply(df[,c(4,6)],parse_number)
  }
  for(col_name in colnames(df)){
    df <- df[df[, col_name]!='Qty. released to date' & df[, col_name]!='Tgt. qty.' & df[, col_name]!='Open target qty.',]
  }
  df
  #df[df$PO!='Qty. released to date' & df$PO!='Tgt. qty.' & df$PO!='Open target qty.',]
  
}


oa<-list.files('./contract_oa_data/')

ds<-list()

for (i in seq_along(oa)){
  
  folder<-paste0('./contract_oa_data/',oa[i])
  path<-list.files(folder)
  
  df<-list()
  for (j in seq_along(path)){
    #print(paste0(folder,'/',path[j]))
    df[[j]]<-parse_txt(paste0(folder,'/',path[j]))
    
  }
  ds[[i]]<-do.call(rbind,df)
}



spent <- data.frame("OA" = unlist(oa), "used" = NA)
for(i in 1:nrow(spent)){
  oa_code <- spent$OA[i]
  index <- match(oa_code, oa)
  spent$used[i] <- sum(ds[[index]]$value)
}


contract <-read_excel('contract.xlsx',1)

for(i in 1:nrow(contract)){
  if(!is.na(contract$OA[i])){
    index_c <- match(contract$OA[i], spent$OA)
    contract$Remaining[i] <- contract$`Contract value`[i] - spent$used[index_c]
  }
}


l <- list(contract, spent)
write.xlsx(l,file='./contract.xlsx')

