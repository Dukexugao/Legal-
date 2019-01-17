setwd("C:/Local/Legal Scoring/Version1")
install.packages("readxl")
install.packages("gdata")
install.packages("stringr")
install.packages("xlsx")
install.packages("forcats")
install.packages("lubridate")
install.packages("caret")
install.packages("dplyr")
install.packages("Metrics")
install.packages("ggthemes")
install.packages("rJava")
install.packages("lubridate", dependencies = TRUE)
install.packages('ade4')
install.packages("regsubsets")
install.packages("stringi")


.libPaths()

library(ade4)
library(rJava)
library(readxl) # read_excel
library(gdata) # read.xls
library(stringr)
library(xlsx)
library(forcats)
library(lubridate)
library(caret)
library(dplyr)
library(Metrics)
library(ggthemes)
library(stringi)

is.empty <-function(string){
  if(nchar(string)==0|nchar(gsub(" ","",string))==0 | string=='-'|string=="....."|is.na(string)|string=='..'|string=="/  /"){
    return(TRUE)
  }else{
    return (FALSE)
  }
}


filterZeroVar <- function(df){
  nzv <- nearZeroVar(df)
  return(df[,-nzv])
}




#ClientDetails = data.frame(read.xls("ClientDetails_S&APortoflio_2018-04-09.xls",sheet=1))
#AdditionalDetails = data.frame(read.xls("AdditionalDetails_S&APortfolio_2018-04-09.xls",sheet=1))
#JudgementDetails = data.frame(read.xls("JudgementDetails_S&APortfolio_2018-04-09.xls",sheet=1))


ClientDetails = data.frame(read.csv("ftp1/Temp/ClientDetails_S&APortoflio_2018-04-09.csv"))
AdditionalDetails = data.frame(read.csv("ftp1/Temp/AdditionalDetails_S&APortfolio_2018-04-09.csv"))
JudgementDetails = data.frame(read.csv("ftp1/Temp/JudgementDetails_S&APortfolio_2018-04-09.csv"))
AccountAttributes = data.frame(read.csv("ftp1/Temp/AccountAttributes_S&APortfolio_2018-04-09.csv",sep="\t"))
Debtor_mastertable = data.frame(read.csv("ftp1/debtor_mastertable_2012-2018.csv"))
LCS_data = data.frame(read.csv("ftp2/LCS_Data_append_201809.csv"))



colnames(Debtor_mastertable)[1] <- "QLAW.Number"
colnames(AccountAttributes)[1] <- "QLAW.Number"
colnames(LCS_data)[1] <- "QLAW.Number"

df = Reduce(function(x,y) merge(x, y, by="QLAW.Number",suffixes = c(".ClientDetails", ".AdditionalDetails", ".JudgementDetails","AccountAttributes","Debtor_mastertable", "LCS_data")),
            list(ClientDetails,AdditionalDetails,JudgementDetails,AccountAttributes,Debtor_mastertable, LCS_data))
merge_df = df[, !duplicated(t(df))]
final_df <- filterZeroVar(merge_df)


# order of datetime
date_column = c("Last_Worked.ClientDetails", "Judgment_Expiration","Judgment_Renewal","lwork" )
for (i in colnames(final_df)){
  if(i =="Alert_Date"){    
    new_col_var = "order_Alert_Date"
    final_df[,new_col_var] =  as.numeric(as.factor(as.Date(parse_date_time(final_df[,i],"my"))))
    final_df = final_df[,!(names(final_df) %in% "Alert_Date" )]
  }else if((grepl('date',tolower(i),fixed=TRUE) |i %in% date_column) & i !='Alert_Date' ){
    new_col_var = paste("order_of_",i,sep='')
    final_df[,new_col_var] =  as.numeric(as.factor(as.Date(parse_date_time(final_df[,i],"mdy"))))
    final_df[as.logical(lapply(final_df[,new_col_var],is.na)),new_col_var]<-0
    final_df = final_df[,!(names(final_df) %in% i)]
  }
}


#delete_column = c("Last.Garn.Date","Last_Garn_Date","letter","inttype","letdate","cdate","fc","poeinfo","dletcount","oletcount","deptview","deptsort","assfinclas","ins2","lastbill","billto","billcount","relfile","matter","bill","fee_code","billdate","chgcode","chgrate","stmtype","depart","ccc","preourfile")
#strip_dollar_sign = c("Interest_Rate","Total.Collections.ClientDetails", "Assigned.Amount.ClientDetails","Current.Balance.ClientDetails", "Total.Collections.AccountAttributes","Assigned.Amount.AccountAttributes","Current.Balance.AccountAttributes","Judgment.Principal")

# strip_dollar_sign and make interest rate numeric
strip_dollar_sign = c("Assigned.Amount.ClientDetails","Current.Balance.ClientDetails", "Judgment.Principal","Assigned.Amount.AdditionalDetails","Current.Balance.AdditionalDetails","Judgment.Principal")
for (col in strip_dollar_sign){
  final_df[,col] <- as.numeric(gsub('[$,()]', '', final_df[,col]))
}

final_df[,"Interest_Rate"] = as.numeric(final_df[,"Interest_Rate"])


# delete "QLAW.Number" and dzip
for (col in c("QLAW.Number","dzip.ClientDetails","dzip.AdditionalDetails","dwphone","ezip","dphone","dcity","arecloc","relfile")){
  final_df = final_df[,!(names(final_df) %in% col)]
}
for (fea in colnames(final_df)){
  if ( class(final_df[,fea])=="factor" & length(unique(final_df[,fea]))>500)
  { 
    #print(fea)
    #print(length(unique(final_df[,fea])))
    final_df = final_df[,!(names(final_df) %in% fea)]
}
}

for (fea in colnames(final_df)){
  if (class(final_df[,fea])=="factor" & fea!="Payer"){
    final_df[,fea] = as.character(final_df[,fea])
    final_df[as.logical(lapply(final_df[,fea], is.empty)),fea] <- paste('Missing',fea) 
    #final_df[,fea] = fct_explicit_na(final_df[,fea],paste(fea,'missing'))
    final_df[,fea] = as.factor(final_df[,fea])
    #dumm = as.data.frame(model.matrix(~final_df[,fea])[,-1])
  }else{
    message("not a factor ",fea)
  }
}

factor <-function(col,df){
  if(class(df[,col])=='factor'){
    return (TRUE)
  }
  else{return (FALSE)}
}

one_hot_cols = as.logical(lapply(colnames(final_df),factor,df=final_df))



exp_cols = acm.disjonctif(final_df[,one_hot_cols])
data = cbind(final_df[,!one_hot_cols],exp_cols)

write.csv(data,'data.csv')
