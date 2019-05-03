library(dplyr)
library(caret)
library(e1071)
library(pROC)
library(DMwR)

# for classifiers
library(mboost)
library(gbm)
library(fastAdaboost) 
library(monomvn)
library(lubridate)

# Data Input
payers <- read.csv("DataToUse20190212.csv", header=TRUE, row.names = 1, sep = ',')


CODate <- as.Date(payers$CODate, format="%Y-%m-%d")
ccSOLDate <- as.Date(payers$ccSOLDate, format="%Y-%m-%d")
date_diff<-as.data.frame(ccSOLDate-CODate)
diff_in_days = as.numeric(difftime(ccSOLDate, CODate, units = "days"))
diff_in_years = as.double(diff_in_days)/365


final_df = payers
final_df$diff_date = diff_in_days

#payers = final_df


# order of datetime
#date_column = c("CODate", "ccSOLDate")
date_column = c("Assignment.Date", "InterestStartDate", "IssuerLPDate", "AccountOpenDate", "ccLastPDate", "ccLastMoveDate", "ScheduledReportDate")
for (i in colnames(final_df)){
  if(i =="Alert_Date"){    
    new_col_var = "order_Alert_Date"
    final_df[,new_col_var] =  as.numeric(as.factor(as.Date(parse_date_time(final_df[,i],"my"))))
    final_df = final_df[,!(names(final_df) %in% "Alert_Date" )]
  }else if((grepl('Date',tolower(i),fixed=TRUE) |i %in% date_column) & i !='Alert_Date' ){
 #}else if((grepl('Date',tolower(i),fixed=TRUE) |i  !='Alert_Date' )){
    new_col_var = paste("order_of_",i,sep='')
    #final_df[,new_col_var] =  as.numeric(as.factor(as.Date(parse_date_time(final_df[,i],"ymdHMS"))))
    final_df[,new_col_var] =  as.numeric(as.factor(as.Date(final_df[,i],format="%Y-%m-%d")))
    final_df[as.logical(lapply(final_df[,new_col_var],is.na)),new_col_var]<-0
    final_df = final_df[,!(names(final_df) %in% i)]
  }
}


# select only numerical attributes for now
#payers_num <- select_if(payers, is.numeric)
payers_num <- select_if(final_df, is.numeric)
payers <- payers_num


# Delete columns with containing all zeros
payers <- payers[, which(colSums(payers)!= 0)]
#str(payers)

# Replace NA with zeors
payers[is.na(payers)] <- 0

payers$diff_date = diff_in_days
payers[is.na(payers)] <- 0
reduced = payers
reduced$Payer <- factor(reduced$Payer, levels = c(0,1), labels = c(0,1))

reduced <- reduced[ , -which(names(reduced) %in% c("AccountID", "OURFILE", "DomainStatusID"))]


names <- colnames(reduced)
print(names)


#ctrl <- trainControl(method = "cv", number = 2)

#tbmodel <- train(Payer ~ ., data = reduced, method = "rf", trControl = ctrl)  

tbmodel <- readRDS("./Model1_single.rds")
print(tbmodel)


#test_data <- read.csv("TestData_v3.csv", header=TRUE, row.names = 1, sep = ',')
test_data <- read.csv("Test_Data_new.csv", header=TRUE, row.names = 1, sep = ',')


#df <- data.frame(df[,-1], row.names = df[,1])
#test_data <- df
# remove columns that are single value or all null
index = 1
pred_list = vector()
pred_prob_list = vector()
while (index <= dim(test_data)[1]) { 
#while (index <= 100) { 
	test_data_final = Filter(function(x) !(all(x=="NULL")), test_data[index,])
	current_names = colnames(test_data_final)
	if (('CODate' %in% current_names) & ('ccSOLDate' %in% current_names) ){
    		#CODate <- as.Date(test_data_final$CODate, format="%m/%d/%Y")
    		CODate <- as.Date(test_data_final$CODate, format="%Y-%m-%d")
    		#ccSOLDate <- as.Date(test_data_final$ccSOLDate, format="%m/%d/%Y")
    		ccSOLDate <- as.Date(test_data_final$ccSOLDate, format="%Y-%m-%d")
				  date_diff<-as.data.frame(ccSOLDate-CODate)
    			diff_in_days = as.numeric(difftime(ccSOLDate, CODate, units = "days"))
    			diff_in_years = as.double(diff_in_days)/365
    			test_data_final$diff_date = diff_in_days
	}


	for (i in colnames(test_data_final)){
  	if(i =="Alert_Date"){    
    		new_col_var = "order_Alert_Date"
    		test_data_final[,new_col_var] =  as.numeric(as.factor(as.Date(parse_date_time(test_data_final[,i],"my"))))
    		test_data_final = test_data_final[,!(names(test_data_final) %in% "Alert_Date" )]
  	}else if((grepl('Date',tolower(i),fixed=TRUE) |i %in% date_column) & i !='Alert_Date' ){
 		#}else if((grepl('Date',tolower(i),fixed=TRUE) |i  !='Alert_Date' )){
    		new_col_var = paste("order_of_",i,sep='')
    		#test_data_final[,new_col_var] =  as.numeric(as.factor(as.Date(parse_date_time(test_data_final[,i],"ymdHMS"))))
    		#test_data_final[,new_col_var] =  as.numeric(as.factor(as.Date(test_data_final[,i],format="%m/%d/%Y")))
    		test_data_final[,new_col_var] =  as.numeric(as.factor(as.Date(test_data_final[,i],format="%Y-%m-%d")))
    		#print(test_data_final[,new_col_var])
    		test_data_final[as.logical(lapply(test_data_final[,new_col_var],is.na)),new_col_var]<-0
    		test_data_final = test_data_final[,!(names(test_data_final) %in% i)]
  	}
	}


	test_data_final_1 <- select_if(test_data_final, is.numeric)
	predictors <- names(reduced)[names(reduced) != 'Payer']

	predictors = intersect(colnames(test_data_final_1), predictors)
	print(length(predictors))
	
	for (i in colnames(reduced)){ 
	 	if(! (i %in% predictors)){   
			test_data_final_1[, i] = 0
		}	
	}

	test_data_final_1  = test_data_final_1[ , -which(names(test_data_final_1) %in% c("Payer", "ClientAccountID", "DomainStatusID"))]
	#print(test_data_final_1)
	pred <- predict(tbmodel, test_data_final_1) 
	pred_prob  <- predict(tbmodel, test_data_final_1, "prob")
	#print(pred)
	#print(pred_prob)
	index = index + 1
	pred_list = c(pred_list, pred)
	pred_prob_list = c(pred_prob_list, list(pred_prob))
}



sink("pred_prob_one_model_new.txt")
writeLines(unlist(lapply(pred_prob_list, paste, collapse=" ")))
sink()

sink("pred_one_model_new.txt")
writeLines(unlist(lapply(pred_list, paste, collapse=" ")))
sink()

#saveRDS(tbmodel, "./Model1_single.rds")
