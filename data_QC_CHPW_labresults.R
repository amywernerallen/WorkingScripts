###############################################################

#### DATA QUALITY CHECKS
####
#### To be used for looking into the quality of vitals tables
####
#### Written by: A. Werner-Allen
#### Arcadia Solutions 8/26/2014

################################################################

#### Background:
#### This script is intended to dive more into the vitals base table
#### allowing the user to look at the quality of the data.
#### What is of interest are anomolies and outliers; data that does
#### not seem to make sense. We do that here by looking at the 
#### distribution of values to find outliers, determining appropriate
#### "good" cutoffs, examining the spread of the values to see if any
#### trends are present, and looking at averages to ensure that the 
#### data falls in the intuitive range.

#### This current script looks at data from:
#### SLVTN_WARESHOUSE_DEV (database) on qdwsqldev05 (server)

################################################################

#### Packages needed:

# Connect remotely to correct ODBC (create a new one if necessary)
# install.packages("RODBC")
library(RODBC)
myconn <- odbcConnect("SLVTN_Warehouse_Dev")

# Use ggplot2 for creating nice-looking plots
# install.packages("ggplot2")
library(ggplot2)

# Use outlier package for running Grubbs test
library(outliers)

grubbs.flag = function(x) {
  outliers = NULL
  test = x
  grubbs.result = grubbs.test(test)
  pv <- grubbs.result$p.value
  while(pv < 0.05) {
    outliers <- c(outliers,as.numeric(strsplit(grubbs.result$alternative," ")[[1]][3]))
    test <- x[!x %in% outliers]
    grubbs.result <- grubbs.test(test)
    pv <- grubbs.result$p.value
  }
  return(data.frame(X=x,Outlier=(x %in% outliers)))
}

as.numeric.factor <- function(x) {as.numeric(levels(x))[x]}

########################## LAB RESULTS TABLE #########################

# The following are the types of labs that are run:
# select distinct lab_type from t_result
# a1c
# hdl
# ldl
# microalbumin
# Needs Update
# pap
# Total Cholesterol
# Triglycerides

###################################################################
#### A1c Data Check 

# Clean data
a1c = sqlQuery(myconn, "select * from t_result where pat_id in (select enc_patient_id from t_encounter where enc_timestamp >= dateadd(day, 1, dateadd(month, -18 , '01-01-2014')) ) and lab_type ='a1c'")
a1c = sapply(a1c, as.character)
a1c[is.na(a1c)] = " "
a1c = as.data.frame(a1c)

length(grep(pattern = "%", x = a1c$result_value)) # How many have %? 168
a1c$result_value = sub("%", "", a1c$result_value) # Remove % sign

length(grep(pattern = ">", x = a1c$result_value)) # How many have >? 6
a1c$result_value = sub(">", "", a1c$result_value) # Remove > sign

length(grep(pattern = "<", x = a1c$result_value)) # How many have >? 0
a1c$result_value = sub("<", "", a1c$result_value)

a1c$result_value = as.numeric.factor(a1c$result_value) # Text converted to NA
a1c$result_value_numeric = as.numeric.factor(a1c$result_value_numeric)
a1c[is.na(a1c)] = " "
a1c = as.data.frame(a1c)

# Units recorded correctly:
length(grep(pattern = "%", x = a1c$result_units)) # 42308 correct
a1c$result_units = c("%")

# Average value:
mean(a1c$result_value_numeric, na.rm=TRUE) #7.130701
median(a1c$result_value_numeric, na.rm=TRUE) #6.6
sd(a1c$result_value_numeric, na.rm=TRUE) #1.637653

#### Look at results
# Max values
max(a1c$result_value_numeric[!is.na(a1c$result_value_numeric)]) # 702
which.max(a1c$result_value_numeric) # 9668
a1c[9668,] # This seems problematic

# Outliers
a1c_flag = grubbs.flag(a1c$result_value_numeric[!is.na(a1c$result_value_numeric)])
dim(a1c_flag[a1c_flag$Outlier==TRUE,])[1] #37
min(a1c_flag[a1c_flag$Outlier==TRUE,1]) # 14.6
a1c_good = a1c_flag[a1c_flag$Outlier==FALSE,1]
a1c_good = as.data.frame(a1c_good)
ggplot(a1c_flag ,aes(x=X,fill=Outlier))+ geom_histogram(binwidth = 0.1)+ theme_bw() + xlab("A1c Values")

# Min results
min(a1c$result_value[a1c$result_value != " "]) # 1.3

# Average value:
a1c_average = mean(a1c_good$a1c_good, na.rm=TRUE) #7.034016
a1c_median =median(a1c_good$a1c_good, na.rm=TRUE) #6.6
a1c_dev = sd(a1c_good$a1c_good, na.rm=TRUE) #1.637653

# Plots without outliers:
a1c_plot =  ggplot(a1c_good, aes(x = a1c_good))
a1c_plot + geom_histogram(aes(fill = ..count..)) + xlab("A1C Values")
a1c_plot + geom_density(colour="darkgreen", fill="green") + xlab("A1C Values")

# Cutoff for Diabetes Measure:
dim(a1c[a1c$result_value_numeric > 9,])[1] #2525 over 9
dim(a1c)[1] #20352 total

# Missing Values:
dim(a1c[a1c$result_value_numeric == " ",])[1] #246 missing values


###################################################################
#### hdl Data Check

# Clean data
hdl = sqlQuery(myconn, "SELECT result_type, result_value, result_units, result_value_numeric, result_min, result_max, result_out_of_range, result_status from t_result where lab_type ='hdl'")
hdl = sapply(hdl, as.character)
hdl[is.na(hdl)] = " "
hdl = as.data.frame(hdl)

length(grep(pattern = ">", x = hdl$result_value)) # None
length(grep(pattern = "<", x = hdl$result_value)) # 11 with symbol
hdl$result_value = sub("<", "", hdl$result_value)

hdl$result_value = as.numeric(hdl$result_value) # Text converted to NA
hdl$result_value_numeric = as.numeric.factor(hdl$result_value_numeric)
hdl[is.na(hdl)] = " "
hdl = as.data.frame(hdl)

# Units recorded correctly:
length(grep(pattern = "mg/dL", x = hdl$result_units)) # 38061 correct
hdl$result_units = c("mg/dL")

#### Look at results
# Max/Min values
max(hdl$result_value_numeric[!is.na(hdl$result_value_numeric)]) #187
min(hdl$result_value_numeric[!is.na(hdl$result_value_numeric)]) #0

# Plots:
hdl_plot =  ggplot(hdl, aes(x = result_value_numeric))
hdl_plot + geom_histogram(aes(fill = ..count..)) + xlab("HDL Values")

# Outliers
hdl_flag = grubbs.flag(hdl$result_value_numeric)
dim(hdl_flag[hdl_flag$Outlier==TRUE,])[1] #67 outliers
min(hdl_flag[hdl_flag$Outlier==TRUE,1]) # 113 is cutoff
hdl_good = hdl_flag[hdl_flag$Outlier==FALSE,1]
hdl_good = as.data.frame(hdl_good)

# Averages
hdl_average = mean(hdl_good$hdl_good, na.rm=TRUE) #46.58245
hdl_median =median(hdl_good$hdl_good, na.rm=TRUE) #44
hdl_dev = sd(hdl_good$hdl_good, na.rm=TRUE) #14.03092 (!!!)

# Plots (no outliers):
hdl_plot =  ggplot(hdl_good, aes(x = hdl_good))
hdl_plot + geom_histogram(aes(fill = ..count..)) + xlab("HDL Values")
hdl_plot + geom_density(colour="darkgreen", fill="green") + xlab("HDL Values")

dim(hdl[hdl$result_value_numeric > 100,])[1] #705 over 100mg/dL
dim(hdl)[1] #38674 total

dim(hdl[hdl$result_value_numeric == " ",])[1] #490 missing values

###################################################################
#### ldl Data Check

# Clean data
ldl = sqlQuery(myconn, "SELECT result_type, result_value, result_units, result_value_numeric, result_min, result_max, result_out_of_range, result_status from t_result where lab_type ='ldl'")
ldl = sapply(ldl, as.character)
ldl[is.na(ldl)] = " "
ldl = as.data.frame(ldl)

length(grep(pattern = ">", x = ldl$result_value)) # None
length(grep(pattern = "<", x = ldl$result_value)) # None

ldl$result_value = as.numeric(ldl$result_value) # Text converted to NA
ldl$result_value_numeric = as.numeric.factor(ldl$result_value_numeric)

# Units recorded correctly:
length(grep(pattern = "mg/dL", x = ldl$result_units)) # 37989 correct
ldl$result_units = c("mg/dL")

#### Look at results
# Max/Min values
max(ldl$result_value_numeric[!is.na(ldl$result_value_numeric)]) #354
min(ldl$result_value_numeric[!is.na(ldl$result_value_numeric)]) #-94 Negative values?
length(ldl$result_value_numeric[ldl$result_value_numeric <= 0]) #7 negatives

# Plots:
ldl_plot =  ggplot(ldl, aes(x = result_value_numeric))
ldl_plot + geom_histogram(aes(fill = ..count..)) + xlab("ldl Values")

# Outliers
ldl_flag = grubbs.flag(ldl$result_value_numeric)
dim(ldl_flag[ldl_flag$Outlier==TRUE,])[1] #19 outliers
ldl_good = ldl_flag[ldl_flag$Outlier==FALSE,1]
ldl_good = as.data.frame(ldl_good)

# Averages
ldl_average = mean(ldl_good$ldl_good, na.rm=TRUE) #110.4251
ldl_median =median(ldl_good$ldl_good, na.rm=TRUE) #44
ldl_dev = sd(ldl_good$ldl_good, na.rm=TRUE) #14.03092 (!!!)

# Plots (no outliers):
ldl_plot =  ggplot(ldl_good, aes(x = ldl_good))
ldl_plot + geom_histogram(aes(fill = ..count..)) + xlab("ldl Values")
ldl_plot + geom_density(colour="darkgreen", fill="green") + xlab("ldl Values")

dim(ldl[ldl$result_value_numeric > 100,])[1] #22592 over 100mg/dL
dim(ldl)[1] #38160 total

dim(ldl[ldl$result_value_numeric == " ",])[1] #1037 missing values


