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
#### CHPW_WAREHOUSE_DEV (database) on qdwsqldev05 (server)

################################################################

# What should a data quality check look at?
# Severity of inconsistency
# Incompleteness
# Outliers
# Missing / Unknown

#### Packages needed:

# Connect remotely to correct ODBC (create a new one if necessary)
# install.packages("RODBC")
library(RODBC)
myconn <- odbcConnect("CHPW_Warehouse_Dev")

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
    outliers = c(outliers,as.numeric(strsplit(grubbs.result$alternative," ")[[1]][3]))
    test = x[!x %in% outliers]
    grubbs.result = grubbs.test(test)
    pv = grubbs.result$p.value
  }
  return(data.frame(X=x,Outlier=(x %in% outliers)))
}

########################## VTIALS TABLE #########################

#### Output parameters:
# good_heights: percent of heights that are under 100 and over 10 inches
# good_weights: percent of weights that are under 400 and over 2 pounds
# good_BMI: percent of BMIs that are under 100 and over 10
# good_systolic: percent of systolic measurements between 60 and 200
# good_diastolic: percent of diastolic measurements between 50 and 150
# good_temps: percent of temperatures between 80 and 150 F

# average_height: average of good_heights
# average_weight: average of good_weights
# average_BMI: average of good_BMI
# average_systolic: average of good_systolic
# average_diastolic: average of good_diastolic
# average_temp: average of good_temps

###################################################################
#### Height distribution 

height = sqlQuery(myconn, "select vitals_height, vitals_height_unit from t_vitals where vitals_orig_site_id = '6' order by vitals_height")
height = na.omit(height)
height$conversion = 0

# Here we convert cm to in:
for(i in 1:dim(height)[1]) {
  
  unit = height$vitals_height_unit[i]
  
  if(unit == "cm") { 
    height$conversion[i] = height$vitals_height[i] / 2.5 
  }
  
  else {   
    height$conversion[i] = height$vitals_height[i] 
  }
}

# Check to see that conversion worked:
head(height[height$vitals_height_unit == "cm",])
dim(height[height$vitals_height_unit == "cm",]) #74

# Some looks into the data:
height[height$conversion == max(height$conversion),] # what is max? 104 in
dim(height[height$conversion > 100,])[1] # how many are over 100 inches? 619
dim(height[height$conversion < 10,])[1] # how many are under 10 inches? 16
dim(height)[1] # total number: 723
good_heights = dim(height[height$conversion < 100 & height$conversion > 10,])[1]/dim(height)[1] # percent that are good

# Look at distribution of good heights:
height_temp = height[which(height$conversion < 100 & height$conversion > 10),]
average_height = mean(height_temp$conversion)

m = ggplot(height_temp, aes(x=conversion))
m + geom_histogram(aes(fill = ..count..)) + xlab("Height (in)")
#hist(height_temp$conversion, breaks=40, main="Height in Inches", xlab="Height (in)")
qqnorm(height_temp$conversion)
ggplot(height_temp, aes(sample = height_temp$conversion)) + stat_qq(color="firebrick2", alpha=1)  + geom_abline(intercept = mean(height_temp$conversion), slope = sd(height_temp$conversion))

grubbs.test(height$conversion)
height_flag = grubbs.flag(height$conversion)
dim(height_flag[height_flag$Outlier==TRUE,])[1] #83 outliers identified
ggplot(grubbs.flag(height$conversion),aes(x=height$conversion,fill=Outlier))+ geom_histogram(binwidth=diff(range(height$conversion))/30)+ theme_bw() + xlab("Height in Inches")

####################################################################
#### Weight distribution 

weight = sqlQuery(myconn, "select distinct vitals_weight, vitals_weight_unit from t_vitals where vitals_orig_site_id = '6' order by vitals_weight")
weight = na.omit(weight)

# Here we convert kg to lb:
for(i in 1:dim(weight)[1]) {
  
  unit = weight$vitals_weight_unit[i]
  
  if(unit == "kg") {
    weight$conversion[i] = weight$vitals_weight[i] * 2.2
  }
  
  else {
    weight$conversion[i] = weight$vitals_weight[i]
  }
  
}

# Check kg conversion:
head(weight[weight$vitals_weight_unit == "kg",])
dim(weight[weight$vitals_weight_unit == "kg",]) #145

# Some looks into the data:
weight[weight$conversion == max(weight$conversion),] # what is max? 901126 lb
dim(weight[weight$conversion > 400,])[1] # how many are over 400 lbs? 121
dim(weight[weight$conversion < 2,])[1] # how many are under 2 lbs? 6
dim(weight)[1] # total number: 5435
good_weights = dim(weight[weight$conversion < 400 & weight$conversion > 2,])[1]/dim(weight)[1] # percent that are good

# Look at distribution of good weights:
weight_temp = weight[which(weight$conversion < 400 & weight$conversion > 2),] 
average_weight = mean(weight_temp$conversion)

m = ggplot(weight_temp, aes(x=conversion))
m + geom_histogram(aes(fill = ..count..)) + xlab("Weight (lbs)")
ggplot(weight_temp, aes(sample = weight_temp$conversion)) + stat_qq(color="firebrick2", alpha=1)  + geom_abline(intercept = mean(weight_temp$conversion), slope = sd(weight_temp$conversion))

####################################################################
#### BMI 

BMI = sqlQuery(myconn, "select distinct vitals_BMI from t_vitals where vitals_orig_site_id = '6' order by vitals_BMI")
BMI = na.omit(BMI)

# Some looks into the data:
BMI[BMI$vitals_BMI == max(BMI$vitals_BMI),] # what is max? 286852
length(BMI[BMI$vitals_BMI > 60,]) # how many are over 100? 81
length(BMI[BMI$vitals_BMI < 10,]) # how many are under 10? 10
dim(BMI)[1] # total number: 142
good_BMI = length(BMI[BMI$vitals_BMI < 60 & BMI$vitals_BMI > 10,])/dim(BMI)[1] 

# Look at distribution of good BMIs:
BMI_temp = BMI[which(BMI$vitals_BMI < 60 & BMI$vitals_BMI >10),]
BMI_temp = as.data.frame(BMI_temp)
average_BMI = mean(BMI_temp$BMI_temp)

m = ggplot(BMI_temp, aes(x=BMI_temp))
m + geom_histogram(aes(fill = ..count..), binwidth = 0.3) + xlab("BMI")
#hist(BMI_temp$BMI_temp, breaks=40, main="BMI Distribution", xlab="BMI")

grubbs.test(BMI$vitals_BMI)
BMI_flag = grubbs.flag(BMI$vitals_BMI)
dim(BMI_flag[BMI_flag$Outlier==TRUE,])[1] #25 outliers identified
ggplot(grubbs.flag(BMI$vitals_BMI),aes(x=BMI$vitals_BMI,fill=Outlier))+ geom_histogram(binwidth=diff(range(BMI$vitals_BMI))/30)+ theme_bw() + xlab("BMI")


####################################################################
#### Systolic/Diastolic distribution

systolic = sqlQuery(myconn, "select distinct vitals_systolic from t_vitals where vitals_orig_site_id = '6' order by vitals_systolic")
systolic = na.omit(systolic)
diastolic = sqlQuery(myconn, "select distinct vitals_diastolic from t_vitals where vitals_orig_site_id = '6' order by vitals_diastolic")
diastolic = na.omit(diastolic)

# Some looks into the data:
systolic[systolic$vitals_systolic == max(systolic$vitals_systolic),] # what is max? 14068
length(systolic[systolic$vitals_systolic > 200,]) # how many are over 200? 41
length(systolic[systolic$vitals_systolic < 60,]) # how many are under 60? 1
dim(systolic)[1] # total number: 177

good_systolic = length(systolic[systolic$vitals_systolic < 140 & systolic$vitals_systolic > 60,])/dim(systolic)[1] 
average_systolic = mean(systolic$vitals_systolic[systolic$vitals_systolic < 200 & systolic$vitals_systolic > 60])

diastolic[diastolic$vitals_diastolic == max(diastolic$vitals_diastolic),] # what is max? 622
length(diastolic[diastolic$vitals_diastolic > 150,]) # how many are over 150? 3
length(diastolic[diastolic$vitals_diastolic < 50,]) # how many are under 60? 29
dim(diastolic)[1] # total number: 118

good_diastolic = length(diastolic[diastolic$vitals_diastolic < 90 & diastolic$vitals_diastolic > 50,])/dim(diastolic)[1] 
average_diastolic = mean(diastolic$vitals_diastolic[diastolic$vitals_diastolic < 150 & diastolic$vitals_diastolic > 50])

####################################################################
#### Temperature distribution

temperature = sqlQuery(myconn, "select distinct vitals_temperature, vitals_temperature_units from t_vitals where vitals_orig_site_id = '6' order by vitals_temperature")
temperature = na.omit(temperature)
temperature$conversion = 0

# Convert from Farenheit to Celcius:
for(i in 1:dim(temperature)[1]) {
  
  unit = temperature$vitals_temperature_unit[i]
  
  if(unit == "C") {
    temperature$conversion[i] = (temperature$vitals_temperature[i] * 1.8) + 32
    # F = 9/5*C +32
  }
  
  else {
    temperature$conversion[i] = temperature$vitals_temperature[i]
  }
}

# Check C/F conversion:
head(temperature[temperature$vitals_temperature_unit == "C",])
dim(temperature[temperature$vitals_temperature_unit == "C",]) #6

# Some looks into the data:
temperature[temperature$vitals_temperature == max(temperature$vitals_temperature),] # what is max? 998.8 F
dim(temperature[temperature$conversion > 150,])[1] # how many are over 150F? 15
dim(temperature[temperature$conversion < 80,])[1] # how many are under 80F? 42
dim(temperature)[1] # total number: 260
good_temps = dim(temperature[temperature$conversion < 150 & temperature$conversion > 80,])[1]/dim(temperature)[1] # percent that are good

# Look at distribution of good temps:
temperature_temp = temperature[which(temperature$conversion < 150 & temperature$conversion > 80),]
average_temp = mean(temperature_temp$conversion)

m = ggplot(temperature_temp, aes(x=conversion))
m + geom_histogram(aes(fill = ..count..), binwidth = 1) + xlab("Temp (F)")
#hist(temperature_temp$conversion, breaks=40, main="Temperature in F", xlab="Temp (F)")


########################## VTIALS TABLE OUTPUT #########################

# good_heights:       0.8340249
# good_weights:       0.9631744
# good_BMI:           0.3450704
# good_systolic:      0.7514124
# good_diastolic:     0.7118644
# good_temps:         0.7769231

# average_height:     49.03683
# average_weight:     135.732
# average_BMI:        35
# average_systolic:   131.8496
# average_diastolic:  92.91667
# average_temp:       98.30832








