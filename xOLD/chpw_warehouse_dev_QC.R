
# Looking at the data
library(RODBC)
myconn <- odbcConnect("CHPW_Warehouse_Dev")

########################## VTIALS TABLE #########################

# Height distribution (NewHP only)

height = sqlQuery(myconn, "select distinct vitals_height, vitals_height_unit from t_vitals where vitals_orig_site_id = '6' order by vitals_height")
height = na.omit(height)
height$conversion = 0

for(i in 1:dim(height)[1]) {
  
  unit = height$vitals_height_unit[i]
  
  if(unit == "cm") {
    
    height$conversion[i] = height$vitals_height[i] / 2.5
    
  }
  
  else {
    
    height$conversion[i] = height$vitals_height[i]
    
  }
  
}

head(height[height$vitals_height_unit == "cm",])
hist(height$conversion, breaks=40)

height[height$conversion == max(height$conversion),] # what is max? 768 in
dim(height[height$conversion < 100,])[1] # how many are under 100 inches? 619
dim(height)[1] # total number: 723

height_temp = height[which(height$conversion < 100),] #100in as max
hist(height_temp$conversion, breaks=40, main="Height in Inches", xlab="Height (in)")


# Weight distribution (NewHP Only)

weight = sqlQuery(myconn, "select distinct vitals_weight, vitals_weight_unit from t_vitals where vitals_orig_site_id = '6' order by vitals_weight")
weight = na.omit(weight)

for(i in 1:dim(weight)[1]) {
  
  unit = weight$vitals_weight_unit[i]
  
  if(unit == "kg") {
    
    weight$conversion[i] = weight$vitals_weight[i] * 2.2
    
  }
  
  else {
    
    weight$conversion[i] = weight$vitals_weight[i]
    
  }
  
}

head(weight[weight$vitals_weight_unit == "kg",])
hist(weight$conversion, breaks=40)

weight[weight$conversion == max(weight$conversion),] # what is max? 901126 lb
dim(weight[weight$conversion < 400,])[1] # how many are under 400 lbs? 5046
dim(weight)[1] # total number: 5435

weight_temp = weight[which(weight$conversion < 400),] #400lb as max
hist(weight_temp$conversion, breaks=40, main="Weight in Lbs", xlab="Weight (lbs)")

# BMI distribution

BMI = sqlQuery(myconn, "select distinct vitals_BMI from t_vitals where vitals_orig_site_id = '6' order by vitals_BMI")
BMI = na.omit(BMI)
hist(BMI$vitals_BMI)

BMI[BMI$vitals_BMI == max(BMI$vitals_BMI),] # what is max? 286852
length(BMI[BMI$vitals_BMI < 400,]) # how many are under 400? 116
dim(BMI)[1] # total number: 142

BMI_temp = BMI[which(BMI$vitals_BMI < 400),] #400lb as max
hist(BMI_temp, breaks=40, main="Weight in Lbs", xlab="BMI")

#### Temperature distribution

temperature = sqlQuery(myconn, "select distinct vitals_temperature, vitals_temperature_units from t_vitals where vitals_orig_site_id = '6' order by vitals_temperature")
temperature = na.omit(temperature)
temperature$conversion = 0

# Convert from Farenheit to Celcius:
for(i in 1:dim(temperature)[1]) {
  
  unit = weight$vitals_weight_unit[i]
  
  if(unit == "C") {
    
    temperature$conversion[i] = (temperature$vitals_temperature[i] * 1.8) + 32
    # F = 9/5*C +32
    
  }
  
  else {
    
    temperature$conversion[i] = temperature$vitals_temperature[i]
    
  }
  
}

temperature[temperature$vitals_temperature == max(temperature$vitals_temperature),] # what is max? 998.8 F
dim(temperature[temperature$conversion < 200,])[1] # how many are under 200F? 245
dim(temperature[temperature$conversion > 80,])[1] 
dim(temperature)[1] # total number: 260

temperature_temp = temperature[which(temperature$conversion < 200),]
temperature_temp = temperature_temp[which(temperature_temp$conversion > 80),] #200 F as max
hist(temperature_temp$conversion, breaks=40, main="Temperature in F", xlab="Temp (F)")


# Heart Rate distribution

hr = sqlQuery(myconn, "select distinct vitals_heart_rate from t_vitals where vitals_orig_site_id = '6' order by vitals_heart_rate")
hr = na.omit(hr)

hr[hr$vitals_heart_rate == max(hr$vitals_heart_rate),] # what is max? 12597 
length(hr[hr$vitals_heart_rate < 300,]) # how many are under 300 bmp? 169
length(hr[hr$vitals_heart_rate > 30,])
dim(hr)[1] # total number: 297

hr_temp = hr[which(hr < 300),]
hr_temp = hr_temp[hr_temp> 30] #200 F as max
hist(hr_temp, breaks=40, main="Heart Rate in BMP", xlab="Heart Rate (F)")












