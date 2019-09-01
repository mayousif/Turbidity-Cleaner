# This script fills in data gaps using a predictive model

Turb <<- ReactiveTurbidityLVL3$Data[,c("DateTime","DataValue")]
Precip <<- PrecipData[,c("DateTime","DataValue")]
Level <<- LevelData

Turb[,"DataType"] = "Turbidity"
Precip[,"DataType"] = "Precip"
Level[,"DataType"] = "Level"

MergedData = rbind(Turb,Precip,Level)

df1 = dcast(MergedData, DateTime ~ DataType,mean, value.var = "DataValue")
df1[,2:ncol(df1)] = replace(df1[,2:ncol(df1)], df1[,2:ncol(df1)] == "NaN", NA) # repalce NaN with NA

# Replace all negative values with NA
df1[,2:ncol(df1)][df1[,2:ncol(df1)]<0] = NA

# Format date and time - needed to separate month data for seasonal comparison
df1$DateTime = as.POSIXct(df1$DateTime, "%m/%d/%Y %H:%M", tz="UTC")
df1$Time = as.factor(format(df1$DateTime, format = "%H:%M"))
df1$Day = as.factor(day(df1$DateTime))
df1$Month = as.factor(month(df1$DateTime))
df1$Year = as.factor(year(df1$DateTime))

# Separate data by seasons
df1Winter = df1[month(df1$DateTime) %in% c(12,1,2),]
df1Spring = df1[month(df1$DateTime) %in% c(3,4,5),]
df1Summer = df1[month(df1$DateTime) %in% c(6,7,8),]
df1Fall = df1[month(df1$DateTime) %in% c(9,10,11),]

# Predict data for each season
WinterImputedData = parlmice(df1Winter[,c("Time","Day","Month","Year","Turbidity","Precip","Level")], m=5, maxit= 50, method = 'cart', seed = 500)
SpringImputedData = parlmice(df1Spring[,c("Time","Day","Month","Year","Turbidity","Precip","Level")], m=5, maxit= 50, method = 'cart', seed = 500)
SummerImputedData = parlmice(df1Summer[,c("Time","Day","Month","Year","Turbidity","Precip","Level")], m=5, maxit= 50, method = 'cart', seed = 500)
FallImputedData = parlmice(df1Fall[,c("Time","Day","Month","Year","Turbidity","Precip","Level")], m=5, maxit= 50, method = 'cart', seed = 500)

completeWinterData = complete(WinterImputedData)
completeSpringData = complete(SpringImputedData)
completeSummerData = complete(SummerImputedData)
completeFallData = complete(FallImputedData)

completeWinterData = cbind(df1Winter$DateTime,completeWinterData)
completeSpringData = cbind(df1Spring$DateTime,completeSpringData)
completeSummerData = cbind(df1Summer$DateTime,completeSummerData)
completeFallData = cbind(df1Fall$DateTime,completeFallData)

colnames(completeWinterData)[1] = "DateTime"
colnames(completeSpringData)[1] = "DateTime"
colnames(completeSummerData)[1] = "DateTime"
colnames(completeFallData)[1] = "DateTime"

alldata = rbind(completeWinterData,completeSpringData,completeSummerData,completeFallData)
alldata = alldata[order(alldata$DateTime),]

df.mean = HoltWinters(alldata$Turbidity,
                       alpha = 0.05,
                       beta=FALSE,
                       gamma=FALSE)

df.pred = predict(df.mean, n.ahead=10)
dff = df.mean$fitted
dff = data.frame(dff)
dff = dff$xhat
dff[nrow(alldata)] = alldata$Turbidity[nrow(alldata)]




