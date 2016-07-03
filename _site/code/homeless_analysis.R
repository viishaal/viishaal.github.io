
#install.packages("leaflet")
#library("leaflet")
require(leaflet)
require(plotly)
require(dplyr)
setwd("/Users/vishaljuneja/Desktop/EDAV/EDAV_Final/")


df = read.csv("311_Service_Requests_from_2010_to_Present-3.csv", strip.white=TRUE)
#df_manhattan = df[grep("manhattan", df$Borough, ignore.case = TRUE),]
dfh = df[grep("homeless", df$Complaint.Type, ignore.case = TRUE), ]


library(ggmap)
theme_set(theme_bw())
#qmplot(Longitude, Latitude, data=dfh[1:100,])


dfh$year = as.numeric(format(as.Date(dfh$Created.Date, format= "%m/%d/%Y"), "%Y"))
dfh$month = as.numeric(format(as.Date(dfh$Created.Date, format= "%m/%d/%Y"), "%m"))

qmplot(Longitude, Latitude, data=dfh, extent="panel") + facet_wrap(~year)

qmplot(Longitude, Latitude, data=dfh[dfh$year=="2015",], extent="panel") + facet_wrap(~month)


dfh2015 = dfh[dfh$year=="2015",]

cc = dfh2015[complete.cases(dfh2015), ]
leaflet(cc) %>% 
  addTiles() %>% 
  addMarkers(lng=~Longitude, lat=~Latitude, clusterOptions=markerClusterOptions(), popup="homeless")

dfh[dfh$year=="2013",] %>%
  group_by(month) %>%
  summarise(total=n())

year_wise = dfh %>%
            group_by(year) %>%
            summarise(total=n())

dfh2015jan = dfh2015[dfh2015$month == 1,]
dfh2015aug = dfh2015[dfh2015$month == 8,]

qmplot(Longitude, Latitude, data=dfh2015jan, geom = c("point","density2d"))
qmplot(Longitude, Latitude, data=dfh2015[dfh2015$month == 8,] , geom = c("point","density2d"))

monthwise = dfh2015 %>%
            group_by(month) %>%
            summarise(total=n())

g3 = ggplot(data=monthwise) + geom_bar(aes(x=reorder(month_name, month), y=total), stat="identity", fill="blue", alpha=0.5)
g3 = g3 + labs(title="Monthwise Complaints for 2015", x="Month", y="Complaints")
g3

g3 = ggplot(data=year_wise) + geom_bar(aes(x=year, y=total), stat="identity", fill="purple", alpha=0.5)
g3 = g3 + labs(title="Yearwise Complaints", x="Year", y="Complaints")
g3


dfh2015$date = as.Date(dfh2015$Created.Date, format= "%m/%d/%Y")
day_wise = dfh2015 %>%
            group_by(date) %>%
            summarise(total=n())

g3 = ggplot(data=day_wise) + geom_bar(aes(x=date, y=total), stat="identity", fill="blue", alpha=0.5)
g3 = g3 + labs(title="Daywise Complaints for 2015", x="Day", y="Complaints")
g3

day_wise[which.max(day_wise$total),]

g3 = ggplot(data=day_wise[352:365, ]) + geom_bar(aes(x=date, y=total), stat="identity", fill="blue", alpha=0.5)
g3 = g3 + labs(title="Daywise Complaints for 2015", x="Day", y="Complaints")
g3

threshold=0.6
win_length = 3
MAX = 999999
vals = c()
idx = c()
for (i in 1:nrow(day_wise)) {
  val = day_wise$total[i]
  #val_prev = day_wise$total[i-1]
  #val_next = day_wise$total[i+1]
#   
#   if ((val-val_prev)/val_prev > threshold && (val - val_next)/val_next > threshold) {
#     print(day_wise$date[i])
#   }
  
  if (i > 3 && i < 363 && 
      any( (val - day_wise$total[(i-win_length):(i-1)])/day_wise$total[(i-win_length):(i-1)] > threshold &
            (val -day_wise$total[(i+1):(i+win_length+1)])/day_wise$total[(i+1):(i+win_length+1)] > threshold ) ) {
    vals = c(vals,day_wise$date[i])
    idx = c(idx, i)
  }
}
class(vals) = "Date"

fest_2015 = c(1,19,47,145,185,250,285,315,330,359)

g3 = ggplot(data=day_wise) + geom_bar(aes(x=date, y=total), stat="identity", fill="grey", alpha=0.8)
g3 = g3 + geom_point(data=day_wise[fest_2015,], aes(date, total), stat="identity", colour="red", size=3,alpha=0.8)
g3 = g3 + geom_bar(data=day_wise[idx,], aes(date, total), stat="identity", fill="blue")
g3 = g3 + labs(title="Red dot holidays 2015", x="Day", y="No. of Complaints")
g3


## read weather data
wthr = read.csv("weather_data.csv")
wthr2015 = wthr[wthr$Year == 2015, ]
day_wise$temp = wthr2015$Mean.Temperature

cor(day_wise$temp, day_wise$total)

## ggplot of regression
ggplot(day_wise, aes(x=temp,y=total)) + geom_point(shape=1) + geom_smooth(method=lm) +labs(title="No of complaints vs Temperature", x="Mean Temperature", y="No. of Complaints")

wthr.lm = lm(total~temp, data=day_wise)
coeffs = coefficients(wthr.lm); coeffs
summary(wthr.lm)$r.squared

# Call:
#   lm(formula = total ~ temp, data = day_wise)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -25.311  -9.018  -2.018   7.334  74.337 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  0.94913    2.17995   0.435    0.664    
# temp         0.52956    0.03622  14.619   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 13.15 on 363 degrees of freedom
# Multiple R-squared:  0.3706,	Adjusted R-squared:  0.3688 
# F-statistic: 213.7 on 1 and 363 DF,  p-value: < 2.2e-16

# residuals
wthr.res = resid(wthr.lm)
plot(day_wise$temp, wthr.res, xlab="temperature", ylab="residuals", main="residuals plot")
abline(0,0)
