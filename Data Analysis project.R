# Tan Kok Zhen


# Import Data
hourlyWD = read.csv("C:/Users/terra/Desktop/Hourly weather data.csv")

# Data structure 
class(hourlyWD)
dim(hourlyWD)
names(hourlyWD)
str(hourlyWD)
summary (hourlyWD)
nrow(hourlyWD)
ncol(hourlyWD)

# To view data
View (hourlyWD)

# Required packages
install.packages("ggplot2")
install.packages("dplyr")
install.packages("viridis")
install.packages("ggridges")
install.packages("hrbrthemes")

#load the packages 
library(ggplot2)
library(dplyr)
library(viridis)
library(ggridges)
library(hrbrthemes)

##### Data pre-processing #####

# missing data column with value
sapply(hourlyWD, function(hourlyWD) sum(is.na(hourlyWD)))
# sum of all missing value
sum(is.na(hourlyWD))

#Filling missing data in wind_gust and pressure by Month
windgustmean = hourlyWD %>% group_by(month)%>% summarise(Mean = mean(wind_gust, na.rm = TRUE))
pressuremean= hourlyWD %>% group_by(month)%>% summarise(Mean = mean(pressure, na.rm = TRUE))
testD = merge(hourlyWD, windgustmean, by = "month")
testD1 = merge(hourlyWD, pressuremean, by = "month")
# add the mean value column into original"hourlyWD" dataset 
hourlyWD$wind_gust[is.na(hourlyWD$wind_gust)] = testD$Mean[is.na(hourlyWD$wind_gust)]
hourlyWD$pressure[is.na(hourlyWD$pressure)] = testD1$Mean[is.na(hourlyWD$pressure)]
# use to check whether it still having remaining null value 
sapply(hourlyWD, function(hourlyWD) sum(is.na(hourlyWD)))
# remove the duplicate dataset that has taken the mean and replace into "hourlyWD" NA value
remove(testD)
remove(testD1)


# To remove remaining NA value by inserting mean values (wind_dir &wind_speed)
for (i in which(sapply(hourlyWD, is.numeric))) { 
  hourlyWD[is.na(hourlyWD[, i]), i] = mean(hourlyWD[, i],  na.rm = TRUE) 
}
# use to check whether it still having remaining null value in that column
sapply(hourlyWD, function(hourlyWD) sum(is.na(hourlyWD)))

# Check outlier
cleantemperature= hourlyWD%>%filter(month==5, day==8,origin =="JFK")
# To delete the row that are outlier
df = cleantemperature[-23,]
#To calculate the mean of temperature in df
mean(df$temp)
#check which row number that are outlier in main datasets
which(hourlyWD$temp==13.1)
#replace mean
hourlyWD[3065,6]=mean(df$temp)
remove(cleantemperature)
remove(df)


# Example of Analysis 1 
#Relationship between average humidity and month in 2013
humiditymeanmonth <- hourlyWD %>% group_by(origin,month) %>% summarise(mean_humidity = mean(humid, na.rm = TRUE))

ggplot(humiditymeanmonth,aes(x=month,y=mean_humidity))+
  geom_col(color= "black", fill="red")+
  facet_grid(.~origin)+
  scale_x_continuous(breaks=c(1:12))+
  ylab("Average of humidity (RH) ")+ xlab("Month")
  ggtitle("Average humidity each Month in 2013")+theme_light()


# Example of Analysis 2
# Density plot of temperature for both airports based on the month
hourlyWD %>% ggplot( aes(x=temp, fill=origin)) +
  geom_density( color="#e9ecef", alpha=0.5) +
  scale_fill_manual(values=c("#41d0e0", "#f2bf85")) +
  facet_wrap(~month)+
  xlab("Temperature(°F)")+ggtitle("Density plot of temperature for both ariports in each month in 2013")


# Example of Analysis 3 
# Relationship between Temperature and Dew point 
ggplot(hourlyWD,aes(temp, dewp))+geom_point(alpha =0.3, color ="Blue") +
  geom_smooth(method ="lm",color ="red", se=FALSE)+
  ggtitle("Relationship between Temperature and Dew point")+ylab("Dew point") + xlab("Temperature(°F)")

# Example of Analysis 4
# Relationship between Average Temperature and Month in 2013
tempmonth <- hourlyWD %>% group_by(origin,month) %>% summarise(mean_temperature = mean(temp, na.rm = TRUE))
ggplot(tempmonth, aes (fill =origin, x =month, y = mean_temperature)) +
  geom_bar (position = "dodge", stat = "identity", width= 1) + scale_x_continuous(name="Month", breaks = 1 :12) +
  ylab("Average Temperature") + ggtitle("Relationship between Average Temperature and Month for both origin in 2013")+
  scale_fill_manual( "origin", values = c("JFK" = "black", "LGA" = "red"))+ theme_classic()

#Example of Analysis 5
# Relationship between pressure and wind speed
ggplot(hourlyWD, aes(wind_speed, pressure))+ geom_jitter(width = .5, size=1, color = "lightblue") +
  geom_smooth(method ="lm",color ="red", se=FALSE)+
  labs(y="pressure",x="wind_speed", 
       title="Relationship between pressure and wind speed")

# Example of Analysis 6
# Relationship between humidity and visibility
ggplot(hourlyWD, aes(x=humid, y=visib)) + geom_point(colour = "red")+ 
  xlab("Humidity (RH)") +
  ylab("Visibility")+
  facet_wrap(~origin)+
  ggtitle("Relationship between humidity and visibility")

# Example of Analysis 7
# Relationship of precipitation and humidity that affects visibility.
ggplot(hourlyWD, aes(x = precip, y = humid, group = visib, colour = visib))+
  geom_point()+
  facet_wrap(.~origin)+
  xlab("Precipitation") +
  ylab("Humidity (RH)") +
  ggtitle("Relationship of precipitation and humidity that affects visibility.")+
  scale_color_viridis( option = "B")

# Example of analysis 8
# Relationship between windspeed and windgust for both airport in 2013
ggplot(hourlyWD, aes(x = wind_speed, y = wind_gust, colour = origin))+
  geom_point()+
  geom_smooth(method="lm",color ="red", se=FALSE)+
  facet_wrap(.~origin)+
  xlab("Wind Speed") +
  ylab("Wind Gust") +
  ggtitle("Relationship between windspeed and windgust for both airport in 2013")+
  scale_color_manual(values = c("orange","lightblue"))

# Example of analysis 9
# Study the average wind speed based on hour 
WShour <- hourlyWD %>% group_by(origin,hour) %>% summarise(avgWS =mean(wind_speed, na.rm =TRUE))
ggplot(WShour,aes(x=hour , y= avgWS, color =origin))+
  geom_segment(aes(x=hour, xend=hour, y=5, yend=avgWS), color="black", size= 1, linetype="dotted") +
  geom_point(alpha=0.6,size=5)+
  scale_x_continuous(breaks=(0:23))+
  ylim(5,15)+
  theme_classic() +
  xlab("Hour") +
  ylab("average WS") + ggtitle(" Average wind speed based on hour ")


#Example of analysis 10
#Average wind direction by hour in both origin
WDhour <- hourlyWD %>% group_by(origin,hour) %>% summarise(avgWD =mean(wind_dir, na.rm =TRUE))
ggplot(WDhour,aes(x=hour , y= avgWD, color =origin))+
  geom_segment(aes(x=hour, xend=hour, y=150, yend=avgWD), color="black", size= 1, linetype="dotted") +
  geom_point(alpha=0.6,size=5)+
  scale_x_continuous(breaks=(0:23))+
  ylim(150,220)+
  theme_classic() +
  xlab("Hour") +
  ylab("average Wind direction (degrees °)") + ggtitle(" Average wind direction based on hour ")

# Additional features 1
ggplot(WDhour,aes(x = hour, y = 0,fill =avgWD , angle = avgWD, radius =0.5))+
  geom_raster() +
  geom_spoke(arrow = arrow(length = unit(.05, 'inches'))) + 
  scale_fill_distiller(palette = "RdYlGn") + 
  coord_equal(expand = 0) + 
  theme(legend.position = 'bottom', 
        legend.direction = 'horizontal') +
  scale_x_continuous(breaks=(0:23))+scale_y_continuous(breaks=seq(-1,1,2),labels= c("",""))+ ylab("")+
  ggtitle("Average wind direction based on hour")

#Example of analysis 11
#Average Precipitation by month for both origin in year 2013
precipmonth <- hourlyWD %>% group_by(origin,month) %>% summarise(avgP =mean(precip, na.rm =TRUE))
ggplot(precipmonth,aes(x=month , y= avgP, color =origin))+
  geom_line() + geom_point()+
  scale_x_continuous(breaks=(1:12))+
  theme_classic() +
  xlab("Month") +
  ylab("Average precipitation (inches)") +
  ylim(0.000,0.015)+ggtitle("Average Precipitation based on month for both origin in year 2013 ")


#Example of analysis 12
#Study on ideal dewpoint based on average dewpoint in each month for both origin in year 2013
dewpointdata <- group_by(hourlyWD, origin, month) %>% summarise (avgDP = mean (dewp, na.rm = TRUE))
ggplot(dewpointdata,aes(x= month, y=avgDP, colour= origin))+
  geom_point()+geom_line()+
  scale_x_continuous(breaks=(1:12))+
  xlab("Month") +
  ylab("Average Dewpoint (°F)") + ggtitle("Ideal dewpoint based on average dewpoint in each month for both origin.")+
  theme_classic()+annotate("rect",xmin=0, xmax = 12, ymin =60, ymax= 65, alpha= 0.1, color="purple")


#Example of analysis 13
#Study the Non-ideal Windspeed based on average windspeed in each month for both origin 
windspeeddata <- group_by(hourlyWD, origin, month) %>% summarise (avgWS = mean (wind_speed, na.rm = TRUE))
ggplot(hourlyWD,aes(x= month, y=wind_speed, colour= origin))+
  geom_point()+
  scale_x_continuous(breaks=(1:12))+
  xlab("Month") +
  ylab("Average Wind Speed (mph)") + ggtitle("The Non-ideal Windspeed based on average windspeed in each month for both origin.")+
  theme_bw()+annotate("rect",xmin=0, xmax = 12, ymin = 34, ymax= 40, alpha= 0.1, color="purple",fill="orange")


# Example of Analysis 14
# Relationship between average pressure and month 
pressuremonth <- hourlyWD %>% group_by(origin,month) %>% summarise(mean_pressure = mean(pressure, na.rm = TRUE))
# Plot #c(0(down), .1(up))use percentage
ggplot(pressuremonth,aes(x=month, y=mean_pressure-1012,color=origin)) +
  geom_segment(aes(x=month, xend=month, y=0, yend=mean_pressure-1012), color="black", size= 1, linetype= "dotted") +
  geom_point(alpha=0.6,size=5) + scale_x_continuous(breaks=(1:12))+
  scale_y_continuous(expand = expansion(mult = c(0, .1)),breaks=seq(0,10,2),labels= c(1012,1014,1016,1018,1020,1022))+
  theme_classic()+scale_color_manual(values = c("red","blue"))+
  xlab("Month") +
  ylab("pressure") + ggtitle("Relationship between Average Pressure and Month")


# Example of Analysis 15
# Study in relationship between average temperature and average humidity in each Month

humidtempmonth <- hourlyWD %>% group_by(month) %>% summarise(temp_mean = mean(temp, na.rm = TRUE),
                                                             humid_mean = mean(humid, na.rm = TRUE))
ggplot(humidtempmonth) + 
  geom_col(aes(x = month, y = temp_mean), size = 1, color = "black", fill = "skyblue") +
  geom_line(aes(x = month, y = humid_mean), size = 1, color="purple") + 
  geom_point(aes(x = month, y = humid_mean), size = 1, color="red")+
  scale_x_continuous(breaks=(1:12))+
  scale_y_continuous(name="Average temperature",sec.axis = sec_axis(~./1, name = "Average humidity"))+
  ggtitle("Average temperature and humidity in each Month")+
  theme_classic()


# Example of Analysis 16
# Additional features 2
#Distribution of temperature by month
#violin plot 
ggplot(hourlyWD, aes(y=temp, x=month)) + 
  geom_violin(aes(group=cut_width(month,1)),scale="width",alpha=2,fill="orange") +
  ylab("Temperature") +xlab("month")+scale_x_continuous(breaks=(1:12))+
  ggtitle("Distribution of temperature on each Month")


# Example of Analysis 17
# Additional features 3
# Distribution of wind speed by month
# Ridgeline plot

# Changing variable month from numerical to categorical data
datanew=hourlyWD   

for(a in 1:12){ 
  val=switch(a, "January","February","March","April","May","June","July","August",
             "September","October","November","December")
  datanew[which(datanew$month==a),3]=val
}

ggplot(datanew, aes(x = wind_speed, y = month, fill = ..x..)) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
  scale_fill_viridis(name = "windspeed", option = "C") +
  labs(title = "Distribution of wind speed by month") +
  theme_ipsum()+
  theme(
    legend.position="right",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8))+
  scale_y_discrete (limits= c("December","November","October","September","August","July",
                              "June","May","April","March","February","January"))+
  xlim(0,45)
