Bangalore=read.csv("C:\\Users\\Ka Ri Sh Ma\\Desktop\\R\\archive (1)\\Temperature_And_Precipitation_Cities_IN\\Bangalore_1990_2022_BangaloreCity.csv")
Chennai=read.csv("C:\\Users\\Ka Ri Sh Ma\\Desktop\\R\\archive (1)\\Temperature_And_Precipitation_Cities_IN\\Chennai_1990_2022_Madras.csv")
Delhi=read.csv("C:\\Users\\Ka Ri Sh Ma\\Desktop\\R\\archive (1)\\Temperature_And_Precipitation_Cities_IN\\Delhi_NCR_1990_2022_Safdarjung.csv")
Lucknow=read.csv("C:\\Users\\Ka Ri Sh Ma\\Desktop\\R\\archive (1)\\Temperature_And_Precipitation_Cities_IN\\Lucknow_1990_2022.csv")
Mumbai=read.csv("C:\\Users\\Ka Ri Sh Ma\\Desktop\\R\\archive (1)\\Temperature_And_Precipitation_Cities_IN\\Mumbai_1990_2022_Santacruz.csv")
Rajasthan=read.csv("C:\\Users\\Ka Ri Sh Ma\\Desktop\\R\\archive (1)\\Temperature_And_Precipitation_Cities_IN\\Rajasthan_1990_2022_Jodhpur.csv")
Geo_location=read.csv("C:\\Users\\Ka Ri Sh Ma\\Desktop\\R\\archive (1)\\Temperature_And_Precipitation_Cities_IN\\Station_GeoLocation_Longitute_Latitude_Elevation_EPSG_4326.csv")
Bhuvaneshvar=read.csv("C:\\Users\\Ka Ri Sh Ma\\Desktop\\R\\archive (1)\\Temperature_And_Precipitation_Cities_IN\\weather_Bhubhneshwar_1990_2022.csv")
Rourkela=read.csv("C:\\Users\\Ka Ri Sh Ma\\Desktop\\R\\archive (1)\\Temperature_And_Precipitation_Cities_IN\\weather_Rourkela_2021_2022.csv")

library("tidyverse")
Bangalore["City"]="Bangalore"
Chennai["City"]="Chennai"
Delhi["City"]="Delhi"
Lucknow["City"]="Lucknow"
Mumbai["City"]="Mumbai"
Rajasthan["City"]="Rajasthan"
Bhuvaneshvar["City"]="Bhuvaneshvar"
Rourkela["City"]="Rourkela"


#Data Cleaning
result=bind_rows(Bangalore,Chennai,Delhi,Lucknow,Mumbai,Rajasthan,Bhuvaneshvar,Rourkela)
result1=select(result,c(1:6))
result1=tibble(result1)
class(result1)
result1=result1%>%
  fill(tavg,tmin,tmax,.direction=c("down"))
result1$prcp=replace_na(result1$prcp,mean(result1$prcp,na.rm=TRUE))

#Data Manipulation

#Hottest city 
hottest_years <- result1 %>%
  group_by(City) %>%
  filter(tmax == max(tmax, na.rm = TRUE)) %>%
  select(City, time, tmax)
print(hottest_years)

#coldest City

coldest_years=result1 %>%
  group_by(City) %>%
  filter(tmin == min(tmin, na.rm = TRUE)) %>%
  select(City, time, tmin)
  
print(coldest_years)

###________Abnormally high or low rainfall_______

#Precipitation

average_precipitation=mean(result1$prcp,na.rm=TRUE)

sd_precipitation=sd(result1$prcp,na.rm=TRUE)

#threshold
lower_threshold=average_precipitation-2*sd_precipitation

higher_threshold=average_precipitation+2*sd_precipitation

#Abnormal rainfall
abnormally_less=result1$time[result1$prcp<lower_threshold]
abnormally_more=result1$time[result1$prcp>higher_threshold]


#to check if there is abnormally less rainfall

if(length(abnormally_less)>0){
  print(abnormally_less)
}else{
  print("No Abnormally less rainfall found")
}#to check if there is abnormally more rainfall

if(length(abnormally_more)>0){
  print(abnormally_more)
}else{
  print("No abnormally more rainfall found")
}

library(ggplot2)

#ggplot for city with its highest temperature

hottest_years_plot=ggplot(hottest_years,aes(hottest_years$City,tmax,fill=time))+geom_col(color="red")+labs(y="Maximum temperature",x="City",title="Hottest temperature recorded in each city")+geom_text(aes(label=hottest_years$tmax),vjust=-0.5)
hottest_years_plot

coldest_years_plot=ggplot(coldest_years,aes(coldest_years$City,tmin,fill=time))+geom_col(color="red")+labs(y="Minimum temperature",x="City",title="coldest temperature recorded in each city")+geom_text(aes(label=coldest_years$tmin),vjust=-0.5)
coldest_years_plot

##ggplot for abnormal rainfall

abnormally_less_plot= ggplot(data = result1[result1$prcp < lower_threshold, ], aes(x = time, y = prcp)) +
  geom_point() +
  ggtitle("Abnormally Less Precipitation")


abnormally_more_plot=ggplot(data=result1[result1$prcp>higher_threshold,],aes(x=time,y=prcp))+geom_point()+ggtitle("Abnormally High")
abnormally_more_plot


