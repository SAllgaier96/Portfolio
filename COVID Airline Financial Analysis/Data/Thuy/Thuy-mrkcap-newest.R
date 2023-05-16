library(reshape)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(car)
library(lubridate)
library(gridExtra)

knitr::opts_chunk$set(echo = TRUE)
knitr::opts_knit$set(root.dir = '/Data/Thuy')


# import Airlines Data
southwest = read.csv("southwest.csv", head = TRUE, sep = ",")
delta = read.csv("delta.csv", head = TRUE, sep = ",")
united = read.csv("united.csv", head = TRUE, sep = ",")


# merge into one Dataset,remove N/A values
all_air <- rbind(southwest, united, delta)
head(all_air)
all_air <- na.omit(all_air)
all_air[all_air == 0] <- 0.001

# Add correct columns for merging later
all_air <- all_air %>%
            mutate(Airline = case_when(carrier == 'WN' ~ 'SouthWest',
                             carrier == 'DL' ~ 'Delta',
                             carrier == 'UA' ~ 'United')) %>% 
            dplyr::rename(Year = year, Month = month)

# Aggregate all airport data by Airline and Month
all_month = all_air %>% group_by(Airline,Year,Month) %>% 
  summarise(across(where(is.numeric), list(sum = sum)))

# convert all 0 to .0001 to avoid log issues later
all_month[all_month == 0] <- 0.001

# Read in Market_cap data
mrkcap = read.csv("Monthly Market Cap.csv", head = TRUE, sep = ",")
str(mrkcap)

# Convert date to separate Month and Year columns
mrkcap$Month = format(as.Date(mrkcap$Date, format="%m/%d/%Y"),"%m")
mrkcap$Year = format(as.Date(mrkcap$Date, format="%m/%d/%Y"),"%Y")

# Clean market cap data for integration and classify covid
mrkcap = mrkcap %>% dplyr::rename(Airline = Company)%>% 
  filter(Year>=2017)%>%
  mutate(Date = mdy(Date),
         covid = ifelse(Date < mdy("03/01/2020"), "0", "1"))
    
# clean data type for merging
mrkcap$Month <- as.integer(mrkcap$Month)
mrkcap$Year <- as.integer(mrkcap$Year)
mrkcap$MarketCap <- as.numeric(mrkcap$MarketCap)

#Combine data sets ## note that airline data was only available till August
comb <- merge(all_month, mrkcap ,by=c("Airline","Year","Month"))

comb = comb %>% arrange(Airline,Year,Month)

head(comb)

#Split by airlines
SW = comb %>% filter(Airline == "SouthWest")
DL = comb %>% filter(Airline == "Delta")
UA = comb %>% filter(Airline == "United")
  
flights = ggplot(data=comb, aes(x=Airline, y=arr_flights_sum, color = Airline))+
  geom_boxplot()+ labs( x = "Airlines", y = "Arrival_flights", title="Flights by Airline") 
flights

delays = ggplot(data=comb, aes(x=Airline, y=arr_delay_sum, color = Airline))+
  geom_boxplot()+ labs( x = "Airlines", y = "Total Delays", title="Delays by Airline") 
delays

delays_percent = ggplot(data=comb, aes(x=Airline, y=arr_flights_sum/arr_delay_sum, color = Airline))+
  geom_boxplot(outlier.shape = NA)+ labs( x = "Airlines", y = "Percent Delays", title="% Delays by Airline")+coord_cartesian(ylim = c(0, 1))
delays_percent

# Comparison between delays and Airlines
delay_type <- melt(comb, id.vars='Airline', measure.vars=c('carrier_ct_sum','weather_ct_sum', 'nas_ct_sum', "security_ct_sum", "late_aircraft_ct_sum"))
k <- ggplot(delay_type) + labs(title="Delays by Count", x = "Airlines", y = "Count")+
  geom_boxplot(aes(x=Airline, y=value, color=variable))

delay_time <- melt(comb, id.vars='Airline', measure.vars=c("carrier_delay_sum", "weather_delay_sum", "nas_delay_sum", "security_delay_sum", "late_aircraft_delay_sum"))
l <- ggplot(delay_time) + labs(title="Delays by Minute", x = "Airlines", y = "Minutes")+
  geom_boxplot(aes(x=Airline, y=value, color=variable))

grid.arrange(k,l, ncol=1)


#overview
total = lm(MarketCap~Airline+arr_flights_sum+carrier_ct_sum+weather_ct_sum+nas_ct_sum+security_ct_sum+late_aircraft_ct_sum+arr_cancelled_sum, data = comb)
summary(total)
par(mfrow = c(2,2))
plot(total)

vif(total)


total2 = lm(MarketCap~Airline+arr_flights_sum+arr_del15_sum+arr_cancelled_sum, data = comb)
summary(total2)
par(mfrow = c(2,2))
plot(total2)

vif(total2)

#Remove arr_flights_sum from models to due to milticolinarity
total3 = lm(MarketCap~Airline+carrier_ct_sum+weather_ct_sum+nas_ct_sum+security_ct_sum+late_aircraft_ct_sum+arr_cancelled_sum, data = comb)
summary(total3)
par(mfrow = c(2,2))
plot(total3)
vif(total3)

total4 = lm(MarketCap~Airline+arr_del15_sum+arr_cancelled_sum, data = comb)
summary(total4)
par(mfrow = c(2,2))
plot(total4)

vif(total4)


#SW overview
SW_model_overview = lm(MarketCap~arr_flights_sum+arr_del15_sum+arr_cancelled_sum, data= SW)
summary(SW_model_overview)
par(mfrow = c(2,2))
plot(SW_model_overview)
vif(SW_model_overview)


#SW remove miltico
SW_model_overview2 = lm(MarketCap~arr_flights_sum+arr_cancelled_sum, data= SW)
summary(SW_model_overview2)
par(mfrow = c(2,2))
plot(SW_model_overview2)
vif(SW_model_overview2)



# #SW by delay count
# SW_model = lm(MarketCap~arr_flights_sum+carrier_ct_sum+weather_ct_sum+nas_ct_sum+security_ct_sum+late_aircraft_ct_sum+arr_cancelled_sum, data= SW)
# summary(SW_model)
# par(mfrow = c(2,2))
# plot(SW_model)

#DL overview
DL_model_overview = lm(MarketCap~arr_flights_sum+arr_del15_sum+arr_cancelled_sum, data= DL)
summary(DL_model_overview)
par(mfrow = c(2,2))
plot(DL_model_overview)
vif(DL_model_overview)


#DL remove miltico
DL_model_overview2 = lm(MarketCap~arr_flights_sum+arr_cancelled_sum, data= DL)
summary(DL_model_overview2)
par(mfrow = c(2,2))
plot(DL_model_overview2)
vif(DL_model_overview2)


# #DL by delay count
# DL_model = lm(MarketCap~arr_flights_sum+carrier_ct_sum+weather_ct_sum+nas_ct_sum+security_ct_sum+late_aircraft_ct_sum+arr_cancelled_sum, data= DL)
# summary(DL_model)
# par(mfrow = c(2,2))
# plot(DL_model)

#UA overview
UA_model_overview = lm(MarketCap~arr_flights_sum+arr_del15_sum+arr_cancelled_sum, data= UA)
summary(UA_model_overview)
par(mfrow = c(2,2))
plot(UA_model_overview)
vif(UA_model_overview)

#UA remove miltico
UA_model_overview2 = lm(MarketCap~arr_flights_sum+arr_cancelled_sum, data= UA)
summary(UA_model_overview2)
par(mfrow = c(2,2))
plot(UA_model_overview2)
vif(UA_model_overview2)

# #UA by delay count
# UA_model = lm(MarketCap~arr_flights_sum+carrier_ct_sum+weather_ct_sum+nas_ct_sum+security_ct_sum+late_aircraft_ct_sum+arr_cancelled_sum, data= UA)
# summary(UA_model)
# par(mfrow = c(2,2))
# plot(UA_model)


#COVID
ggplot(comb) + 
  geom_line(aes(x = Date, y = MarketCap, color = Airline))+ 
  scale_x_date(date_labels="%b %y",date_breaks  ="3 month")

#Covid Impact
Covid_model = lm(MarketCap~arr_flights_sum+Airline+covid+arr_del15_sum, comb)
summary(Covid_model)
par(mfrow = c(2,2))
plot(Covid_model)


#Plot Covid Effects
arr_flights = ggplot(data=comb, aes(x=Airline, y=arr_flights_sum, color=covid))+
  geom_boxplot()+ labs( x = "Airlines", y = "Arrival_flights") +
  scale_color_manual(labels = c("pre-covid onset", "post_covid onset"), values = c("blue", "red"))

arr_cancelled = ggplot(data=comb, aes(x=Airline, y=arr_cancelled_sum, color=covid))+
  geom_boxplot()+scale_y_log10()+labs( x = "Airlines", y = "Canceled_flights") +
  scale_color_manual(labels = c("pre-covid onset", "post_covid onset"), values = c("blue", "red"))

arr_delay = ggplot(data=comb, aes(x=Airline, y=arr_del15_sum, color=covid))+
  geom_boxplot() +labs( x = "Airlines", y = "Arrival_delays") +
  scale_color_manual(labels = c("pre-covid onset", "post_covid onset"), values = c("blue", "red"))

mkval = ggplot(data=comb, aes(x=Airline, y=MarketCap, color=covid))+
  geom_boxplot() +labs( x = "Airlines", y = "Market Cap") +
  scale_color_manual(labels = c("pre-covid onset", "post_covid onset"), values = c("blue", "red"))

grid.arrange(arr_flights,  arr_delay, arr_cancelled, mkval, ncol=2)



#Airlines Covid Model: 
SW_model_covid = lm(MarketCap~arr_flights_sum+arr_cancelled_sum+covid, data= SW)
summary(SW_model_covid)
par(mfrow = c(2,2))
plot(SW_model_covid)

DL_model_covid = lm(MarketCap~arr_flights_sum+arr_cancelled_sum+covid, data= DL)
summary(DL_model_covid)
par(mfrow = c(2,2))
plot(DL_model_covid)

UA_model_covid = lm(MarketCap~arr_flights_sum+arr_cancelled_sum+covid, data= UA)
summary(UA_model_covid)
par(mfrow = c(2,2))
plot(UA_model_covid)
