# R-Script below plots long-term annual and november temperatures (along with trend) 
# Data prepocessed using Matlab - see process_cli_div_data.m
# Author: Ganesh Mallya (gmallya.com)
# Date: 11/28/2018

# Load libraries
library(readr)
library(lubridate)
library(dplyr)
library(ggplot2)
library(trend)

# Load data
air_temp <- read_csv("long_term_air_temp_clidiv.txt") # base file reading function may also be used

############################################################################
# %>% is the pipe function (similar to Unix) available through dplyr library
# Calculate annual average temperature 
x <- air_temp %>% 
  select(Year,Month,AvgTempDegF) %>% 
  group_by(Year) %>%
  summarize(annual_air_temp = mean(AvgTempDegF))

# calculate 30-year avg annual temperature
x30yr <- x %>% 
  select(Year,annual_air_temp) %>% 
  filter(Year>1970 & Year<2001 ) %>%
  summarize(airTemp_30yravg = mean(annual_air_temp))

x30yrPlt <- data.frame(Year=double(),
                       val=double(), 
                 stringsAsFactors=FALSE) 
x30yrPlt[nrow(x30yrPlt)+1, ] = c(1895,x30yr$airTemp_30yravg)
x30yrPlt[nrow(x30yrPlt)+1, ] = c(2018,x30yr$airTemp_30yravg)

#####################################################
# Perform Mann-Kendall Test and Calculate Sen's slope
res_mk = mk.test(x$annual_air_temp)
res_sen = sens.slope(x$annual_air_temp)
# Calculate intercept values for data pairs using Sen's slope estimate
intercept = x$annual_air_temp - res_sen$estimates*x$Year # This is nothing but c = y - mx
med_intercept = median(intercept) # Calculate median of intercept values
slope_y = res_sen$estimates*x$Year + med_intercept

xTrend <- data.frame(x$Year,slope_y)

#####################################################
# # Filter all observations belonging to november 2018
# x18 <- air_temp %>% 
#   select(Date,Air_temp) %>% 
#   filter(year(Date)==2018) %>%  
#   filter(month(Date)==11) %>%
#   mutate(doy = day(Date))

#####################################################
# Plot time-series of annual avg air temperature and save in desired format.
# tiff("image.tif", res=600, compression = "lzw", height=5, width=8, units="in")
png("long_term_image.png", width = 8, height = 5, units = 'in', res = 600)
theme_set(theme_bw())
gg1 <- ggplot(x, aes(x=Year)) 
gg2 <- gg1 +  geom_line(aes(y=annual_air_temp, col="Annual avg.")) + 
  geom_point(aes(y=annual_air_temp, col="Annual avg.")) +
  geom_line(data=x30yrPlt, linetype=2, aes(y=val, col="30-year (1971-2000) avg."))+
  geom_line(data=xTrend, linetype=1, size=1.5, aes(x=x.Year,y=slope_y, col="Trend"))+ 
  annotate("text",x=1926,y=55.5,label="Sen's slope = 0.01 °F/year") +
  labs(title="Annual Avg. Air Temperature", 
       subtitle="Climate Div. 4 Indiana", 
       caption="Data Source: NOAA-NCDC", y="Air temperature (F)", x="Years") +  # title and caption
  scale_x_continuous(breaks = seq(1895,2018,20)) +  
  scale_color_manual(name="", 
                     values = c("Annual avg."="#00ba38", "30-year (1971-2000) avg."="#f8766d", "Trend"="#999999")) +  # line color
  theme(panel.grid.minor = element_blank())  # turn off minor grid
gg2
dev.off()



############################################################################
# %>% is the pipe function (similar to Unix) available through dplyr library
# Calculate annual average temperature 
xNov <- air_temp %>% 
  select(Year,Month,AvgTempDegF) %>% 
  filter(Month==11 & Year<2018)

# calculate 30-year avg annual temperature
x30yrNov <- xNov %>% 
  select(Year,AvgTempDegF) %>% 
  filter(Year>1970 & Year<2001 ) %>%
  summarize(NovairTemp_30yravg = mean(AvgTempDegF))

x30yrNovPlt <- data.frame(Year=double(),
                       val=double(), 
                       stringsAsFactors=FALSE) 
x30yrNovPlt[nrow(x30yrNovPlt)+1, ] = c(1895,x30yrNov$NovairTemp_30yravg)
x30yrNovPlt[nrow(x30yrNovPlt)+1, ] = c(2017,x30yrNov$NovairTemp_30yravg)

#####################################################
# Perform Mann-Kendall Test and Calculate Sen's slope
res_mk_Nov = mk.test(xNov$AvgTempDegF)
res_sen_Nov = sens.slope(xNov$AvgTempDegF)
# Calculate intercept values for data pairs using Sen's slope estimate
intercept_Nov = xNov$AvgTempDegF - res_sen_Nov$estimates*xNov$Year # This is nothing but c = y - mx
med_intercept_Nov = median(intercept_Nov) # Calculate median of intercept values
slope_y_Nov = res_sen_Nov$estimates*xNov$Year + med_intercept_Nov

xTrend_Nov <- data.frame(xNov$Year,slope_y_Nov)

#####################################################
# Plot time-series of November avg air temperature and save in desired format.
# tiff("image.tif", res=600, compression = "lzw", height=5, width=8, units="in")
png("long_term_Nov_image.png", width = 8, height = 5, units = 'in', res = 600)
theme_set(theme_bw())
gg1 <- ggplot(xNov, aes(x=Year)) 
gg2 <- gg1 +  geom_line(aes(y=AvgTempDegF, col="Nov. avg.")) + 
  geom_point(aes(y=AvgTempDegF, col="Nov. avg.")) +
  geom_line(data=x30yrNovPlt, linetype=2, aes(y=val, col="30-year (1971-2000) avg."))+
  geom_line(data=xTrend_Nov, linetype=1, size=1.5, aes(x=xNov.Year,y=slope_y_Nov, col="Trend"))+ 
  annotate("text",x=1926,y=50,label="Sen's slope = 0.027 °F/year") +
  labs(title="Nov. Avg. Air Temperature", 
       subtitle="Climate Div. 4 Indiana", 
       caption="Data Source: NOAA-NCDC", y="Air temperature (F)", x="Years") +  # title and caption
  scale_x_continuous(breaks = seq(1895,2017,20)) +  
  scale_color_manual(name="", 
                     values = c("Nov. avg."="#00ba38", "30-year (1971-2000) avg."="#f8766d", "Trend"="#999999")) +  # line color
  theme(panel.grid.minor = element_blank())  # turn off minor grid
gg2
dev.off()
