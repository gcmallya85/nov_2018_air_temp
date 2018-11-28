# Load libraries
library(readr)
library(lubridate)
library(dplyr)
library(ggplot2)

# Load data
air_temp <- read_csv("acre_air_temp.csv") # base file reading function may also be used

# convert the char field to date field
air_temp[[1]] <- as.Date(air_temp[[1]],"%m/%d/%Y")

# %>% is the pipe function (similar to Unix) available through dplyr library
# Filter all observations belonging to november 
x <- air_temp %>% 
  select(Date,Air_temp) %>% 
  filter(month(Date)==11) %>%
  mutate(doy = day(Date)) %>%
  group_by(doy) %>%
  summarize(mean_air_temp = mean(Air_temp))

# Filter all observations belonging to november 2017
x17 <- air_temp %>% 
  select(Date,Air_temp) %>% 
  filter(year(Date)==2017) %>%  
  filter(month(Date)==11) %>%
  mutate(doy = day(Date))

# Filter all observations belonging to november 2018
x18 <- air_temp %>% 
  select(Date,Air_temp) %>% 
  filter(year(Date)==2018) %>%  
  filter(month(Date)==11) %>%
  mutate(doy = day(Date))

# Plot time-series data and save in desired format.
# tiff("image.tif", res=600, compression = "lzw", height=5, width=8, units="in")
png("image.png", width = 8, height = 5, units = 'in', res = 600)
theme_set(theme_bw())
gg1 <- ggplot(x, aes(x=doy)) 
gg2 <- gg1 +  geom_line(aes(y=mean_air_temp, col="Mean 2002-18")) + 
  geom_point(aes(y=mean_air_temp, col="Mean 2002-18")) + 
  geom_point(data=x17,aes(y=Air_temp, col="Daily 2017")) +
  geom_line(data=x17, aes(y=Air_temp, col="Daily 2017")) +
  geom_point(data=x18,aes(y=Air_temp, col="Daily 2018")) +
  geom_line(data=x18, aes(y=Air_temp, col="Daily 2018")) +
  labs(title="November Air Temperature", 
       subtitle="Station: ACRE, West Lafayette, IN", 
       caption="Data Source: iclimate.org", y="Air temperature (F)", x="Days") +  # title and caption
  scale_x_continuous(breaks = seq(1,30,2)) +  # change to monthly ticks and labels
  scale_color_manual(name="", 
                     values = c("Mean 2002-18"="#00ba38", "Daily 2017"="#f8766d", "Daily 2018"="#0072B2")) +  # line color
  theme(panel.grid.minor = element_blank())  # turn off minor grid
gg2
dev.off()