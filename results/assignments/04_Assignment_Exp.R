#-----------Q1

#No in comparison to other two upstream stations this one has a significant different 
#behavior which makes it an improper representing station. Some permanent changes during 
#the years between 1960 _1970 like building a dam might have changed its behavior forever.

#--------------Q2

library(data.table)
library(ggplot2)

precip_day <- readRDS('data/raw/precip_day.rds')
colset_4 <-  c("#D35C37", "#BF9A77", "#D6C6B9", "#97B8C2")

precip_day[, month := month(date)]
precip_day[month == 12 | month == 1 | month == 2, season := 'winter']
precip_day[month == 3 | month == 4 | month == 5, season := 'spring']
precip_day[month == 6 | month == 7 | month == 8, season := 'summer']
precip_day[month == 9 | month == 10 | month == 11, season := 'autumn']
precip_day[, season := factor(season, levels = c('winter', 'spring', 'summer', 'autumn'))]

precip_day[, year := year(date)]
precip_winter <- precip_day[season == 'winter', 
                            .(value = sum(value)), by = year]
precip_summer <- precip_day[season == 'summer', 
                            .(value = sum(value)), by = year]
year_thres <- 1990
to_plot <- rbind(cbind(precip_winter, season = factor('winter')), 
                 cbind(precip_summer, season = factor('summer'))) 

to_plot[year < year_thres, period := factor('pre_1990')]
to_plot[year >= year_thres, period := factor('aft_1990')]
to_plot[year < year_thres, period := factor('pre_1990')]
to_plot[year >= year_thres, period := factor('aft_1990')]
ggplot(to_plot[year >= 1960], aes(season, value, fill = period)) +
  geom_boxplot() +
  scale_fill_manual(values = colset_4[c(4, 1)]) +
  xlab(label = "Season") +
  ylab(label = "Precitation") +
  theme_bw()
ggplot(to_plot[season == 'summer' & year >= 1960], aes(x = year, y = value)) +
  geom_line(col = colset_4[3])+
  geom_point(col = colset_4[3])+
  geom_smooth(method = 'lm', formula = y~x, se = 0, col = colset_4[1]) +
  geom_smooth(method = 'loess', formula = y~x, se = 0, col = colset_4[4]) +
  scale_color_manual(values = colset_4[c(1, 2, 4)]) +
  xlab(label = "Year") +
  ylab(label = "Summer Precipitation") +
  theme_bw()

ggplot(to_plot[season == 'winter' & year >= 1960], aes(x = year, y = value)) +
  geom_line(col = colset_4[3])+
  geom_point(col = colset_4[3])+
  geom_smooth(method = 'lm', formula = y~x, se = 0, col = colset_4[1]) +
  geom_smooth(method = 'loess', formula = y~x, se = 0, col = colset_4[4]) +
  scale_color_manual(values = colset_4[c(1, 2, 4)]) +
  xlab(label = "Year") +
  ylab(label = "Winter Precipitation") +
  theme_bw()

#we do not have the precipitation data regarding each station;
#however, by comparing the two plots with the one in 04_classification 
#we consider the same figures also the same happened in boxplots with regard to Navigation questions. 


#--------------------------------Q3
#In my view there are drastic changes happening in Rhine climate and the main reason 
#is the changes in temperature. This means that the river source is getting more reliable 
#on rains rather than the melting snow from the sources. This increases the chance of flood 
#during the year and decreases the quality of soil. The result of flows in summer creates bad 
#consequences for agriculture and economy and long term plans should consider this 
#changes and be prepared for them.



#---------------------------------Q4
#in my view the evaporation must be considered in future measurements because it has a
#really important role in the volume of the water. The structure of our environment 
#is changing we must take this into consideration an create strategies that help us to 
#improve the river.  
