library(data.table)
library(ggplot2)
runoff_day <- readRDS('data/runoff_day.rds')
runoff_summer <- readRDS('data/runoff_summer.rds')
runoff_year_key <- readRDS('data/runoff_year_key.rds')
runoff_month_key <- readRDS('data/runoff_month_key.rds')
runoff_summary <- readRDS('data/runoff_summary.rds')
runoff_winter <- readRDS('data/runoff_winter.rds')

colset_4 <-  c("#D35C37", "#BF9A77", "#D6C6B9", "#97B8C2")

key_stations <- c('DOMA', 'BASR', 'KOEL')

#---------------------------------------------------------Q1

runoff_month_key[year <= 2000, age_range := factor('beforeyear_2000')]
runoff_month_key[year > 2000, age_range := factor('afteryear_2000')]
runoff_month_key
ggplot(runoff_month_key, aes(factor(month), value, fill = age_range)) +
  geom_boxplot() +
  facet_wrap(~sname, scales = 'free_y') +
  scale_fill_manual(values = colset_4[c(4, 1)]) +
  xlab(label = "Month") +
  ylab(label = "Runoff (m3/s)") +
  theme_bw()
#with monthly distinction we can see that in upstream stations during the last summer
#months the runoffs decrease the changes in downstream areas are less considerable.  

####yearly data
runoff_year_key[year <= 2000, age_range := factor('beforeyear_2000')]
runoff_year_key[year > 2000, age_range := factor('afteryear_2000')]
ggplot(runoff_year_key, aes(age_range, value, fill = age_range)) +
  geom_boxplot() +
  facet_wrap(~sname, scales = 'free_y') +
  scale_fill_manual(values = colset_4[c(4, 1)]) +
  xlab(label = "Age Range") +
  ylab(label = "Runoff (m3/s)") +
  theme_bw()
#In upstream stations the runoffs decreased during the past years. 
#However, the other two key stations stayed unchanged. This is 
#maybe for the reason that the source of the river is losing its resources.


#---------------------------------------------------------Q2
runoff_day_key <- runoff_day[sname %in% key_stations]
year_thres <- 1990
runoff_day_key[year <= year_thres, age_range := factor('beforeyear_1990')]
runoff_day_key[year > year_thres, age_range := factor('afteryear_1990')]

runoff_day_key[, qu_01 := quantile(value, 0.1), by = .(sname, month)]
runoff_day_key[, qu_09 := quantile(value, 0.9), by = .(sname, month)]

runoff_day_key[, runoff_class := factor('medium')]
runoff_day_key[value <= qu_01, runoff_class := factor('low')]
runoff_day_key[value >= qu_09, runoff_class := factor('high')]
runoff_day_key[, days := .N, .(sname, year, runoff_class, season)]
runoff_day_key

runoff_day_key_class <- unique(
  runoff_day_key[, .(sname, days, year, age_range, season, runoff_class)])


ggplot(runoff_day_key_class[season == 'winter' | season == 'summer'], 
       aes(season, days, fill = age_range)) +
  geom_boxplot() +
  facet_wrap(runoff_class~sname, scales = 'free_y') +
  scale_fill_manual(values = colset_4[c(4, 1)]) +
  xlab(label = "Season") +
  ylab(label = "Days") +
  theme_bw()
#less outliers are observed after 1990 in summer this might describe
#that changes in runoff are becoming constant throw years


#---------------------------------------------------------Q3

#linear r
runoff_winter[, value_norm := scale(value), sname]
runoff_summer[, value_norm := scale(value), sname]
n_stations <- nrow(runoff_summary)

ggplot(runoff_winter[year > 1950 & year <= 2010], aes(x = year, y = value_norm, col = sname)) +
  geom_smooth(method = 'lm', formula = y~x, se = 0) + 
  scale_color_manual(values = colorRampPalette(colset_4)(n_stations)) +
  ggtitle('Winter runoff') +
  xlab(label = "Year") +
  ylab(label = "Runoff (m3/s)") +
  theme_bw()

ggplot(runoff_summer[year > 1950 & year <= 2010], aes(x = year, y = value_norm, col = sname)) +
  geom_smooth(method = 'lm', formula = y~x, se = 0) + 
  scale_color_manual(values = colorRampPalette(colset_4)(n_stations)) +
  ggtitle('Summer runoff') +
  xlab(label = "Year") +
  ylab(label = "Runoff (m3/s)") +
  theme_bw()
#both graphs show a continuous increase in winter and decrease in summer, 
#less changes are shown in linear regression 



ggplot(runoff_summer[year > 1950 & year <= 2010], aes(x = year, y = value_norm, col = sname)) +
  geom_smooth(method = 'loess', formula = y~x, se = 0) + 
  scale_color_manual(values = colorRampPalette(colset_4)(n_stations)) +
  ggtitle('Summer runoff') +
  xlab(label = "Year") +
  ylab(label = "Runoff (z-score)") +
  theme_bw()

ggplot(runoff_winter[year > 1950 & year <= 2010], aes(x = year, y = value_norm, col = sname)) +
  geom_smooth(method = 'loess', formula = y~x, se = 0) + 
  scale_color_manual(values = colorRampPalette(colset_4)(n_stations)) +
  ggtitle('Winter runoff') +
  xlab(label = "Year") +
  ylab(label = "Runoff (z-score)") +
  theme_bw()
#both graphs are more accurate than the ones using linear r. in the summer graph it kind of looks like the runoff stabilizes after 2010 and there isn't a big decrease except in 2 stations from the 1950
#in the winter it looks like the highest runoff was around 1990 and in the summer around 1960 _1970
# however, by comparing the graph of _today and _2010 we consider that the plots are smoother 
#and the curve of summer_2010 has sharpen maximums in 1980
#Also in comparison to _today we see that the graoh of 2010 decreased by the end of the period