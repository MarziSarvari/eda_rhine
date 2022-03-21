runoff <- 130
runoff_ts <- data.frame(time = as.Date(1:90, origin = '2020/12/31'), 
                        value = sample(c(130, 135, 140), 
                                       size = 90, replace = T))
head(runoff_ts)

library(data.table)
runoff_dt <- data.table(runoff_ts)
runoff_dt[, mon := month(time)]
runoff_dt[, mon_mean := mean(value), by = mon]
runoff_month <- runoff_dt[, .(mon, mon_mean)] 
runoff_month
unique_runoff <- unique(runoff_month)


percentage_change = function(first_month_avg, second_month_avg){
  raw_difference <- second_month_avg - first_month_avg
  final_percentage <- (raw_difference/10)*100
}



january_percentage <- percentage_change(unique_runoff[1,mon_mean],unique_runoff[2,mon_mean])


february_percentage <- percentage_change(unique_runoff[2,mon_mean],unique_runoff[3,mon_mean])

unique_runoff[, differential_percentage := c(0, january_percentage, february_percentage)]
unique_runoff
