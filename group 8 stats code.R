flies = c14['TF']
fliestime = c14[c('TF', 'week')]
parasites = c14['TP']
ptime <- c14[c('TP', 'week')]
fliespara <- c14[c('TP', 'TF')]

#Plot the time series to visually inspect
#Flies and parasites over time
ggplot(c14, aes(x = week)) +
  geom_line(aes(y = TF), color = "blue", size = 1, linetype = "solid", alpha = 0.8) +
  geom_line(aes(y = TP), color = "red", size = 1, linetype = "solid", alpha = 0.8) +
  labs(title = "Flies and Parasites Over Time",
       x = "Weeks",
       y = "Counts")

#Do the acf to determine the autoregressive (p) component in an ARIMA model
#All data interactions, 2x2 plot
acf(fliespara, lag.max = 20, plot = TRUE)

#Not stationary
#Best to use 4 as lifespan of fly, or maybe 5
#Then do for other three series

#Condition the data?
#Flies
flies$TF_c <- c(rep(mean(flies$TF), 4), diff(flies$TF, lag = 4))
#Parasites
parasites$TP_c <- c(rep(mean(parasites$TP), 4), diff(parasites$TP, lag = 4))
#Join back together
flies_c <- flies[,'TF_c']
parasites_c <- parasites[,'TP_c']
fliesparasites_c <- cbind(flies_c, parasites_c)

#Replot the conditioned data
acf(fliesparasites_c, lag.max = 20, plot = TRUE)

#Next steps: look at the other cage runs
#Are the significant lags at the same place?
#The lags once corrected then become the number of weeks you specified in the diff
#Can look if there is the same importance of weeks in the other three time series
#ARMA model on a log scale, choose the same noisy parameter as the ACF one