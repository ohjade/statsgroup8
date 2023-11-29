flies <- c14[,'TF']

```{r}
# Convert dataframe into time series object
ts_flies <- zoo(c14$TF, order.by = c14$week)
ts_para <- zoo(c14$TP, order.by = c14$week)

zoo# Specify the lag for seasonal pattern
lag_for_seasonality <- 4

# Calculate net growth rate with lag for seasonality
net_growth_rate <- c(0, diff(ts_flies, lag = lag_for_seasonality) / lag(ts_flies, lag = lag_for_seasonality)) * 100

# Plot the results
plot(ts_flies[-(1:lag_for_seasonality)], net_growth_rate, type = "b", pch = 16, col = "blue",
     main = "Net Growth Rate vs. Number of Flies",
     xlab = "Number of Flies at Time t",
     ylab = "Net Growth Rate (%)")

# Create a data frame with the data
df <- data.frame(Number_of_Flies = ts_flies[-(1:lag_for_seasonality)],
                 Net_Growth_Rate = net_growth_rate)

# Plot using ggplot2
ggplot(df, aes(x = Number_of_Flies, y = Net_Growth_Rate)) +
  geom_line(color = "blue") +
  labs(title = "Net Growth Rate vs. Number of Flies",
       x = "Number of Flies at Time t",
       y = "Net Growth Rate (%)")

#logs of differences between time points
#plot against t-1, scatter
#do for flies and wasps to look at feeback - density dependant relationship
#look at difference in replicates

'''ln it
'''

# Calculate the differences between consecutive time points
delta_X <- diff(ts_flies)

# Calculate the natural logarithm of the differences
ln_delta_X <- log(abs(delta_X))

# Create a vector of X_{t-1}
X_t_minus_1 <- ts_flies[-length(ts_flies)]

df <- data.frame(X_t_minus_1 = X_t_minus_1, ln_delta_X = ln_delta_X)

model <- gam(y ~ s(x), data = df, family = binomial)

# Plot the scatter plot with a smooth curve
ggplot(df, aes(x = X_t_minus_1, y = ln_delta_X)) +
  geom_point(color = "blue") +
  geom_smooth(method = "gam", formula = y ~ s(x), family = binomial, se = FALSE, color = "red", linetype = "dashed") +
  labs(title = "ln(Delta X_t) vs. X_{t-1}",
       x = "X_{t-1}",
       y = "ln(Delta X_t)")

'''parasites'''

# Calculate the differences between consecutive time points
delta_X <- diff(ts_para)

# Calculate the natural logarithm of the differences
ln_delta_X <- log(abs(delta_X))

# Create a vector of X_{t-1}
X_t_minus_1 <- ts_para[-length(ts_para)]

df <- data.frame(X_t_minus_1 = X_t_minus_1, ln_delta_X = ln_delta_X)

model <- gam(y ~ s(x), data = df, family = binomial)

# Plot the scatter plot with a smooth curve
ggplot(df, aes(x = X_t_minus_1, y = ln_delta_X)) +
  geom_point(color = "blue") +
  geom_smooth(method = "gam", formula = y ~ s(x), family = binomial, se = FALSE, color = "red", linetype = "dashed") +
  labs(title = "ln(Delta X_t) vs. X_{t-1}",
       x = "X_{t-1}",
       y = "ln(Delta X_t)")
