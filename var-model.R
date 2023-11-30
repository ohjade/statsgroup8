library(readxl)
c38 <- read_excel("Documents/DTP_23_TS_DATA/C38.xlsx")

flies = c11[,10]
parasites = c11[,11]

# Plot the autocorrelation for flies
acf(flies, main="Autocorrelation of Flies")

# Plot the autocorrelation for parasites
acf(parasites, main="Autocorrelation of Parasites")

# Combine the series into a multivariate time series object
timeseries_data = cbind(flies, parasites)


timeseries_data = log(timeseries_data)

timeseries_data[is.na(timeseries_data)] <- 0
timeseries_data[timeseries_data < 0] <- 0



# Load the vars package
library(vars)

# Specify and fit the VAR model
# The 'p' value is the order of the VAR model, which you need to determine
# You can use AIC or BIC criteria to select the appropriate order
var_model = VAR(timeseries_data, p = 1)

# View summary of the model
summary(var_model)

# Predict future values
# You can specify the number of steps ahead for prediction
predictions = predict(var_model, n.ahead = 10)

# Plot the predictions
plot(predictions)



# Assuming 'var_model' is your fitted VAR model

# Generate fitted values from the VAR model for the historical data
fitted_results <- fitted(var_model)

# Extract the fitted values for X0
fitted_X0 <- fitted_results[, "flies"]

# Extract original X0 data
original_X0 <- timeseries_data[, "flies"]

# Plot the original data
plot(original_X0, type = "l", col = "blue", xlab = "Time", ylab = "Value", main = "Fitted vs Actual Host Number")

# Add the fitted data to the plot
lines(fitted_X0, col = "red", lty = 2)

# Add a legend
legend("topright", legend = c("Actual number of hosts", "Fitted number of hosts"), col = c("blue", "red"), lty = 1:2)



# Generate fitted values from the VAR model for the historical data
fitted_results <- fitted(var_model)

# Extract the fitted values for X0
fitted_X40 <- fitted_results[, "parasites"]

# Extract original X0 data
original_X40 <- timeseries_data[, "parasites"]

# Plot the original data
plot(original_X40, type = "l", col = "blue", xlab = "Time", ylab = "Value", main = "Fitted vs Actual X40")

# Add the fitted data to the plot
lines(fitted_X40, col = "red", lty = 2)

# Add a legend
legend("topright", legend = c("Actual X40", "Fitted X40"), col = c("blue", "red"), lty = 1:2)



# Assuming your dataframe is named 'timeseries_data' with columns 'x' and 'y'
x <- timeseries_data$
y <- timeseries_data$X0

# Plotting x against y
plot(x, y, type = "p", col = "blue", xlab = "X-axis label", ylab = "Y-axis label", main = "Title of the Plot")






# Load necessary library
install.packages("mgcv")
library(mgcv)

# Assuming `timeseries_data` is your dataframe with columns `X40` and `X0`
gam_model <- gam(X0 ~ s(X40), data = timeseries_data)

# Check the summary for model details
summary(gam_model)

# Plot the smooth term to visualize the relationship
plot(gam_model)




# Load necessary library
install.packages("nnet")
library(nnet)

# Fit a neural network model
# For this example, we'll use a single hidden layer with 3 nodes
nn_model <- nnet(X0 ~ X40, data = timeseries_data, size = 3, linout = TRUE)

# Check the summary for model details
summary(nn_model)


