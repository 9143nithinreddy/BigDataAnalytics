library(datasets)
library(scatterplot3d)
install.packages("scatterplot3d")
data("airquality")

scatterplot3d(
  airquality$Ozone, airquality$Wind, airquality$Temp, 
  color = as.numeric(factor(airquality$Month)),
  pch = 19,
  main = "3D Scatter Plot of Airquality Dataset",
  xlab = "Ozone",
  ylab = "Wind",
  zlab = "Temperature"
)
legend(
  "topright", legend = unique(airquality$Month), 
  col = unique(as.numeric(factor(airquality$Month))), pch = 19, title = "Month"
) 


# Replace NA values with the mean of Ozone
airquality$Ozone[is.na(airquality$Ozone)] <- mean(airquality$Ozone, na.rm = TRUE)

# Plot the line chart
plot(airquality$Ozone, type = "l", col = "blue", lwd = 2,
     main = "Line Chart of Ozone Levels",
     xlab = "Observation Index", ylab = "Ozone Levels")
