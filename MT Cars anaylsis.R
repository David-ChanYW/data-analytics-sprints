## Load necessary packages
library(tidyr)


data(mtcars)
head(mtcars)

str(mtcars)


## Task 1: Fuel efficiency
# mpg is miles per gallon (fuel usage)
mpg_mean = mean(mtcars$mpg)
mpg_median = median(mtcars$mpg)
mpg_range = range(mtcars$mpg)

writeLines(paste("The mean is", mpg_mean, "\n",
                 "The median is", mpg_median, "\n",
                 "The range is", mpg_range[1], "to", mpg_range[2]))

## Task 2: Transmission Type vs Fuel Economy
# am = 0 is automatic transmission

t.test(mpg ~ am, data = mtcars, alternative = "two.sided", mu = 0, var.equal = FALSE, conf.level = 0.95)

# The t-value is -3.7671, p-value is 0.001374
# This reject H0: there is not different between the 2 group
# It is true that the manual car is more efficient

## Task 3: Trade-off, power vs efficiency
