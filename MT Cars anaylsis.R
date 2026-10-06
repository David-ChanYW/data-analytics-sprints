## Load necessary packages
library(tidyr)
library(dplyr)
library(ggplot2)

data(mtcars)
head(mtcars)

str(mtcars)


## Task 1: Fuel efficiency
# mpg is miles per gallon (fuel usage)
mpg_mean = mean(mtcars$mpg)
mpg_median = median(mtcars$mpg)
mpg_range = range(mtcars$mpg)

writeLines(paste0("The mean is ", mpg_mean, "\n",
                 "The median is ", mpg_median, "\n",
                 "The range is ", mpg_range[1], " to ", mpg_range[2]))

## Task 2: Transmission Type vs Fuel Economy
# am = 0 is automatic transmission

t.test(mpg ~ am, data = mtcars, alternative = "two.sided", mu = 0, var.equal = FALSE, conf.level = 0.95)

# The t-value is -3.7671, p-value is 0.001374
# This reject H0: there is not different between the 2 group
# It is true that the manual car is more efficient

## Task 3: Trade-off, power vs efficiency
# mpg is miles per gallon (fuel usage)
# hp is the horse power

ggplot(data = mtcars, aes(x = hp, y = mpg)) +
  geom_point()

# This shows a logarithmic relationship between mpg and hp: Increase in hp will decrease mpg

hp_model = lm(mpg ~ hp, data = mtcars)
summary(hp_model)

## Task 4: Vehicle Weight and Engine Size
mtcars_segmented = mtcars %>%
  mutate(
    weight_tier = case_when(
      wt < 2.5 ~ "Light",
      wt <= 3.5 ~ "Medium",
      wt > 3.5 ~ "Heavy"
    ),
    weight_tier = factor(weight_tier, levels = c("Light", 
                                                 "Medium", 
                                                 "Heavy"))
  )

mtcars_segmented %>% 
  select(mpg, wt, disp, weight_tier) %>% 
  head()

## Summarize the car count
tier_summary <- mtcars_segmented %>%
  group_by(weight_tier) %>%
  summarize(
    car_count = n(),
    avg_weight = mean(wt) * 1000, # Convert back to actual lbs
    avg_displacement = mean(disp),
    avg_mpg = mean(mpg)
  )

print(tier_summary)

# 3. Create a scatter plot faceted by weight tier
ggplot(mtcars_segmented, aes(x = disp, y = mpg)) +
  geom_point(aes(color = weight_tier), size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "black", linetype = "dashed") +
  facet_wrap(~ weight_tier) +
  labs(
    title = "Fuel Economy vs. Engine Size across Weight Tiers",
    x = "Engine Displacement (cu. in.)",
    y = "Miles Per Gallon (MPG)",
    color = "Weight Classification"
  ) +
  theme_minimal()
