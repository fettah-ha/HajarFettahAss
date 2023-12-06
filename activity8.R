clim <- read.csv("https://userpage.fu-berlin.de/soga/data/raw-data/Climfrance.csv", sep = ";")
str(clim)
clim$altitude
clim$altitude <- as.numeric(gsub(",", "", clim$altitude))
clim$altitude
install.packages("raster")
install.packages("geodata")
install.packages("scatterplot3d")
library(tidyverse)
library(broom)
library(sp)
library(raster)
library(rgl)
library(ggplot2)
library(maps)
library(scatterplot3d)


G1 <- raster::getData(country = "France", level = 1)


ggplot() +
  geom_polygon(
    data = G1,
    aes(x = long, y = lat, group = group),
    colour = "grey10", fill = "#fff7bc"
  ) +
  geom_point(
    data = clim,
    aes(x = lon, y = lat),
    alpha = .5,
    size = 2
  ) +
  theme_bw() +
  xlab("Longitude") +
  ylab("Latitude") +
  coord_map()




#Exersice1
climfrar <- clim[1:34,]
model <- lm(t_mean ~ altitude + lat + lon, data = clim)
coef(model)
summary(model)
y#the estimate value for the intercept is 18.45
#for lat the estimate coef is -0.16
#for lon estimate coeff is 0.07
#The estimated mean value of the response variable (t_mean) when latitude and 
#longitude are both zero.
#For each one-unit increase in latitude, the mean value of t_mean decreases 
#by 0.16383 units. However, this effect is not statistically significant 
#at the 0.05 significance level (p-value = 0.4640)
#For each one-unit increase in longitude, the mean value of t_mean increases by 0.07603 units. However, this effect is not statistically significant at the 0.05 significance level (p-value = 0.6314)

# Exersice2
climfrar<- clim[1:34,]
model2 <- lm(t_mean ~ altitude+ lat, data = clim)
summary(model2)
#Interpretation the results
#For each one-unit increase in latitude, the mean value of t_mean decreases 
#by 0.2205 units. However, this effect is marginally non-significant at 
#the 0.05 significance level (p-value = 0.100)


pred_t_mean <- predict(model2, newdata = list("altitude" = c(1212, 2860), "lat" = c(44.2, 42.9)), interval = "p", level = 0.95)
pred_t_mean

#exxercie3

scatter_3d <- with(climfrar, scatterplot3d(altitude, lat, t_mean,
                                           pch = 16, highlight.3d = TRUE,
                                           angle = 45,
))
scatter_3d$plane3d(model2)
