### Research  Question: 
# Exploring the geo-spatial associations between socioeconomic deprivations and homicide rate in US (1990s)
# Dependent variable: homicide rate in 1990s (HR90)
# Independent variables: RD90-resource deprivation/affluence, DV90-divorce rate, 
# MA90-the median age, PS90-population structure, UE90-unemployment rate
# spatial area：US
# temporal coverage: 1990s (1990-1999)


# set work directory in windows
setwd("D:/Desktop/GEOG0114/GEOG0114_assessment/US_Crime")


### Load Packages
library("sf")
library("tmap")
library("spdep")
library("sp")
library("spatialreg")
library("tidyverse")


### Data Wrangling

## Loading data
# Import NOCVR shape file in US
NCOVRshp <- st_read("ncovr/NAT.shp")
# 3085 columns of 69 variables, CRS: WGS 84

# Import boundaries for STATE.shp in US
STATE <- st_read("cb_2018_us_state_500k/cb_2018_us_state_500k.shp") # Geodetic CRS:  NAD83
# Transform CRS to WGS 84
STATEshp <- STATE %>%
  st_transform(.,4326)


## Make a map
# Generate an empty map to visualize the spatial configuration and hierarchy of US
# First add Counties layer 
US <- tm_shape(NCOVRshp) + tm_polygons() +
  # Add States layer on top of Counties layer and make it transparent with alpha = 0
  tm_shape(STATEshp) + tm_polygons(alpha = 0, border.alpha = 1, border.col = "black") +
  # Apply cosmetics by adding compass and scale
  tm_compass(position = c("right", "bottom")) + tm_scale_bar(position = c("left", "bottom"))
US


## Descriptive analysis

# Reporting basic summary statistical measures
summary(NCOVRshp$HR90)
# the mean HR90 is 6.183 with ranges from 0.000 to 71.378. 
sd(NCOVRshp$HR90)
# the standard deviation of HR90 is 6.641407
summary(NCOVRshp$RD90)
# the mean RD90 is 0.0000 with ranges from -2.4103 to 5.5831. 
sd(NCOVRshp$RD90)
# the standard deviation of RD90 is 1
summary(NCOVRshp$DV90)
# the mean DV90 is 7.160 with ranges from 2.075 to 17.190. 
sd(NCOVRshp$DV90) # 1.733121
summary(NCOVRshp$MA90)
# the mean MA90 is 34.41 with ranges from 20.00 to 55.40. 
sd(NCOVRshp$MA90) # 3.599675
summary(NCOVRshp$PS90)
# the mean PS90 is 0.00000 with ranges from -3.83513 to 4.21350. 
sd(NCOVRshp$PS90) # 1
summary(NCOVRshp$UE90)
# the mean PS90 is 6.181 with ranges from  0.000 to 30.534. 
sd(NCOVRshp$UE90) # 3.055313

# Plot histogram to determine the skewness of indicators 
par(mfrow=c(2,3)) 
# histogram of HR90
hist((as.numeric(NCOVRshp$HR90)), 
     main="Homicide Rate in 1990s", 
     xlab="HR90", 
     ylab="Frequency")
# histogram of RD90
hist((as.numeric(NCOVRshp$RD90)), 
              main="Resource Deprivation/Affluence in 1990s", 
              xlab="RD90", 
              ylab="Frequency")
# histogram of DV90
hist((as.numeric(NCOVRshp$DV90)), 
              main="Divorce Rate in 1990s", 
              xlab="DV90", 
              ylab="Frequency")
# histogram of MA90
hist((as.numeric(NCOVRshp$MA90)), 
              main="Median Age in 1990s", 
              xlab="MA90", 
              ylab="Frequency")
# histogram of PS90
hist((as.numeric(NCOVRshp$PS90)), 
              main="Population Structure in 1990s", 
              xlab="PS90", 
              ylab="Frequency")
# histogram of UE90
hist((as.numeric(NCOVRshp$UE90)), 
              main="Unemployment Rate in 1990s", 
              xlab="UE90", 
              ylab="Frequency")
# From the figure, we can see that some indicators are not normally distributed and exist skewness.
# and need to be transformed into a more normally distributed version.
# Since there are variables with negative value, log10() cannot be used to transform data, 
# we will express the residuals by standard deviations away from the mean when analyze residuals later.


### Diagnostics of residuals

## Reporting the spatial distribution of the variables

# generate the first map to inspect the distribution of dependent var - homicide rate
plot1 <- tm_shape(NCOVRshp) + 
  tm_fill("HR90", title = "Homicide Rate (Quantiles)", style="quantile", n = 5, palette = "Reds") +
  tm_shape(STATEshp) +
  tm_polygons(alpha = 0, border.alpha = 1, border.col = "black") +
  tm_text("STUSPS", size = "AREA") +
  tm_compass(position = c(0.75, 0.80), size = 1.1) +
  tm_scale_bar(position = c("left","bottom"), size = 0.4) +
  tm_layout(frame = FALSE, main.title = "Homicide Rate across US Counties, 1990s", main.title.size = 0.7 ,
            legend.position = c(0.85, 0.01), legend.title.size = 0.5,legend.text.size = 0.5)
plot1
# Counties in the parts of southeast and southwest tend to have high homicide rates exceeding 10.33 per 100,000 persons,
# whereas most counties in the northeast of US have lower homicide rates below 3.12 per 100,000 persons

# generate other five maps to inspect the distribution of 5 independent variables
# map for Resource Deprivation/Affluence
plot2 <- tm_shape(NCOVRshp) + 
  tm_fill("RD90", title = "Resource Deprivation", style="cont", midpoint = 0, palette = "-PRGn") +
  tm_shape(STATEshp) +
  tm_polygons(alpha = 0, border.alpha = 1, border.col = "black") +
  tm_text("STUSPS", size = "AREA") +
  tm_compass(position = c(0.75, 0.75), size = 0.8) +
  tm_scale_bar(position = c("left","bottom")) +
  tm_layout(frame = FALSE, legend.position = c(0.83,0.03), legend.title.size = 0.45, legend.text.size = 0.4)
# map for divorce rate
plot3 <- tm_shape(NCOVRshp) + 
  tm_fill("DV90", title = "Divorce Rate", style="quantile", n = 5, palette = "Oranges") +
  tm_shape(STATEshp) +
  tm_polygons(alpha = 0, border.alpha = 1, border.col = "black") +
  tm_text("STUSPS", size = "AREA") +
  tm_compass(position = c(0.75, 0.75), size = 0.8) +
  tm_scale_bar(position = c("left","bottom")) +
  tm_layout(frame = FALSE, legend.position = c(0.83,0.03), legend.title.size = 0.45, legend.text.size = 0.4)
# map for median age
plot4 <- tm_shape(NCOVRshp) + 
  tm_fill("MA90", title = "Median Age", style="quantile", n = 5, palette = "Blues") +
  tm_shape(STATEshp) +
  tm_polygons(alpha = 0, border.alpha = 1, border.col = "black") +
  tm_text("STUSPS", size = "AREA") +
  tm_compass(position = c(0.75, 0.75), size = 0.8) +
  tm_scale_bar(position = c("left","bottom")) +
  tm_layout(frame = FALSE, legend.position = c(0.83,0.03), legend.title.size = 0.45, legend.text.size = 0.4)
# map for population structure
plot5 <- tm_shape(NCOVRshp) + 
  tm_fill("PS90", title = "Population Structure", style="cont", midpoint = 0, palette = "PuOr") +
  tm_shape(STATEshp) +
  tm_polygons(alpha = 0, border.alpha = 1, border.col = "black") +
  tm_text("STUSPS", size = "AREA") +
  tm_compass(position = c(0.75, 0.75), size = 0.8) +
  tm_scale_bar(position = c("left","bottom")) +
  tm_layout(frame = FALSE, legend.position = c(0.83,0.03), legend.title.size = 0.45, legend.text.size = 0.4)
# map for unemployment rate
plot6 <- tm_shape(NCOVRshp) + 
  tm_fill("UE90", title = "Unemployment Rate", style="quantile", n = 5, palette = "Purples") +
  tm_shape(STATEshp) +
  tm_polygons(alpha = 0, border.alpha = 1, border.col = "black") +
  tm_text("STUSPS", size = "AREA") +
  tm_compass(position = c(0.75, 0.75), size = 0.8) +
  tm_scale_bar(position = c("left","bottom")) +
  tm_layout(frame = FALSE, legend.position = c(0.83,0.03), legend.title.size = 0.45, legend.text.size = 0.4)
# stitch the maps together using tmap_arrange() function
tmap_arrange(plot2, plot3, plot4, plot5, plot6, nrow = 2)

# Visually, all these 5 indicators appears to be correlated with homicide rate. 

## Fitting a non-spatial Linear Regression on spatial data and check the residuals

# lm() function builds a regression model and stores model output into the object 'fit_1'
# Since there are variables with negative value, log10() cannot be used to transform data:
# fit_1 <- lm(log10(HR90) ~ log10(RD90) + log10(DV90) + log10(MA90) + log10(PS90) + log10(UE90), data = NCOVRshp)
fit_1 <- lm(HR90 ~ RD90 + DV90 + MA90 + PS90 + UE90, data = NCOVRshp)
str(fit_1)
# An element of this object then includes the residual for each of the observations
summary(fit_1)
# Adjusted R square = 0.4173 , p-value < 2.2e-16

# Extract residuals from "fit_1" object and dump into "NCOVRshp" and call the column "res_fit1"
NCOVRshp$res_fit1 <- residuals(fit_1)
# if the residual is negative, then the observed value is lower than the predicted 
# (that is, the model is overpredicting the level of homicide for that observation) 
# if the residual is positive, then the observed value is higher than the predicted 
# (that is, the model is underpredicting the level of homicide for that observation).

# Extract the predicted values
NCOVRshp$fitted_fit1 <- fitted(fit_1)

# Reporting basic summary measures of residuls
summary(NCOVRshp$res_fit1)
# the mean of residuals is 0 with range from -19.1022 to 68.1403
# we will express the residuals in terms of standard deviations away from the mean. (sd_breaks)
NCOVRshp$sd_breaks <- (NCOVRshp$res_fit1 - mean(NCOVRshp$res_fit1)) / sd(NCOVRshp$res_fit1)
# Reporting basic summary measures of sd_breaks
summary(NCOVRshp$sd_breaks)
# the mean of residuals is 0 with range from -3.7710 to 13.4518

# plot choropleth maps of sd_breaks
my_breaks <- c(-14,-3,-2,-1,1,2,3,14)
# where 1 means 1 standard deviation higher than the mean 
# and -1 would be 1 s.d. lower than the mean
plot_LR <- tm_shape(NCOVRshp) + 
  tm_fill("sd_breaks", title = "RESIDUALS", style = "fixed", breaks = my_breaks, palette = "-RdBu") +
  tm_shape(STATEshp) + tm_polygons(alpha = 0, border.alpha = 1, border.col = "black") +
  tm_text("STUSPS", size = "AREA") +
  tm_compass(position = c(0.75, 0.80), size = 1.1) +
  tm_scale_bar(position = c("left", "bottom"), size = 0.4) +
  tm_layout(frame = FALSE, 
            legend.position = c(0.85,0.01), legend.title.size = 0.5, legend.text.size = 0.5)
plot_LR
# Since if we assume normal distribution of residuals, 
# then 68% of all counties should lie within the middle band from -1 to +1 s.d.
# From the plot, we can roughly conclude that the residuals exhibit a normal distribution
# And the spatial patterning of blue areas are over-prediction and red areas are under-prediction
# This visual inspection of the residuals shows that spatial autocorrelation may be present here. 
# Therefore,we would require a more formal test.


## Confirm the presence of spatial autocorrelation -- Moran’s I test

# generate unique number for each row
NCOVRshp$ROWNUM <- 1:nrow(NCOVRshp)
# coerce the sf object into a new sp object
NCOVR_sp <- as(NCOVRshp, "Spatial")
# Create spatial weights matrix for areas
Weights <- poly2nb(NCOVR_sp, row.names = NCOVR_sp$ROWNUM)
WeightsMatrix <- nb2mat(Weights, style='B')
Residual_WeightMatrix <- mat2listw(WeightsMatrix , style='W')

# Run the test on the linear regression model output object "fit_1" using lm.morantest()
lm.morantest(fit_1, Residual_WeightMatrix, alternative="two.sided")
# we obtained a statistically significant value (i.e., p-value < 0.001) for Moran’s I of value = 0.1301782844. 
# The value of the Moran’s I test is not too high. 
# However, we still need keep in mind that if the residuals exist spatial autocorrelation (the residuals are re somewhat related to each other and thus not independent)
# Therefore, A spatial regression would be much appropriate for modelling this type of data since there’s evidence of spatial autocorrelation.


### Spatial Regression Model

## Spatial Lag Model on the dependent variable
# we assume that the the homicide rate in one county is impacted by the homicide rate from nearby neighbouring counties
# Fit model using lagsarlm()
# reuse spatial weight matrix created earlier as an object called "Residual_WeighMatrix" 
fit_2_lag <- lagsarlm(HR90 ~ RD90 + DV90 + MA90 + PS90 + UE90, data = NCOVR_sp, Residual_WeightMatrix)

# Report results with summary()
summary(fit_2_lag)
# The pho = 0.25948 informs us of how the neighbouring counties' homicide rates affect the homicide rate at y.
# pho value is positive, which means the neighbouring counties affect is a positive manner, and it is statistically significant (i.e., p-value < 0.001). 
# We can see the AIC for the lag model is lower than the original linear regression model (i.e., Lag: 18636 vs LM: 18778) therefore the lag model is okay.

# extract the residuals for fit_2_lag object and dump back to original sf NCOVRshp object
NCOVRshp$res_fit2_lag <- fit_2_lag$residuals

# express the residuals in terms of standard deviations away from the mean. (RESID_SLY)
NCOVRshp$RESID_SLY <- (NCOVRshp$res_fit2_lag - mean(NCOVRshp$res_fit2_lag)) / sd(NCOVRshp$res_fit2_lag)
# use Moran's I test using moran.mc() function
moran.mc(NCOVRshp$RESID_SLY, Residual_WeightMatrix, 1000, zero.policy = T)
# Moran’s I in this lag model is 0.0029274, much lower than Moran’s I of linear regression model (0.1301782844) and p-value = 0.3736 > 0.05
# Thus, the lag model has accounted for a lot of spatial autocorrelation.

# generate the map
# Reporting basic summary measures of RESID_SLY
summary(NCOVRshp$RESID_SLY)
# the mean of residuals is 0 with range from -3.5634 to 13.9439
# plot choropleth maps of sd_breaks
my_breaks <- c(-14,-3,-2,-1,1,2,3,14)
# where 1 means 1 standard deviation higher than the mean 
# and -1 would be 1 s.d. lower than the mean
# map
plot_SLY <- tm_shape(NCOVRshp) + 
  tm_fill("RESID_SLY", title = "LAG_RESIDUALS", style = "fixed", breaks = my_breaks, palette = "-RdBu") +
  tm_shape(STATEshp) + tm_polygons(alpha = 0, border.alpha = 1, border.col = "black") +
  tm_text("STUSPS", size = "AREA") +
  tm_compass(position = c(0.75, 0.80), size = 1.1) +
  tm_scale_bar(position = c("left", "bottom"), size = 0.4) +
  tm_layout(frame = FALSE, 
            legend.position = c(0.85,0.01), legend.title.size = 0.5, legend.text.size = 0.5)
plot_SLY
# In the map output of the residual lags, we can conclude that the spatial lag model
# does not address all the issues of spatial autocorrelation in the model's residuals.

# Interpretation of results using impacts
# impacts
Weights_2.0 <- as(Residual_WeightMatrix, "CsparseMatrix")
trMC <- trW(Weights_2.0, type="MC")
summary(impacts(fit_2_lag, tr = trMC, R=100), zstats=TRUE)
# e.g. RD90: if the levels of Resource Deprivation were to increase by 1%,
# for the direct effects in its own county, this will cause an increase in the homicide rate by 3.84% (p < 0.001)
# for the indirect affects, if the Resource Deprivation were to change across neighbouring counties, this will affect the value of our homicide rates by 1.278% (p < 0.001). 
# The total column is the combined effect.


## Spatial Error Model

# Fit the Spatial Error Model
fit_3_error <- errorsarlm(HR90 ~ RD90 + DV90 + MA90 + PS90 + UE90, data = NCOVR_sp, Residual_WeightMatrix)

# Report results with summary()
summary(fit_3_error)
# Lambda: 0.32614, p-value: < 2.22e-16, AIC: 18643, (AIC for lm: 18778)
# The λ value is a positive value of 0.32614 which means the affect of neighbouring Counties are positive 
# and the impact is statistically significant (i.e., p-value < 0.001). 
# the AIC for the lag model is lower than both the original linear regression & error model (i.e., Error: 18643 vs LM: 18778 & Lag: 18636) 
# therefore the lag model better than the two.

# extract the residuals for fit_3_error object and dump back to original sf NCOVRshp object
NCOVRshp$res_fit3_error <- fit_3_error$residuals
# express the residuals in terms of standard deviations away from the mean. (RESID_SER)
NCOVRshp$RESID_SER <- (NCOVRshp$res_fit3_error - mean(NCOVRshp$res_fit3_error)) / sd(NCOVRshp$res_fit3_error)
# use Moran's I test using moran.mc() function
moran.mc(NCOVRshp$RESID_SER, Residual_WeightMatrix, 1000, zero.policy = T)
# Moran’s I is -0.011235 which is negative, but Moran’s I value of lag model is cloest to zero. 
# On top of that there is no evidence of spatial autocorrelation since its p-value is not significant. (p-value = 0.8262)
# Therefore, we can conclude that the spatial error model does address the issue of spatial autocorrelation in the residuals. 
# Lag model is a better model than the Linear regression and Error model for exploring the relationship with those independent variables and accounting for spatial autocorrelation. 

# generate the map
# Reporting basic summary measures of RESID_SER
summary(NCOVRshp$RESID_SER)
# the mean of residuals is 0 with range from -3.5359 to 13.8981 
# plot choropleth maps of sd_breaks
my_breaks <- c(-14,-3,-2,-1,1,2,3,14)
# where 1 means 1 standard deviation higher than the mean 
# and -1 would be 1 s.d. lower than the mean
# map
plot_SER <- tm_shape(NCOVRshp) + 
  tm_fill("RESID_SER", title = "ERROR_RESIDUALS", style = "fixed", breaks = my_breaks, palette = "-RdBu") +
  tm_shape(STATEshp) + tm_polygons(alpha = 0, border.alpha = 1, border.col = "black") +
  tm_text("STUSPS", size = "AREA") +
  tm_compass(position = c(0.75, 0.80), size = 1.1) +
  tm_scale_bar(position = c("left", "bottom"), size = 0.4) +
  tm_layout(frame = FALSE, 
            legend.position = c(0.85,0.01), legend.title.size = 0.5, legend.text.size = 0.5)
plot_SER
