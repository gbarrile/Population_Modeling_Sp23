

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# POPULATION MODELING IN ECOLOGY
# ZOO 5890-09
# University of Wyoming
# Spring 2023

# Week 1 - Closed Binomial N-mixture model

# Description: Steps through an example analysis of repeated count data
# using the N-mixture model of Royle et al. 2004 as 
# implemented in the package `unmarked`.

# Royle, J.A. (2004) N-Mixture Models for Estimating Population Size
# from Spatially Replicated Counts. Biometrics 60, pp. 108–105.

# Addressing the question: "How does wildfire influence salamander abundance?"

# Gabe Barrile - University of Wyoming
# Last updated 01/23/2023
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# ---- Outline -----
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# This script contains the following sections:
# 1) Install and load packages
# 2) Read-in input data
# 3) Format input data for unmarked
# 4) Fit Closed Binomial N-mixture models
# 5) Prediction and plotting
# 6) Goodness-of-fit
# 7) Produce map of expected abundance
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# ---- 1) Install and load packages -----
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# Check if unmarked and tidyverse are installed.
# If yes, load them.  If not, install, then load.

# unmarked for fitting the Binomial N-mixture model
if("unmarked" %in% rownames(installed.packages()) == FALSE) {
  install.packages("unmarked")
}
require(unmarked)

# reshape to format data
# if("reshape" %in% rownames(installed.packages()) == FALSE) {
#   install.packages("reshape")
# }
# require(reshape)

# tidyverse to format data
if("tidyverse" %in% rownames(installed.packages()) == FALSE) {
  install.packages("tidyverse")
}
require(tidyverse)

# if you needed to cite the unmarked package
citation("unmarked")
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# ---- 2) Read-in input data -----
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

getwd()

# read-in the salamander count data
df <- read.csv("January_23/data/Salamander_Wildfire.csv")


# We now have our data stored as 'df'
# check out our data
head(df)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# QUESTION: how many transects did we survey?
n_distinct(df$Transect)

# QUESTION: how many surveys per transect?
df %>% 
  group_by(Transect) %>% 
  summarise(n=n()) %>% 
  rename(Number.of.Surveys=n) %>% 
  data.frame()
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# ---- 3) Format data for unmarked -----
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

# order the data frame by transect, then survey
df <- df %>% arrange(Transect, Survey)
df

# now format count data for unmarked (pivot data from long format to wide format)

# IF YOU WANT TO FORMAT DATA USING THE 'reshape' PACKAGE
# m <- melt(df, id.var = c("Transect","Survey"), measure.var = "Count")
# y=cast(m, Transect ~ Survey)
# K <- ncol(y)
# C <- as.matrix(y[,2:K])


# IF YOU WANT TO FORMAT DATA USING tidyr and dplyr

# reduce dataframe to columns that we need
m <- df %>% select(Transect, Survey, Count)
# make Transect a factor variable
m$Transect <- as.factor(as.character(m$Transect))
# order the data frame by Survey
m <- m %>% arrange(Survey)
# pivot data to wide format
y <- m %>% pivot_wider(names_from = Survey, values_from = Count, values_fill = NA)
# look at resulting dataframe
y

# unmarked only wants a matrix of counts
C <- as.matrix(y[,2:ncol(y)])


C # each transect is a row (10 rows)
# each column indicates the survey at each site (three surveys at each transect)






#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# QUESTION: Does our matrix of counts match our field data?
# Check: how many salamanders were observed at Transect 2 during Survey 2?
head(df)
# Now using the counts matrix, how many salamanders were observed at 
# Transect 2 during Survey 2?
C
# Do the number of salamanders match between the field data and counts matrix?
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Format covariates for abundance and detection probability
# we hypothesized that percent burned area influences abundance
# we hypothesized that time of day influences detection probability

# Site-level covariates versus Observational covariates

# Site covariates do not change in value across surveys at a given site
# i.e., one value for a site covariate at each transect
# e.g., ten values for percent burned area, one value for each transect 

# We are interested in the abundance at a given transect, which we are assuming
# does not change over our three surveys at that transect. Therefore, if we
# want to estimate transect abundance as a function of percent burned area,
# then the percent burned area also cannot change over our three surveys at that
# given transect

# Observation covariates can change across each survey at a given site
# Observation covariate data should match survey data (i.e., matrix of counts)
# We can have a different detection probability for each survey
# e.g., different time of day for each survey.

# Format percent burned area as a covariate on abundance (as a 'site' covariate)
burned <- unique(df[,c("Transect","Burned")])
burned <- as.matrix(burned[,"Burned"])
burned

# alternative way of formatting if you prefer
# burned <- df[!duplicated(df$Transect), ]
# burned <- as.matrix(burned[,"Burned"])
# burned

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# QUESTION: Do the values for percent burned area match our field data?
# Check: what was the percent burned area at Transect 2?
head(df)
# Now with the percent burned values, what was the percent burned area at Transect 2?
burned
# Do they match?
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Format time of day as a covariate on detection (as an 'observation' covariate)

# IF YOU WANT TO FORMAT DATA USING THE 'reshape' PACKAGE
# m<-melt(df,id.var=c("Transect","Survey"),measure.var="Time")
# y=cast(m, Transect ~ Survey)
# K <- ncol(y)
# time <- as.matrix(y[,2:K])


# IF YOU WANT TO FORMAT DATA USING tidyr and dplyr 

# reduce dataframe to columns that we need
m <- df %>% select(Transect, Survey, Time)
# make Transect a factor variable
m$Transect <- as.factor(as.character(m$Transect))
# order the data frame by Survey
m <- m %>% arrange(Survey)
# pivot data to wide format
y <- m %>% pivot_wider(names_from = Survey, values_from = Time, values_fill = NA)
# check out resulting dataframe
y

# unmarked only wants a matrix of counts
time <- as.matrix(y[,2:ncol(y)])




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# QUESTION: Do the values for time match our field data?
# Check: what time was Survey 3 conducted at Transect 2?
head(df)
# Now with the time values, what time was Survey 3 conducted at Transect 2??
time
# Do they match?
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# remove objects that we don't need
rm(m,y)

# Here are the data for analysis:
C      # matrix of survey data (salamander counts)
burned # percent burned area at each transect (we think it might influence abundance)
time   # time recorded during every survey (we think it might influence detection)

# important to remember that we can also include site covariates on detection

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Let's examine the functions within 'unmarked' that we will use
?pcount # this function fits the model (scroll down & read 'Details' section on your own time)
?unmarkedFramePCount # this function organizes the data for analysis
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Input data into an 'unmarked data frame'
umf <- unmarkedFramePCount(
  y=C,                                   # Counts matrix
  siteCovs= data.frame(burned = burned), # Site covariates
  obsCovs = list(time = time))           # Observation covariates

# Explore umf, the input data to unmarked for analysis
# Good point to stop and ensure you're feeding the model what you think you are
umf
summary(umf)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# QUESTION: Is the number of sites (transects) correct in 'umf'?
# Is the site-covariate correct?
# Is the observation-level covariate correct?
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# ---- 4) Fit Closed Binomial N-mixture models -----
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

# Fit Closed Binomial N-mixture Model
# Let's first fit a null model
m1 <- pcount(~1 ~1, data=umf, K=130) # don't worry about K right now

# Look at output
m1
coef(m1) # lam (short for lambda) refers to the abundance parameter in this case

# Back-transformed output (to values on the scale we humans typically think on)
backTransform(m1, type="state") # Estimate is mean number of salamanders
backTransform(m1, type="det")  # detection on probability scale

# Extract abundance at each transect
meanEst <- bup(ranef(m1))
# compare this to the maximum count at each transect
maxCount <- aggregate(Count ~ Transect, data=df, FUN=max)
maxCount$mean_estimate <- meanEst

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
plot(maxCount$Transect, maxCount$mean_estimate, pch=16, col="blue",
     ylab="Salamander Count / Abundance", xlab="Transect")
points(maxCount$Transect, maxCount$Count, pch=16, col="black")
legend(6, 33, c("Predicted abundance", "Maximum count"),
       col=c("blue", "black"), pch = 16)
# QUESTION: Discuss the results of this figure as a group
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Let's fit a model that addresses our focal question (and hypothesis)
# let's model detection as a function of the time of day when a survey was conducted
# let's model abundance as a function of the percent burned area at each transect

# linear model for p (detection) follows first tilde, 
# then comes linear model for abundance (may see abundance ref. to as lambda for this model)
m2 <- pcount(~time ~burned, data=umf, K=130)
# Here 'K' is the upper summation limit for the summation over the random effects
# in the integrated likelihood. In unmarked, the default choice of K is the maximum
# observed count plus 100. That should normally be adequate, but you can play with K
# to determine the sensitivity of estimates to the value set for K.
max(C) # max observed count was 30...30+100=130 for K

# Model-fitting function 'pcount' stands for 'point count' as this model can be
# employed with point count data. However, applications of the binomial mixture
# model are not restricted to point count data, as we can see with our use
# of counts along transects.


# NOTE: if you have transects or sampling areas of different lengths or sizes and/or
#       if you want to estimate the number of individuals per unit area (density),
#       you can add an offset term to the right-hand side of the abundance formula.

# For example: 
# m2 <- pcount(~time ~burned + offset(log(TransectLength)), data=umf, K=130) or
# m2 <- pcount(~time ~burned + offset(log(Area)), data=umf, K=130)

# In the two example models above, you would simply code/include 'TransectLength'
# or 'Area' in your unmarked dataframe as a site-level covariate.




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# QUESTIONS:
# Look at output
m2
# Positive or negative relationship with abundance and percent burned area? 
# Is the relationship significant?

# Positive or negative relationship with detection and time of day? 
# Is the relationship significant?
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# ---- 5) Prediction and plotting -----
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

# Predictions of abundance at specified values of percent burned area, say 0, 30, and 60)
newdat <- data.frame(burned=c(0, 30, 60))
predict(m2, type="state", newdata=newdat, append = T)
# 'predict' here uses delta rule to compute SEs and 95% CIs

# Predictions of p (detection probability) for values of time (e.g., 7am, 9am, and 11am)
newdat <- data.frame(time=c(7,9,11))
predict(m2, type="det", newdata=newdat, append = T)

# Visualize covariate relationships

# For abundance, predict to a new dataframe with 
# a suitable range for % burned area values
range(burned)
newdat <- data.frame(burned=seq(0, 60, length.out = 40))
pred.lam <- predict(m2, type="state", newdata=newdat, appendData = TRUE)

# For detection, predict to a new dataframe with 
# a suitable range for time of day values
range(time)
newdat <- data.frame(time=seq(7, 12, length.out = 20))
pred.det <- predict(m2, type="det", newdata=newdat, appendData = TRUE)

# plot abundance relationship with percent burned area
min(pred.lam$lower)
max(pred.lam$upper)
op <- par(mar = c(5,5,4,2) + 0.1) # default is 5,4,4,2
plot(x = pred.lam$burned, y = pred.lam$Predicted, pch=16, 
     ylab = "Salamander Abundance",
     xlab = "% Burned Area", cex.lab=1.5, cex.axis=1.2, col="darkgray", ylim=c(0,65))
box(lwd = 4, col = 'black')
lines(pred.lam$burned, pred.lam$Predicted, lwd=8, col="blue")
lines(pred.lam$burned, pred.lam$lower, lwd=4, lty=2, col="black")
lines(pred.lam$burned, pred.lam$upper, lwd=4, lty=2, col="black")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# QUESTION: What is the mean predicted abundance when percent burned area is 12%
# Hint: you may need to use code from earlier in the script

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# plot detection relationship with time of day
min(pred.det$lower)
max(pred.det$upper)
op <- par(mar = c(5,5,4,2) + 0.1) # default is 5,4,4,2
plot(x = pred.det$time, y = pred.det$Predicted, pch=16, 
     ylab = "Detection Probability",
     xlab = "Time of Day (24 hr clock)", cex.lab=1.5, cex.axis=1.2, 
     col="darkgray", ylim=c(0.45,1))
box(lwd = 4, col = 'black')
lines(pred.det$time, pred.det$Predicted, lwd=8, col="blue")
lines(pred.det$time, pred.det$lower, lwd=4, lty=2, col="black")
lines(pred.det$time, pred.det$upper, lwd=4, lty=2, col="black")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# QUESTION: What is the mean predicted detection for surveys conducted at 10am?
# Hint: you may need to use code from earlier in the script

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Extract abundance at each transect
ranef(m1)
ranef(m2)
# Site-specific abundance is a random effect (Ni)
# ranef() function obtains estimates of these random effects via
# applying Bayes' rule for a conditional estimate of abundance (Ni)
# given the model parameters and the observed count data (i.e., the function
# obtains the best unbiased prediction [BUP] of the random effects based on 
# the posterior distribution of Ni)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# ---- 6) Goodness-of-fit -----
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

# goodness-of-fit test
require(AICcmodavg)

m1.gof <- Nmix.gof.test(m2, nsim = 100) # nsim is 100 only for illustrative purposes,
# you should probably run at least 1000 bootstrap replicates in your analysis

# p-value here suggests that we fail to reject the null (CANNOT conclude that
# the observed data are statistically different from the expected values)

# if p-value was less than alpha (e.g., 0.05), by contrast, then we would conclude that
# the observed data are statistically different from the expected values (lack of fit)


# We can choose alternate models for abundance other than the Poisson distribution
# Negative Binomial 
m3 <- pcount(~time ~burned, data=umf, mixture="NB", K=130)

# Zero-inflated Poisson
m4 <- pcount(~time ~burned, data=umf, mixture="ZIP", K=130)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Compare models using AIC
cbind(AIC.P=m2@AIC, AIC.NB=m3@AIC, AIC.ZIP=m4@AIC)
# QUESTION: Poisson has lowest AIC. Why might this be? Discuss with the group.
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# ---- 7) Produce map of expected abundance -----
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

# Predict back to landscape
library(raster)
# library(rgdal)

# NOTE in the code below: 
# When we predict to the raster surface (in the below code), we are assuming that the
# area of each raster cell is equivalent to the area surveyed at each transect.
# Each raster cell is a 30 meter by 30 meter square, so we are assuming that area
# was equivalent to the area surveyed at each transect. This is just for illustrative
# purposes, as it is unlikely that our transect area matches the raster cell size.
# So in practice, we should make sure that, when predicting from the model,
# that we are predicting the number of individuals to the correct per unit area.
# One way to accomplish this would be to include the size/area of our sampling
# area by adding an offset term to the right-hand side of the abundance formula (as shown below).
# This will then estimate the number of individuals per unit area (density).
# Then, when predicting back to the landscape, you can predict the number of individuals
# per unit area, in other words words the number of individuals in each raster cell (30 m x 30 m)

# For example: 
# m2 <- pcount(~time ~burned + offset(log(TransectLength)), data=umf, K=130) or
# m2 <- pcount(~time ~burned + offset(log(Area)), data=umf, K=130)

# In the two example models above, you would simply code/include 'TransectLength'
# or 'Area' in your unmarked dataframe as a site-level covariate.

# Load the burned severity raster of our study area
fire <- raster("January_23/data/BurnSeverity.tif")


mapPalette <- colorRampPalette(c("grey", "yellow", "orange", "red"))
plot(fire, col = mapPalette(100), axes = F, box = F, main = "% Burned Area")
res(fire) # 30 m x 30 m resolution (grid size)
# crs(fire)
CH <- as.data.frame(fire, xy=TRUE)
CH <- data.frame(x = CH$x, y = CH$y, burned = CH$BurnSeverity)

# Get predictions of abundance for each 30 m x 30 m cell of study area
newData <- data.frame(burned = CH$burned) # would need sampling area in this data frame (see note above)!!!
predCH <- predict(m2, type="state", newdata=newData)

# Define new data frame with coordinates and outcome to be plotted
PARAM <- data.frame(x = CH$x, y = CH$y, z = predCH$Predicted)
r1 <- rasterFromXYZ(PARAM)     # convert into raster object

# Plot predicted salamander abundance in study area (based on model m2)
par(mfrow = c(1,1), mar = c(1,2,2,5))
mapPalette <- colorRampPalette(c("grey", "yellow", "orange", "red"))
plot(r1, col = mapPalette(100), axes = F, box = F, main = "Salamander abundance (mean predicted values)")

# Plot predicted salamander abundance in study area (based on model m2)
par(mfrow = c(1,2))
mapPalette <- colorRampPalette(c("grey", "yellow", "orange", "red"))
plot(r1, col = mapPalette(100), axes = F, box = F, main = "Salamander abundance")
plot(fire, col = mapPalette(100), axes = F, box = F, main = "% Burned Area")







# Now let's predict back to the landscape, accounting for differences in sampling area

# Here are the data for analysis:
C      # matrix of survey data (salamander counts)
burned # percent burned area at each transect (we think it might influence abundance)
time   # time recorded during every survey (we think it might influence detection)

# we need a site-level covariate for the area surveyed at each transect
# let's sample some values between 700m2 and 1000m2
Area <- sample(700:1000, 10, replace = TRUE)
Area <- as.matrix(Area)
Area

# Input data into an 'unmarked data frame'
umf <- unmarkedFramePCount(
  y=C,                                              # Counts matrix
  siteCovs= data.frame(burned = burned, Area=Area), # Site covariates
  obsCovs = list(time = time))                      # Observation covariates

# Explore umf, the input data to unmarked for analysis
# Good point to stop and ensure you're feeding the model what you think you are
head(umf)
summary(umf)

# fit model with offset for Area (total area surveyed at each transect)
m2 <- pcount(~time ~burned + offset(log(Area)), data=umf, K=130)
# One important point to be noted: offset should be included on the same “scale” 
# as the linear predictor. In the case of a log link model, this requires the 
# offset variable to be logged before inclusion in the model.

# Load the burned severity raster of our study area
fire <- raster("January_23/data/BurnSeverity.tif")

# plot raster layer
mapPalette <- colorRampPalette(c("grey", "yellow", "orange", "red"))
par(mfrow = c(1,1), mar = c(1,2,2,5))
plot(fire, col = mapPalette(100), axes = F, box = F, main = "% Burned Area")
res(fire) # 30 m x 30 m resolution (grid size)
# crs(fire)
CH <- as.data.frame(fire, xy=TRUE)
CH <- data.frame(x = CH$x, y = CH$y, burned = CH$BurnSeverity)

# Get predictions of abundance for each 30 m x 30 m cell of study area
newData <- data.frame(burned = CH$burned)
newData$Area <- 900 # because each raster cell in 30 m x 30 m so has area of 900m2
predCH <- predict(m2, type="state", newdata=newData)

# Define new data frame with coordinates and outcome to be plotted
PARAM <- data.frame(x = CH$x, y = CH$y, z = predCH$Predicted)
r1 <- rasterFromXYZ(PARAM)     # convert into raster object

# Plot predicted salamander abundance in study area (based on model m2)
par(mfrow = c(1,1), mar = c(1,2,2,5))
mapPalette <- colorRampPalette(c("grey", "yellow", "orange", "red"))
plot(r1, col = mapPalette(100), axes = F, box = F, main = "Salamander abundance (mean predicted values)")

# Plot predicted salamander abundance in study area (based on model m2)
par(mfrow = c(1,2))
mapPalette <- colorRampPalette(c("grey", "yellow", "orange", "red"))
plot(r1, col = mapPalette(100), axes = F, box = F, main = "Salamander abundance")
plot(fire, col = mapPalette(100), axes = F, box = F, main = "% Burned Area")



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# END
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

