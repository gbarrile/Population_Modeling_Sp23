



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# POPULATION MODELING IN ECOLOGY
# ZOO 5890-09
# University of Wyoming
# Spring 2023

# Week 4 - Closed Occupancy model
# (aka Static Occupancy Model)
# (aka Single-Season Occupancy Model)
# (aka Site-Occupancy Model)

# Description: Steps thru an example analysis with detection/nondetection data
# using the occupancy model of MacKenzie et al. 2002 as 
# implemented in the package `unmarked`.

# MacKenzie, D. I., J. D. Nichols, G. B. Lachman, S. Droege, J. Andrew Royle,
# and C. A. Langtimm. 2002. Estimating Site Occupancy Rates When Detection 
# Probabilities Are Less Than One. Ecology 83: 2248-2255.

# Addressing the question: 
# "How does forest cover and prey abundance influence the occurrence 
#  of brown tree snakes on pacific islands?

# Gabe Barrile - University of Wyoming
# Last updated 02/13/2023
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# ---- Outline -----
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# This script contains the following sections:
# 1) Install and load packages
# 2) Read-in input data
# 3) Format input data for unmarked
# 4) Fit Closed (Static) Occupancy models
# 5) Prediction and plotting
# 6) Goodness-of-fit
# 7) Try a random effects model
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

# Specify the path that contains the salamander count data (Salamander_Wildfire.csv) 
# use setwd()

# you will need to set working directory to where you saved the 'BrownTreeSnake_ForestCover.csv'
setwd()

# read-in the brown tree snake data 
df <- read.csv("February_13/data/BrownTreeSnake_IslandSurveys.csv")

# We now have our data stored as 'df'
# check out our data
head(df)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# QUESTION: how many islands did we survey?
n_distinct(df$Island)

# QUESTION: how many surveys per island?
df %>% 
  group_by(Island) %>% 
  summarise(n=n()) %>% 
  rename(Number.of.Surveys=n) %>% 
  data.frame()
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# ---- 3) Format data for unmarked -----
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

# order the data frame by island, then survey
df <- df %>% arrange(Island, Survey)
df


# create matrix of detections/nondetections, with each site (island) as a row
# in other words, create detection histories for each island

# reduce dataframe to columns that we need
m <- df %>% select(Island, Survey, BTS)
# make Island a factor variable
m$Island <- as.factor(as.character(m$Island))
# order the data frame by Survey
m <- m %>% arrange(Survey)
# pivot data from long format to wide format
y <- m %>% pivot_wider(id_cols = Island, names_from = Survey, values_from = BTS, values_fill = NA)
y

# unmarked only wants a matrix of detections
C <- as.matrix(y[,2:ncol(y)])

C # each island is a row (24 rows)
# each column indicates the survey at each island (six surveys at each island)

# Quick aside, and let's take island 2 as an example. Because we detected brown tree snakes
# at island 2 (during surveys 2, 3, 4, and 6), we assume that brown tree snakes were
# present on island 2 during all the other surveys -- it's just that we did not detect them
# (i.e., brown tree snakes were on island 2 during surveys 1 and 5, we just
# did not detect them during those surveys)

# Also note that detection/nondetection data at each island is sort of 
# analogous to our capture histories when we format our mark-recapture data.
# However, rather than individuals, here we deal with sites


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# QUESTION: Do our detection histories match our field data?
# let's look at islands 1 and 2
head(df, 12)
# Does the detection history match?
C[1:2,]
# Looks good
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~




# Next, format covariates for occupancy and detection probability
# hypothesized that forest cover and prey abundance influence occupancy
# hypothesized that temperature influences detection probability


# Refresher:
# Site covariates versus Observation covariates

# Site covariates do not change during each survey at a given site
# i.e., one value for a site covariate at each island
# e.g., twelve values for forest cover, one value for each island 

# Observation covariates can change during each survey at a given site
# Observation covariate data should match survey data 
# (i.e., matrix of detection/nondetection data)
# We can have a different detection probability for each survey
# e.g., different temperature for each survey.


# Format forest cover as a covariate on occupancy (as a 'site' covariate)
forest <- unique(df[,c("Island","ForestCover")])
forest <- as.matrix(forest[,"ForestCover"])
forest

# Format prey abundance as a covariate on occupancy (as a 'site' covariate)
prey <- unique(df[,c("Island","Prey")])
prey <- as.matrix(prey[,"Prey"])
prey

# Format region as a site-level covariate (we might use it as a random effect)
df$Region <- as.factor(as.character(df$Region))
region <- unique(df[,c("Island","Region")])
region <- as.matrix(region[,"Region"])
region


# Format temperature as a covariate on detection (as an 'observation' covariate)

# reduce dataframe to columns that we need
m <- df %>% select(Island, Survey, Temperature)
# make Island a factor variable
m$Island <- as.factor(as.character(m$Island))
# order the data frame by Survey
m <- m %>% arrange(Survey)
# pivot data from long format to wide format
y <- m %>% pivot_wider(id_cols = Island, names_from = Survey, values_from = Temperature, values_fill = NA)
# check out resulting dataframe
y

# unmarked only wants a matrix of temperatures
temp <- as.matrix(y[,2:ncol(y)])

# remove objects that we don't need
rm(m,y)


# Here are the data for analysis:
C      # matrix of survey data (detection/nondetection data of brown tree snakes)
forest # percent forest cover on each island (we think it might influence occupancy)
prey   # prey abundance on each island (we think it might influence occupancy)
temp   # temp (in Celsius) recorded during every survey (we think it might influence detection)
region # we might try as a random effect

# important to remember that we can also include site covariates on detection

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Let's examine the functions within 'unmarked' that we will use
?occu # this function fits the model (scroll down & read 'Details' section on your own time)
?unmarkedFrameOccu # this function organizes the data for analysis
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Input data into an 'unmarked data frame'
umf <- unmarkedFrameOccu(
  y=C,                                   # detection/nondetection data
  siteCovs= data.frame(forest = forest, prey = prey, region = region), # Site covariates
  obsCovs = list(temp = temp))           # Observation covariates

# look at the summary our dataframe
umf
summary(umf)

plot(umf, panels=4)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# ---- 4) Fit Closed (Static) Occupancy models -----
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

# Fit Single-Season Occupancy Models

# linear model for p (detection) follows first tilde, 
# then comes linear model for occupancy 
m1 <- occu(~ 1 ~ 1, data=umf)

m2 <- occu(~ temp ~ 1, data=umf)

m3 <- occu(~ 1 ~ forest, data=umf)

m4 <- occu(~ 1 ~ prey, data=umf)

m5 <- occu(~ temp ~ forest, data=umf)

m6 <- occu(~ temp ~ prey, data=umf)

m7 <- occu(~ temp ~ forest * prey, data=umf)

m8 <- occu(~ temp + I(temp^2) ~ forest * prey, data=umf)

# combine models, then conduct model selection
fms <- fitList("m1"=m1,
               "m2"=m2,
               "m3"=m3,
               "m4"=m4,
               "m5"=m5,
               "m6"=m6,
               "m7"=m7,
               "m8"=m8)

# model selection table
(ms <- modSel(fms))

# proceed with m7

# check out summary of the model with the most support using AIC for model selection
summary(m7)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# ---- 5) Prediction and plotting -----
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

# Visualize covariate relationships

# For detection, predict to a new dataframe with 
# a suitable range for temperature values
range(temp)
newdat <- data.frame(temp=seq(21, 40, length.out = 40))
pred.det <- predict(m7, type="det", newdata=newdat, appendData = TRUE)

# plot detection relationship with temperature
min(pred.det$lower)
max(pred.det$upper)
op <- par(mar = c(5,5,4,2) + 0.1) # default is 5,4,4,2
plot(x = pred.det$temp, y = pred.det$Predicted, pch=16, 
     ylab = "Detection Probability",
     xlab = "Air Temperature (°C)", cex.lab=1.5, cex.axis=1.2, 
     col="darkgray", ylim=c(0,1))
box(lwd = 4, col = 'black')
lines(pred.det$temp, pred.det$Predicted, lwd=8, col="blue")
lines(pred.det$temp, pred.det$lower, lwd=4, lty=2, col="black")
lines(pred.det$temp, pred.det$upper, lwd=4, lty=2, col="black")




# For occupancy, set up plot to visualize interaction between 
# forest cover and prey abundance

# create sequence of covariate values for forest cover
range(forest)
xvar <- seq(0,95,length.out=40)

# create sequence of covariate values for prey abundance
range(prey)
yvar <- seq(5,94,length.out=40)

# expand xvar and yvar to all combinations of these values
newdat <- expand.grid(forest=xvar, prey=yvar)

# predict to the new dataframe
pred.occ <- predict(m7, type="state", newdata=newdat, appendData = TRUE)

# plot the values (you may need the package viridis to run this code)
library(viridis)
library(ggplot2)

ggplot(pred.occ, aes(forest, prey, fill= Predicted)) + 
  geom_tile() +
  scale_fill_viridis(discrete=FALSE) +
  guides(fill = guide_colourbar(title = "Probability \nof occupancy"))+
  xlab("Forest Cover (%)")+
  ylab("Prey Abundance")

# One interpretation of this plot: we would expect brown tree snakes to occur on islands
# with high forest cover, but only if that island also had high prey abundance



# another way to visualize the interaction, if you prefer base r

# like above, create sequence of covariate values for forest cover
range(forest)
xvar <- seq(0,95,length.out=100)

# like above, create sequence of covariate values for prey abundance
range(prey)
yvar <- seq(5,94,length.out=100)

# create a prediction matrix (this code may take a few seconds/minutes)
pred.matrix1 <- array(NA, dim = c(100, 100)) 
for(i in 1:100){
  for(j in 1:100){
    newData1 <- data.frame(forest=xvar[i], prey=yvar[j]) 
    pred <- predict(m7, type="state", newdata=newData1, appendData = TRUE)
    pred.matrix1[i, j] <- pred$Predicted
  }
}


# create plot
par(mfrow = c(1,1), cex.lab = 1.2)
mapPalette <- colorRampPalette(c("grey", "yellow", "orange", "red"))
image(x=xvar, y=yvar, z=pred.matrix1, col = mapPalette(100), 
      xlab = "Forest Cover (%)", ylab = "Prey Abundance")
contour(x=xvar, y=yvar, z=pred.matrix1, add = TRUE, lwd = 1, 
        col = "blue", labcex = 1, method = "edge")
#axis(1, at = seq(min(xvar), max(xvar), by = 0.1))
#axis(2, at = seq(0, 100, by = 10))
box()
title(main = "Expected occupancy probability", font.main = 1)
# points(df$ForestCover, df$Prey, pch="+", cex=1)



# Empirical Bayes estimates of proportion of sites occupied
re <- ranef(m7)
sum(bup(re, stat="mode"))

# how many sites had at least one detection of BTS?
# field <- aggregate(BTS ~ Island, data = df, FUN = max)
# sum(field$BTS)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# ---- 6) Goodness-of-fit -----
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

# Goodness of fit test on detection history frequencies
require(AICcmodavg)
system.time( gof.boot <- mb.gof.test(m7, nsim = 1000) )
gof.boot
# p-value here suggests that we fail to reject the null (CANNOT conclude that
# the observed data are statistically different from the expected values)

# if p-value was less than alpha (e.g., 0.05), by contrast, then we would conclude that
# the observed data are statistically different from the expected values (lack of fit)

# in other words, the observed frequency of the brown tree snake site-level
# detection histories agrees reasonably well with that expected under the 
# AIC-best model 'm7'. We therefore conclude that this model is suitable to
# use for inference and to inspect covariate relationships.


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# ---- 7) Try a random effects model -----
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

# specify random effect just as you would in the lme4 package
mrand <- occu(~ temp ~ forest + (1|region), data=umf)

mrand


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# END
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#


