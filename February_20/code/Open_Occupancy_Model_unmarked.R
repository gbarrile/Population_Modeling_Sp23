


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# POPULATION MODELING IN ECOLOGY
# ZOO 5890-09
# University of Wyoming
# Spring 2023

# Week 5 - Open Occupancy model
# (aka Dynamic Occupancy Model)
# (aka Multi-Season Occupancy Model)

# Description: Steps thru an example analysis of a
# dynamic occupancy model as implemented in the package `unmarked`.

# MacKenzie, D. I., Nichols, J. D., Hines, J. E., Knutson, M. G., & 
# Franklin, A. B. (2003). Estimating site occupancy, colonization, and
# local extinction when a species is detected imperfectly. Ecology, 84(8), 2200-2207. 

# Addressing the question: 
# "How does pack size and prey abundance influence the disease dynamics 
#  of wolves challenged with sarcoptic mange?

# Gabe Barrile - University of Wyoming
# Last updated 02/20/2023
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# ---- Outline -----
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# This script contains the following sections:
# 1) Install and load packages
# 2) Read-in field data
# 3) Format data for unmarked
# 4) Fit Open (Dynamic) Occupancy models
# 5) Prediction and plotting
# 6) Goodness-of-fit
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# ---- 1) Install and load packages -----
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# Check if unmarked and tidyverse are installed.
# If yes, load them.  If not, install, then load.

# unmarked for fitting the Dynamic Occupancy Model
if("unmarked" %in% rownames(installed.packages()) == FALSE) {
  install.packages("unmarked")
}
require(unmarked)

# ggplot2 for plotting
if("ggplot2" %in% rownames(installed.packages()) == FALSE) {
  install.packages("ggplot2")
}
require(ggplot2)

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

# you will need to set working directory to where you saved the 'Wolves_Mange.csv'
setwd()

# read-in field data
df <- read.csv("February_20/data/Wolves_Mange.csv")

# take a peek at the data
head(df)

# how many individuals were sampled?
n_distinct(df$ID) # 26 wolves

# how many samples per wolf?
df %>% 
  group_by(ID) %>% 
  summarise(n=n()) %>% 
  rename(Number.of.Samples=n) %>% 
  data.frame()

# how many samples per year for each individual?
df %>% 
  group_by(ID, Year) %>% 
  summarise(n=n()) %>% 
  rename(Number.of.Samples=n) %>% 
  data.frame()

# So, it seems we have detection/nondetection data for mange on 26 wolves
# and we sampled each wolf four times per year for five years (2011-2015)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# ---- 3) Format data for unmarked -----
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

# first, order the data frame by Site, Primary, Secondary
df <- df %>% arrange(ID, Primary, Secondary)
head(df)



# Now, let's create variable that gives a unique identifier to each primary-secondary survey combinations
# first paste primary and secondary surveys together in new column
df$PrimSec <- paste(df$Primary, df$Secondary, sep = "-")
# order by primary survey (this is not absolutely necessary but I recommend doing it)
df <- df %>% arrange(Primary)
# create new variable with unique identifier for each primary-secondary survey combinations
df <- df %>% group_by(PrimSec) %>% mutate(Total.Survey=cur_group_id())
# remove PrimSec column (make sure you do this)
df$PrimSec <- NULL
# now reorder by Site, Primary, Secondary to see what we did
df <- df %>% arrange(ID, Primary, Secondary)
View(df)

# reduce dataframe to columns that we need
m <- df %>% select(ID, Total.Survey, Mange)
# make ID a factor variable 
m$ID <- as.factor(as.character(m$ID))
# order the data frame by Total.Survey
m <- m %>% arrange(Total.Survey)

# pivot data to wide format
y <- m %>% pivot_wider(names_from = Total.Survey, values_from = Mange, values_fill = NA)
y


# unmarked only wants a matrix of detections/nondetections
C <- as.matrix(y[,2:ncol(y)])
C # each individual constitutes a row (26 rows)
# each column indicates the survey (disease sample) for each wolf (20 samples per wolf)

# let's make sure we pivoted the data correctly
# look at detections for our first wolf between C and df
head(C,1) # first row is wolf 1
head(df, 20) # do they match?



# format covariates for unmarked

# Format whether wolf was male/female as a site-level covariate
female <- unique(df[,c("ID","Female")])
female <- as.matrix(female[,"Female"])
female

# Format region as a site-level covariate
region <- unique(df[,c("ID","Region")])
region <- as.matrix(region[,"Region"])
region


# Format pack size as a yearly-site covariate

# reduce dataframe to columns that we need
m <- df %>% select(ID, Total.Survey, PackSize)
# make ID a factor variable 
m$ID <- as.factor(as.character(m$ID))
# order the data frame by Total.Survey
m <- m %>% arrange(Total.Survey)
# pivot data to wide format
y <- m %>% pivot_wider(names_from = Total.Survey, values_from = PackSize, values_fill = NA)
y

# unmarked only wants a matrix of pack size values
Pack <- as.matrix(y[,2:ncol(y)])
Pack

# switch to data frame
Pack <- as.data.frame(Pack)

# rename columns
df <- df %>% arrange(Primary)
names(Pack) <- unique(paste(df$Primary, df$Secondary, sep = "-"))

Pack

# only want one value of pack size per year at each site
# so subset Pack to include one value for 2011, 2012, 2013, 2014, and 2015 for each wolf
Pack <- Pack[,c("1-1","2-1","3-1","4-1","5-1")]
Pack

# make sure it's still a df
Pack <- as.data.frame(Pack)


# Format elk abundance as a yearly-site covariate

# reduce dataframe to columns that we need
m <- df %>% select(ID, Total.Survey, ElkPop)
# make ID a factor variable 
m$ID <- as.factor(as.character(m$ID))
# order the data frame by Total.Survey
m <- m %>% arrange(Total.Survey)
# pivot data to wide format
y <- m %>% pivot_wider(names_from = Total.Survey, values_from = ElkPop, values_fill = NA)
y

# unmarked only wants a matrix of elk abundance values
Elk <- as.matrix(y[,2:ncol(y)])
Elk

# switch to data frame
Elk <- as.data.frame(Elk)

# rename columns
df <- df %>% arrange(Primary)
names(Elk) <- unique(paste(df$Primary, df$Secondary, sep = "-"))

Elk

# only want one value of elk abundance per year at each site
# so subset Elk to include one value for 2011, 2012, 2013, 2014, and 2015 for each wolf
Elk <- Elk[,c("1-1","2-1","3-1","4-1","5-1")]
Elk

# make sure it's still a df
Elk <- as.data.frame(Elk)




# here are our data for analysis
C
female
region
Pack
Elk

# unmarked wants yearly-site covariates as a list
ysc <- list(pack = as.matrix(Pack),
            elk = as.matrix(Elk))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Let's examine the functions within 'unmarked' that we will use
?colext # this function fits the model (scroll down & read 'Details' section on your own time)
?unmarkedMultFrame # this function organizes the data for analysis
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Create the unmarkedFrame
umf <- unmarkedMultFrame(
  y=C,               # detections matrix
  siteCovs= data.frame(female = female, region = region), # site covariates 
  yearlySiteCovs=ysc, # yearly site covariates
  numPrimary=5)


# Take a look
umf
summary(umf)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# ---- 4) Fit Open (Dynamic) Occupancy models -----
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

# Fit Multi-Season Occupancy Models

# null or 'constant' model
m1 <- colext(psiformula = ~ 1, # initial probability of occupancy
              gammaformula = ~ 1, # colonization probability
              epsilonformula = ~ 1, # extinction probability
              pformula = ~ 1, # detection probability
              data= umf)

# look at model summary
summary(m1)

# what was our detection probability from the null model?
backTransform(m1, type="det") 
confint(backTransform(m1, type="det"))



# fit a second model that tests our hypotheses about colonization and extinction
m2 <- colext(psiformula = ~ Region, # initial probability of occupancy
             gammaformula = ~ pack, # colonization (contracting disease)
             epsilonformula = ~ elk, # extinction (clearing disease)
             pformula = ~ 1, # probability of detecting disease given animal is infected
             data= umf)

# look at model summary
summary(m2)

# what was our detection probability from this model?
backTransform(m2, type="det") 
confint(backTransform(m2, type="det"))
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# ---- 5) Prediction and plotting -----
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

# predictions for initial occupancy
newdata <- data.frame(Region=c("North","South","West")) 
predict(m2, type="psi", newdata=newdata, appendData = TRUE)


# plot covariate relationship with colonization probability and pack size
X <- seq(min(df$PackSize), max(df$PackSize), length.out=40)
newdat <- data.frame(pack = X)
pred <- round(predict(m2, type = "col", newdata = newdat, appendData = TRUE), 3)

# plot relationship between colonization probability and pack size
min(pred$lower)
max(pred$upper)
op <- par(mar = c(5,5,4,2) + 0.1) # default is 5,4,4,2
plot(x = pred$pack, y = pred$Predicted, pch=16, cex=0.1, 
     ylab = "Probability of contracting mange",
     xlab = "Pack Size (# of wolves)", cex.lab=2, cex.axis=1.2, col="darkgray", ylim=c(0,1))
box(lwd = 3, col = 'black')
lines(pred$pack, pred$Predicted, lwd=6, col="blue")
lines(pred$pack, pred$lower, lwd=4, lty=2, col="black")
lines(pred$pack, pred$upper, lwd=4, lty=2, col="black")


# plot covariate relationship with extinction probability and elk abundance
X <- seq(min(df$ElkPop), max(df$ElkPop), length.out=40)
newdat <- data.frame(elk = X)
pred <- round(predict(m2, type = "ext", newdata = newdat, appendData = TRUE), 3)

# plot relationship between extinction probability and elk abundance
min(pred$lower)
max(pred$upper)
op <- par(mar = c(5,5,4,2) + 0.1) # default is 5,4,4,2
plot(x = pred$elk, y = pred$Predicted, pch=16, cex=0.1, 
     ylab = "Probability of clearing mange",
     xlab = "Prey abundance (# of elk)", cex.lab=2, cex.axis=1.2, col="darkgray", ylim=c(0,1))
box(lwd = 3, col = 'black')
lines(pred$elk, pred$Predicted, lwd=6, col="blue")
lines(pred$elk, pred$lower, lwd=4, lty=2, col="black")
lines(pred$elk, pred$upper, lwd=4, lty=2, col="black")


# Empirical Bayes estimates of number of sites occupied in each year
re <- ranef(m2)
modes <- colSums(bup(re, stat="mode"))
plot(2011:2015, modes, xlab="Year", ylab="Predicted number of infected wolves", ylim=c(0, 20), type="b")

# change to 'disease prevalence' by taking number infected out of total number tested 
prev <- (modes/26)*100
plot(2011:2015, prev, xlab="Year", ylab="Expected disease prevalence", ylim=c(20, 80), type="b")

## get the trajectory estimates (occupancy probability over time)
smoothed(m2)
m2@smoothed.mean
# for just one animal (or site)
round(m2@smoothed[2, , 8], digits = 3)

## Find bootstrap standard errors for smoothed trajectory
m2 <- nonparboot(m2, B = 10)  # should increase B
m2@smoothed.mean.bsse

# plot predicted occupancy probability over the study period
predicted_occupancy <- data.frame(year = c(2011:2015),
                                  smoothed_occ = smoothed(m2)[2,],
                                  SE = m2@smoothed.mean.bsse[2,])

# first plot
ggplot(predicted_occupancy, 
       aes(x = year, y = smoothed_occ)) +
  geom_errorbar(aes(ymin = smoothed_occ-SE,
                    ymax = smoothed_occ+SE),
                width = 0) +
  geom_point(size = 3) +
  geom_line() +
  scale_y_continuous(limits = c(0.2,0.8),
                     breaks = c(0.2, 0.4, 0.6, 0.8)) +
  labs(x = "Year", y = "Occupancy probability") +
  theme_classic()

# second plot
ggplot(predicted_occupancy, 
       aes(x = year, y = smoothed_occ)) +
  geom_errorbar(aes(ymin = smoothed_occ-SE,
                    ymax = smoothed_occ+SE),
                width = 0) +
  geom_point(size = 3) +
  geom_line() +
  scale_y_continuous(limits = c(0.2,0.8),
                     breaks = c(0.2, 0.4, 0.6, 0.8)) +
  labs(x = "Year", y = "Probability of infection with mange") +
  theme_classic()

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# ---- 6) Goodness-of-fit -----
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#


# Mackenzie-Bailey GOF test
# Simulates detection history data (if model correct). Compare obs X2 to sim X2
# Likely to take a Very Long Time on large data sets
mb.boot <- AICcmodavg::mb.gof.test(m2, nsim = 100) # nsim should probably be at least 1000
print(mb.boot, digit.vals = 4, digits.chisq = 4)

# p-value here suggests that we fail to reject the null (CANNOT conclude that
# the observed data are statistically different from the expected values)

# if p-value was less than alpha (e.g., 0.05), by contrast, then we would conclude that
# the observed data are statistically different from the expected values (lack of fit)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# END
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#


