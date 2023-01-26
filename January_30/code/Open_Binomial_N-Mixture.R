


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# POPULATION MODELING IN ECOLOGY
# ZOO 5890-09
# University of Wyoming
# Spring 2023

# Week 2 - Open Binomial N-mixture model

# Description: code to fit 'Open Population N-mixture Model' in the 'unmarked' package

# Refer to:
# Hostetler, J.A., & Chandler, R.B. (2015). Improved state‐space models for inference
# about spatial and temporal variation in abundance from count data. Ecology, 96(6), 1713-1723.

# Addressing the question: "How do annual rainfall patterns influence 
# temporal trends in Indian roller abundance??"

# Gabe Barrile - University of Wyoming
# Last updated 01/30/2023
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# ---- Outline -----
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# This script contains the following sections:
# 1) Install and load packages
# 2) Read-in input data
# 3) Format input data for unmarked
# 4) Fit Open Binomial N-mixture model
# 5) Prediction and plotting
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

# ggplot2 for plotting
if("ggplot2" %in% rownames(installed.packages()) == FALSE) {
  install.packages("ggplot2")
}
require(ggplot2)

# lattice for data exploration
if("lattice" %in% rownames(installed.packages()) == FALSE) {
  install.packages("lattice")
}
require(lattice)

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

# you will need to set your working directory to wherever you saved the IndianRoller_Rainfall.csv 
setwd()

#  read-in data from the csv
df <- read.csv("January_30/data/IndianRoller_Rainfall.csv")

# take a peek at the data
head(df)

# how many sites were surveyed?
n_distinct(df$Site) # 14 sites

# how many surveys per site?
df %>% 
  group_by(Site) %>% 
  summarise(n=n()) %>% 
  rename(Number.of.Surveys=n) %>% 
  data.frame()

# how many surveys per year at each site?
df %>% 
  group_by(Site, Year) %>% 
  summarise(n=n()) %>% 
  rename(Number.of.Surveys=n) %>% 
  data.frame()

# order the data frame by Site, Primary, Secondary
df <- df %>% arrange(Site, Primary, Secondary)
head(df)

# We have repeated count data with 4 primary periods (years), 
# each with 3 secondary sampling periods (surveys within a year)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# ---- 3) Format data for unmarked -----
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

# let's explore the data a bit

# look at histogram of counts
hist(df$Count, col = "grey", 
     main = "Counts of Indian Rollers", xlab = "", breaks=10)
abline(v = mean(df$Count), lwd = 3, col = "black")

# Plot counts over time for each site
xyplot(Count ~ Year | Site, data=df)



# create variable that gives a unique identifier to each primary-secondary survey combinations
# first paste primary and secondary surveys together in new column
df$PrimSec <- paste(df$Primary, df$Secondary, sep = "-")
# order by primary survey (this is not absolutely necessary but I recommend doing it)
df <- df %>% arrange(Primary)
# create new variable with unique identifier for each primary-secondary survey combinations
df <- df %>% group_by(PrimSec) %>% mutate(Total.Survey=cur_group_id())
View(df)
# remove PrimSec column (make sure you do this)
df$PrimSec <- NULL
# now reorder by Site, Primary, Secondary to see what we did
df <- df %>% arrange(Site, Primary, Secondary)
head(df, 12)

# reduce dataframe to columns that we need
m <- df %>% select(Site, Total.Survey, Count)
# make Site a factor variable 
m$Site <- as.factor(as.character(m$Site))
# order the data frame by Total.Survey
m <- m %>% arrange(Total.Survey)

# pivot data to wide format
y <- m %>% pivot_wider(names_from = Total.Survey, values_from = Count, values_fill = NA)
y


# unmarked only wants a matrix of counts
C <- as.matrix(y[,2:ncol(y)])
C # each site constitutes a row (14 rows)
# each column indicates the survey at each site (12 surveys at each site)

# let's make sure we pivoted the data correctly
# look at counts for site 1 between C and df
head(C,1) # first row is site 1
head(df, 12) # do they match?

# look at counts for site 14 between C and df
tail(C,1) # last row is site 14
tail(df, 12) # do they match?



# Site-specific covariates (should equal number of sites)
# Format 'Agriculture' as a site covariate
sc <- unique(df[,c("Site","Agriculture")])
sc <- data.frame(Agriculture = sc$Agriculture)
sc

# check out histogram of values for our site-level covariate
hist(sc$Agriculture, col = "grey", 
     main = "Percent agricultural land at our sites", xlab = "", breaks=10)
abline(v = mean(sc$Agriculture), lwd = 3, col = "black")


# Plot values for agriculture over time for each site
xyplot(Agriculture ~ Year | Site, data=df)
# the plot shows why agriculture is a site-level covariate, as the value for
# agriculture does not vary over time at a given site



# Format 'GrassCover' as an observation covariate

# check out histogram of values for our observation-level covariate
hist(df$GrassCover, col = "grey", 
     main = "Grass cover during our surveys", xlab = "", breaks=10)
abline(v = mean(df$GrassCover), lwd = 3, col = "black")


# Plot grass cover over time for each site
xyplot(GrassCover ~ Year | Site, data=df)
# the plot shows why grass cover is an observation-level covariate, as the value for
# grass cover can vary over time at a given site, even within a given year


# reduce dataframe to columns that we need
m <- df %>% select(Site, Total.Survey, GrassCover)
# make Site a factor variable 
m$Site <- as.factor(as.character(m$Site))
# order the data frame by Total.Survey
m <- m %>% arrange(Total.Survey)

# pivot data to wide format
y <- m %>% pivot_wider(names_from = Total.Survey, values_from = GrassCover, values_fill = NA)
y


# unmarked only wants a matrix of grass cover values
GrassCover <- as.matrix(y[,2:ncol(y)])
GrassCover # each site constitutes a row (14 rows)
# each column indicates the survey at each site (12 surveys at each site)

# If you wanted to mean-impute for NA values
#GrassCover[is.na(GrassCover)] <- mean(GrassCover, na.rm = TRUE)

# unmarked wants observation covariate as a list
oc <- list(grasscover = as.matrix(GrassCover))
oc





# Format 'Rain' as a yearly-site covariate

# check out histogram of values for our yearly-site covariate
hist(df$Rain, col = "grey", 
     main = "Annual rainfall (mm)", xlab = "")
abline(v = mean(df$Rain), lwd = 3, col = "black")

# Plot annual rainfall over time for each site
xyplot(Rain ~ Year | Site, data=df)
# the plot shows why rainfall is an yearly-site covariate, as the value for
# rainfall can vary over time at a given site, but not within a given year (i.e., primary period)


# reduce dataframe to columns that we need
m <- df %>% select(Site, Total.Survey, Rain)
# make Site a factor variable 
m$Site <- as.factor(as.character(m$Site))
# order the data frame by Total.Survey
m <- m %>% arrange(Total.Survey)

# pivot data to wide format
y <- m %>% pivot_wider(names_from = Total.Survey, values_from = Rain, values_fill = NA)
y


# unmarked only wants a matrix of rainfall values
Rain <- as.matrix(y[,2:ncol(y)])
Rain # each site constitutes a row (14 rows)
# each column indicates the survey at each site (12 surveys at each site)

# switch to data frame
Rain <- as.data.frame(Rain)

# rename columns
df <- df %>% arrange(Primary)
names(Rain) <- unique(paste(df$Primary, df$Secondary, sep = "-"))


# rename columns to reflect surveys within each year


Rain

# only want one value of rainfall per year at each site
# so subset Rain to include one value for 2015, 2016, 2017, and 2018 at each site
Rain <- Rain[,c("1-1","2-1","3-1","4-1")]
Rain

# make sure it's still a df
Rain <- as.data.frame(Rain)

# unmarked wants yearly-site covariates as a list
ysc <- list(rain = as.matrix(Rain))
ysc


# Primary periods of surveys
primaryPeriod2 <- matrix(as.integer(c(
  1,2,3,4,
  1,2,3,4,
  1,2,3,4,
  1,2,3,4,
  1,2,3,4,
  1,2,3,4,
  1,2,3,4,
  1,2,3,4,
  1,2,3,4,
  1,2,3,4,
  1,2,3,4,
  1,2,3,4,
  1,2,3,4,
  1,2,3,4)), nrow=14, ncol=4, byrow=TRUE) # nrow should equal number of sites, 
  # and ncol should equal number of primary periods (years in this case)

primaryPeriod2 # each row is a site
# each column is a year, denoting time elapsed in the study
# 2015 is the first year, so it gets a 1, 2016 is the second year, so it gets a two, and so on
# however, if the final survey at site 1 was in 2019 and not 2018 (2018 was skipped), the row would
# read 1, 2, 3, 5 to account for the missing year


# Here are the data for analysis:
C      # matrix of survey data (Indian roller counts)
sc     # site covariate (we think 'Agriculture' might influence initial abundance)
oc     # observation covariate (we think 'grass cover' might influence detection)
ysc    # yearly site covariate (we think 'rainfall' may influence trends/changes in abundance,
       # in our case lambda which is the finite rate of increase)
primaryPeriod2



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Let's examine the functions within 'unmarked' that we will use
?pcountOpen # this function fits the model (scroll down & read 'Details' section on your own time)
?unmarkedFramePCO # this function organizes the data for analysis
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Create the unmarkedFrame
umf <- unmarkedFramePCO(
                         y=C,               # Counts matrix
                         siteCovs=sc,       # site covariates 
                         obsCovs=oc,        # observation covariates
                         yearlySiteCovs=ysc,# yearly site covariates
                         numPrimary=4,       
                         primaryPeriod=primaryPeriod2)

# Take a look
umf
summary(umf)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# ---- 4) Fit Open Binomial N-mixture model -----
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

# Here are the meanings of each parameter within the model-fitting function:

# lambdaformula = initial abundance

# gammaformula = recruitment rate (when dynamics is "constant", "autoreg", or "notrend") 
#                OR population growth rate (when dynamics is "trend", "ricker", or "gompertz")

# omegaformula = apparent survival probability (when dynamics is "constant", "autoreg", or "notrend") 
#                OR equilibrium abundance (i.e. carrying capacity) (when dynamics is "ricker" or "gompertz")

# pformula = detection probability

# data = An object of class unmarkedFramePCO.

# mixture = character specifying mixture: "P", "NB", or "ZIP" 
            # for the Poisson, negative binomial, and zero-inflated Poisson distributions.

# K = Integer defining upper bound of discrete integration. 
      #This should be higher than the maximum observed count and high enough 
      #that it does not affect the parameter estimates. However, the higher 
      #the value the slower the compuatation.

# dynamics = see help page

# fix = If "omega", omega is fixed at 1. If "gamma", gamma is fixed at 0.

# starts = vector of starting values

# immigration = logical specifying whether or not to include an 
                # immigration term (iota) in population dynamics.

# iotaformula = Right-hand sided formula for average number of immigrants to a site per time step


# Let's fit the model
(m1 <- pcountOpen(
                  lambdaformula = ~ Agriculture, 
                  gammaformula = ~ rain, 
                  omegaformula = ~ 1, 
                  pformula = ~ grasscover, 
                  data = umf, 
                  mixture = "P", 
                  K=200,
                  dynamics = "trend")) 

summary(m1)



# try grasscover as a quadratic term
# (m1 <- pcountOpen(
#   lambdaformula = ~ Agriculture, 
#   gammaformula = ~ rain, 
#   omegaformula = ~ 1, 
#   pformula = ~ grasscover + I(grasscover^2), 
#   data = umf, 
#   mixture = "P", 
#   K=200,
#   dynamics = "trend")) 
# 
# summary(m1)



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# ---- 5) Prediction and plotting -----
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

# Predictions of growth rate at specified values of rainfall, say 300, 600, and 900)
newdat <- data.frame(rain=c(300, 600, 900))
predict(m1, type="gamma", newdata=newdat, append = T)
# Lambda = 1 (population stable)
# Lambda < 1 (population decreasing)
# Lambda > 1 (population increasing)

# Predictions of initial abundance at specified values of Agriculture, say 0, 50, and 100)
newdat <- data.frame(Agriculture=c(0, 50, 100))
predict(m1, type="lambda", newdata=newdat, append = T)

# Predictions of detection probability at specified values of grasscover, say 0, 20, and 40)
newdat <- data.frame(grasscover=c(0, 20, 40))
predict(m1, type="det", newdata=newdat, append = T)


# Visualize covariate relationships

# For lambda, predict to a new dataframe with 
# a suitable range for rainfall values
X <- seq(min(ysc$rain), max(ysc$rain), length.out=40)
newdat <- data.frame(rain = X)
pred <- round(predict(m1, type = "gamma", newdata = newdat, appendData = TRUE), 3)

# plot lambda relationship with annual rainfall patterns
min(pred$lower)
max(pred$upper)
op <- par(mar = c(5,5,4,2) + 0.1) # default is 5,4,4,2
plot(x = pred$rain, y = pred$Predicted, pch=16, cex=0.1, 
     ylab = expression(paste("Pop Growth Rate","  ", "(", lambda, ")")),
     xlab = "Annual Rainfall (mm)", cex.lab=2, cex.axis=1.2, col="darkgray", ylim=c(0.6,1.4))
box(lwd = 3, col = 'black')
abline(h = 1, lwd=2, lty=3, col="darkgrey")
lines(pred$rain, pred$Predicted, lwd=6, col="blue")
lines(pred$rain, pred$lower, lwd=4, lty=2, col="black")
lines(pred$rain, pred$upper, lwd=4, lty=2, col="black")


# For detection, predict to a new dataframe with 
# a suitable range for grass cover values
X <- seq(min(oc$grasscover), max(oc$grasscover), length.out=40)
newdat <- data.frame(grasscover = X)
pred <- round(predict(m1, type = "det", newdata = newdat, appendData = TRUE), 3)

# plot detection relationship with grass cover
min(pred$lower)
max(pred$upper)
op <- par(mar = c(5,5,4,2) + 0.1) # default is 5,4,4,2
plot(x = pred$grasscover, y = pred$Predicted, pch=16, cex=0.1, 
     ylab = "Detection Probability",
     xlab = "% Grass Cover", cex.lab=2, cex.axis=1.2, col="darkgray", ylim=c(0.4,0.8))
box(lwd = 3, col = 'black')
lines(pred$grasscover, pred$Predicted, lwd=6, col="blue")
lines(pred$grasscover, pred$lower, lwd=4, lty=2, col="black")
lines(pred$grasscover, pred$upper, lwd=4, lty=2, col="black")

# For initial abundance, predict to a new dataframe with 
# a suitable range for Agriculture values
X <- seq(min(sc$Agriculture), max(sc$Agriculture), length.out=40)
newdat <- data.frame(Agriculture = X)
pred <- round(predict(m1, type = "lambda", newdata = newdat, appendData = TRUE), 3)

# plot initial abundance relationship with agriculture
min(pred$lower)
max(pred$upper)
op <- par(mar = c(5,5,4,2) + 0.1) # default is 5,4,4,2
plot(x = pred$Agriculture, y = pred$Predicted, pch=16, cex=0.1, 
     ylab = "Initial Abundance",
     xlab = "% Agricultural Land", cex.lab=2, cex.axis=1.2, col="darkgray", ylim=c(0,200))
box(lwd = 3, col = 'black')
lines(pred$Agriculture, pred$Predicted, lwd=6, col="blue")
lines(pred$Agriculture, pred$lower, lwd=4, lty=2, col="black")
lines(pred$Agriculture, pred$upper, lwd=4, lty=2, col="black")


# Estimates of conditional abundance distribution at each site
(re <- ranef(m1))
# Best Unbiased Predictors
bup(re, stat="mean")           # Posterior mean
bup(re, stat="mode")           # Posterior mode
confint(re, level=0.9) # 90% CI

# Plots of abundance over time at a given site (probability distribution of abundances)
plot(re, subset=site %in% c(1), xlim=c(0,150))
plot(re, subset=site %in% c(7), xlim=c(0,150))
# look at both sites together
plot(re, subset=site %in% c(1,7), xlim=c(0,150))


# We can customize our own plots
# Let's plot mean abundance through time at a subset of our sites
pop <- as.data.frame(bup(re, stat="mean")) 
colnames(pop) # poor naming, let's rename columns
names(pop) <- c("2015","2016","2017","2018")
pop$Site <- 1:14
pop$Site <- as.factor(as.character(pop$Site))

# check out df
pop

# add values for agriculture at each site to the pop df
pop$Agriculture <- df$Agriculture[match(pop$Site, df$Site)]

# pivot to long format for plotting (could use tidy/dplyr functions instead)
pop15 <- data.frame(Site = pop$Site, Year = "2015", Abundance = pop$`2015`)
pop16 <- data.frame(Site = pop$Site, Year = "2016", Abundance = pop$`2016`)
pop17 <- data.frame(Site = pop$Site, Year = "2017", Abundance = pop$`2017`)
pop18 <- data.frame(Site = pop$Site, Year = "2018", Abundance = pop$`2018`)
# combine dfs
pop2 <- rbind(pop15, pop16, pop17, pop18)

# check out new df (it's a long format of pop)
pop2

# add percent agriculture values to pop2
pop2$Agriculture <- df$Agriculture[match(pop2$Site, df$Site)]

# let's only plot a few of our sites, perhaps with varying levels of percent agriculture
# choose three sites
ag1 <- pop2[pop2$Site == "3" | pop2$Site == "11" | pop2$Site == "12", ]

# make agriculture a factor, just for plotting
ag1$Agriculture <- as.factor(as.character(ag1$Agriculture))

# relevel the factor for better visualization in the plot
ag1$Agriculture=relevel(ag1$Agriculture,"100")
ag1$Agriculture=relevel(ag1$Agriculture,"50")
ag1$Agriculture=relevel(ag1$Agriculture,"0")

# plot abundance over time at these selected sites
require(ggplot2)

  ggplot(ag1, aes(x=Year, y=Abundance, group=Agriculture, color=Agriculture)) +
  geom_point(size=5) + geom_line(size=1.5) +
  ylab("Site Abundance") +
  scale_y_continuous(limits = c(0, 170), breaks = seq(0, 160, 40), expand = c(0,0)) +
  theme_bw() +
  theme(#text = element_text(size = 18, family = "Times"), # controls all fonts in plot
    panel.background = element_rect(colour = "black", size=1, linetype = "solid"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks.length = unit(0.2,"cm"),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 22, color = "black"),
    axis.text.x = element_text(size = 18, color = "black"),
    axis.text.y = element_text(size = 14, color = "black")) +
  theme(legend.title = element_text(size=17))+
  theme(legend.text = element_text(size = 17))+
  theme(legend.position="top")+
  theme(strip.text = element_text(size = 7))
  
  # recall we learned that percent agriculture likely played a key role in the 
  # initial or baseline abundance at each site, whereas annual rainfall likely
  # drives changes in abundance over time
  
  # Visualize covariate relationships with rainfall again
  
  # For lambda, predict to a new dataframe with 
  # a suitable range for rainfall values
  X <- seq(min(ysc$rain), max(ysc$rain), length.out=40)
  newdat <- data.frame(rain = X)
  pred <- round(predict(m1, type = "gamma", newdata = newdat, appendData = TRUE), 3)
  
  # plot lambda relationship with annual rainfall patterns
  min(pred$lower)
  max(pred$upper)
  op <- par(mar = c(5,5,4,2) + 0.1) # default is 5,4,4,2
  plot(x = pred$rain, y = pred$Predicted, pch=16, cex=0.1, 
       ylab = expression(paste("Pop Growth Rate","  ", "(", lambda, ")")),
       xlab = "Annual Rainfall (mm)", cex.lab=2, cex.axis=1.2, col="darkgray", ylim=c(0.6,1.4))
  box(lwd = 3, col = 'black')
  abline(h = 1, lwd=2, lty=3, col="darkgrey")
  lines(pred$rain, pred$Predicted, lwd=6, col="blue")
  lines(pred$rain, pred$lower, lwd=4, lty=2, col="black")
  lines(pred$rain, pred$upper, lwd=4, lty=2, col="black")
  


# QUESTION: Why might this model be the simplest of all dynamic population models for abundance?


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# END
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#




























