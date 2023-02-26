
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# POPULATION MODELING IN ECOLOGY
# ZOO 5890-09
# University of Wyoming
# Spring 2023

# Week 6 - Open Occupancy model
# (aka Dynamic Occupancy Model)
# (aka Multi-Season Occupancy Model)

# Description: Steps thru an example analysis of a
# dynamic occupancy model as implemented in the package `RMark`.

# Some references:

# Program MARK – a ‘gentle introduction.’ Chapter 21: Occupancy models – single-species, 
# written by Brian Gerber, Daniel Martin, Larissa Bailey, Thierry Chambert, and Brittany Mosher. 
# Go to http://www.phidot.org/software/mark/docs/book/ and select Chapter 21 from the dropdown menu.
# 
# Maphisa, D. H., Smit-Robinson, H., & Altwegg, R. (2019). Dynamic multi-species occupancy models 
# reveal individualistic habitat preferences in a high-altitude grassland bird community. 
# PeerJ, 7, e6276. https://doi.org/10.7717/peerj.6276
# 
# Ruiz‐Gutiérrez, V., Zipkin, E. F., & Dhondt, A. A. (2010). Occupancy dynamics in a tropical
# bird community: unexpectedly high forest use by birds classified as non‐forest species.
# Journal of Applied Ecology, 47(3), 621-630. 


# The code below addresses the question: 
# How does little tern breeding occurrence change over time on beaches in northern Europe?

# Gabe Barrile - University of Wyoming
# Last updated 02/27/2023
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# ---- Outline -----
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# This script contains the following sections:
# 1) Install and load packages
# 2) Read-in field data
# 3) Format data for RMark
# 4) Fit Open (Dynamic) Occupancy models
# 5) Prediction and plotting
# 6) Adding covariates
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# ---- 1) Install and load packages -----
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# Check if unmarked and tidyverse are installed.
# If yes, load them.  If not, install, then load.

# RMark for fitting the Dynamic Occupancy model
if("RMark" %in% rownames(installed.packages()) == FALSE) {
  install.packages("RMark")
}
require(RMark)

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
citation("RMark")
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# ---- 2) Read-in input data -----
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

# you will need to set working directory to where you saved the 'LittleTern_Breeding.csv'
setwd()

# read-in the little tern detection/nondetection data from the beaches that we surveyed 
setwd("G:/Shared drives/wyo-coop-barrile/Boreal_Toad_Project/RS/Population_Modeling_Sp23/Week_6_Dynamic_Occupancy_RMark/data")

# read-in data from the csv
df <- read.csv("LittleTern_Breeding.csv")

# take a peek at the data
head(df)

# how many beaches were sampled?
n_distinct(df$Site) # 8 beaches

# how many surveys per beach?
df %>% 
  group_by(Site) %>% 
  summarise(n=n()) %>% 
  rename(Number.of.Surveys=n) %>% 
  data.frame()

# how many surveys per year at each beach?
df %>% 
  group_by(Site, Year) %>% 
  summarise(n=n()) %>% 
  rename(Number.of.Samples=n) %>% 
  data.frame()

# So, it seems we have detection/nondetection data for little tern eggs
# (denotes breeding activity) at eight beaches and
# we surveyed each beach three times per year for five years (2015-2019)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# ---- 3) Format data for RMark -----
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

# first, order the data frame by Site, Primary, Secondary
df <- df %>% arrange(Site, Primary, Secondary)
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
df <- df %>% arrange(Site, Primary, Secondary)
View(df)

# reduce dataframe to columns that we need
m <- df %>% select(Site, Total.Survey, Eggs)
# make Site a factor variable 
m$Site <- as.factor(as.character(m$Site))
# order the data frame by Total.Survey
m <- m %>% arrange(Total.Survey)

# pivot data to wide format
y <- m %>% pivot_wider(names_from = Total.Survey, values_from = Eggs, values_fill = 0)
y

# create detection history as character string
y <- y %>% unite("ch", c(2:ncol(.)), remove = FALSE, sep = "")

# reduce dataframe to columns that we need
y <- y %>% select(Site, ch) %>% data.frame()
head(y)

# check if all detection histories are length = 15 (for each of our fifteen visits to each beach)
table(nchar(y$ch))

# tern will be our dataframe with which to proceed
tern <- data.frame(ch = y$ch, freq = 1, beach = y$Site)

# make beach a factor variable
tern$beach <- as.factor(as.character(tern$beach))

# remove unneeded objects
rm(m,y)

# are we missing any data?
table(is.na(tern)) # no missing data = good!

# here is our data frame
tern


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Check detection histories against our field data
# Look at our first beach
head(df %>% arrange(Site), 15)
# Does the detection history ('ch' column in tern) make sense for the first beach?
head(tern, 1)
# Stop here and make sure you understand the detection histories (the 'ch' column in tern df)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# ---- 4) Fit Open (Dynamic) Occupancy models in RMark -----
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

# first, process the data for RMark
# input the model that we want in the 'model =' argument of the process.data function

# here are the list of models in RMark
setup.model(model = "") # 91 models

# As you can see, RMark includes lots of models that can be input into the model= argument below
# Here we use the "Closed" model because we are interested in 
# Closed Population Estimation for modeling Abundance
# Visit this link for a full list of MARK models supported in RMark:
# https://github.com/jlaake/RMark/blob/master/RMark/inst/MarkModels.pdf

# we need to specify time intervals for Robust Design
intervals  <-  c(0,0,1, # 2015
                 0,0,1, # 2016
                 0,0,1, # 2017
                 0,0,1, # 2018
                 0,0)   # 2019
intervals

# now, process data in RMark format
t.proc = process.data(tern, model = "RDOccupEG", groups = "beach", 
                      time.intervals=intervals, begin.time = 2015)

# create design data
t.ddl = make.design.data(t.proc)

# look at design data
names(t.ddl)

t.ddl$Psi # initial occupancy
# probability that little terns bred on a given beach in 2015

t.ddl$Epsilon # extinction
# probability of an occupied site becoming unoccupied
# probability of a beach that little terns bred on during time t
# was not bred on during time t + 1

t.ddl$Gamma # colonization
# probability of an unoccupied site becoming occupied
# probability of a beach that little terns did not breed on during time t
# being bred on during time t + 1

t.ddl$p # detection probability



# Now let's fit a model
# I usually do this in 2 steps
# First, specify the formula for each model parameter and save as an object
# Then insert these objects for each parameter in the model fitting function 'mark'

# So, let's specify model formulas for each parameter (p, Psi, Epsilon, and Gamma)

# define p model
t.ddl$p # detection probability
# let's obtain a different detection probability in each year
p.1 = list(formula =  ~  session)
# note that, for the robust design, 'session' indicates each year
# and 'time' indicates surveys within each year for detection probability
# this can be confusing because 'time' indicates each year for the
# probability of occupancy, extinction, and colonization

# define Psi model
t.ddl$Psi # initial occupancy
# let's allow initial occupancy to vary across beaches
Psi.1 = list(formula =  ~ beach)

# define Epsilon model
t.ddl$Epsilon # extinction probability
# let's allow extinction probability to vary over time
E.1 = list(formula =  ~ time)

# define Gamma model
t.ddl$Gamma # colonization probability
# let's allow colonization probability to vary over time
G.1 = list(formula =  ~ time)


# Now fit the model

# you will need (want) an output folder for the mark files, just so your
# directory does not get cluttered
# Create a new folder called 'models' in your working directory
# set working directory to that folder
setwd("G:/Shared drives/wyo-coop-barrile/Boreal_Toad_Project/RS/Population_Modeling_Sp23/Week_3_Closed_Population_Estimation/models")

# fit model
occ <- mark(t.proc, # processed data
            t.ddl,  # design data
            model.parameters=list(Psi = Psi.1,   # initial occupancy
                                  Gamma = G.1,   # colonization
                                  Epsilon = E.1, # extinciton
                                  p = p.1))      # detection


# check out model output

# beta coefficients
occ$results$beta

# real estimates
occ$results$real

# derived parameters
occ$results$derived$`psi Probability Occupied`

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# ---- 5) Prediction and plotting -----
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

# let's visualize the results

# let's plot temporal variation in extinction probability over our study period
ext <- occ$results$real[9:12,c(1,3,4)]

ext$year <- c("2015-2016","2016-2017","2017-2018","2018-2019")
ext$year <- as.factor(ext$year)
ext

# plot the values
ggplot(ext, aes(x=year, y=estimate)) +
  geom_errorbar(aes(ymin=lcl, ymax=ucl), 
                width=0.2, size=2, position=position_dodge(0.3)) +
  geom_point(position=position_dodge(0.3), stat="identity", size = 7) +
  xlab(NULL) +
  ylab("Extinction probability \n") +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  theme_bw()+
  theme(
    legend.position = "none",
    panel.border = element_blank(),
    axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
    axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.y = element_text(size = 20, vjust = 1, color = "black"), # spacing from y
    axis.text.x = element_text(size = 18, color = "black"),
    axis.text.y = element_text(size = 12, color = "black"))


# let's plot temporal variation in colonization probability over our study period
col <- occ$results$real[13:16,c(1,3,4)]

col$year <- c("2015-2016","2016-2017","2017-2018","2018-2019")
col$year <- as.factor(col$year)
col

# plot the values
ggplot(col, aes(x=year, y=estimate)) +
  geom_errorbar(aes(ymin=lcl, ymax=ucl), 
                width=0.2, size=2, position=position_dodge(0.3)) +
  geom_point(position=position_dodge(0.3), stat="identity", size = 7) +
  xlab(NULL) +
  ylab("Colonization probability \n") +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  theme_bw()+
  theme(
    legend.position = "none",
    panel.border = element_blank(),
    axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
    axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.y = element_text(size = 20, vjust = 1, color = "black"), # spacing from y
    axis.text.x = element_text(size = 18, color = "black"),
    axis.text.y = element_text(size = 12, color = "black"))



# let's plot temporal variation in colonization and extinction probability 
# over our study period on one graph

ext$colext <- "Extinction"
col$colext <- "Colonization"

colext <- rbind(ext, col)

colext$colext <- as.factor(colext$colext)


# plot the values
ggplot(colext, aes(x=year, y=estimate, group=colext, color=colext)) +
  geom_errorbar(aes(ymin=lcl, ymax=ucl), 
                width=0.2, size=2, position=position_dodge(0.3)) +
  geom_point(position=position_dodge(0.3), stat="identity", size = 7) +
  xlab(NULL) +
  ylab("Probability of change in breeding \n") +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  theme_bw()+
  theme(
    panel.background = element_rect(colour = "black", size=1, linetype = "solid"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks.length = unit(0.2,"cm"),
    axis.title.x = element_text(size = 22, color = "black"),
    axis.title.y = element_text(size = 22, color = "black"),
    axis.text.x = element_text(size = 18, color = "black"),
    axis.text.y = element_text(size = 14, color = "black")) +
    theme(legend.title = element_blank())+
    theme(legend.text = element_text(size = 17))+
    theme(legend.position="top")+
    theme(strip.text = element_text(size = 7))








# let's plot temporal variation in detection probability over our study period
det <- occ$results$real[17:21,c(1,3,4)]

det$year <- c("2015","2016","2017","2018","2019")
det$year <- as.factor(det$year)
det

# plot the values
ggplot(det, aes(x=year, y=estimate)) +
  geom_errorbar(aes(ymin=lcl, ymax=ucl), 
                width=0.2, size=2, position=position_dodge(0.3)) +
  geom_point(position=position_dodge(0.3), stat="identity", size = 7) +
  xlab(NULL) +
  ylab("Detection probability \n") +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  theme_bw()+
  theme(
    legend.position = "none",
    panel.border = element_blank(),
    axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
    axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.y = element_text(size = 20, vjust = 1, color = "black"), # spacing from y
    axis.text.x = element_text(size = 18, color = "black"),
    axis.text.y = element_text(size = 12, color = "black"))



# let's plot temporal variation in occupancy probability at each beach
# over our study period
occup <- occ$results$derived$`psi Probability Occupied`

occup$year <- c("2015","2016","2017","2018","2019",
                "2015","2016","2017","2018","2019",
                "2015","2016","2017","2018","2019",
                "2015","2016","2017","2018","2019",
                "2015","2016","2017","2018","2019",
                "2015","2016","2017","2018","2019",
                "2015","2016","2017","2018","2019",
                "2015","2016","2017","2018","2019")
occup$year <- as.factor(occup$year)
occup

occup$Beach <- c("1","1","1","1","1",
                 "2","2","2","2","2",
                 "3","3","3","3","3",
                 "4","4","4","4","4",
                 "5","5","5","5","5",
                 "6","6","6","6","6",
                 "7","7","7","7","7",
                 "8","8","8","8","8")

occup$Beach <- as.factor(occup$Beach)
occup


# plot the values
# plot the values
ggplot(occup, aes(x=year, y=estimate, group=Beach, color=Beach)) +
  #geom_point(size=5) + geom_line(size=1.5) +
  geom_point(position=position_dodge(0.3), stat="identity", size = 5) +
  geom_line(position=position_dodge(0.3), size=1.5) +
  xlab(NULL) +
  ylab("Occupancy probability \n") +
  scale_y_continuous(limits = c(-0.1, 1.1), breaks = seq(0, 1, 0.2), expand = c(0,0)) +
  theme_bw() +
  theme(#text = element_text(size = 18, family = "Times"), # controls all fonts in plot
    panel.background = element_rect(colour = "black", size=1, linetype = "solid"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks.length = unit(0.2,"cm"),
    axis.title.x = element_text(size = 22, color = "black"),
    axis.title.y = element_text(size = 22, color = "black"),
    axis.text.x = element_text(size = 18, color = "black"),
    axis.text.y = element_text(size = 14, color = "black")) +
  theme(legend.title = element_text(size = 17))+
  theme(legend.text = element_text(size = 17))+
  theme(legend.position="top")+
  theme(strip.text = element_text(size = 7))

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# ---- 6) Adding covariates -----
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

# THE BELOW CODE IS MAINLY HERE JUST AS AN EXAMPLE OF WHAT WE WILL DO
# IN LATER LESSONS IN RMARK

# take a look at the tern df
tern

# add covariate to tern df
# create some covariate data (here we have a yearly-site covariate in unmarked terminology)
tern$ppl2015 <- sample(10:20, size=8, replace=TRUE)
tern$ppl2016 <- sample(12:22, size=8, replace=TRUE)
tern$ppl2017 <- sample(20:26, size=8, replace=TRUE)
tern$ppl2018 <- sample(21:28, size=8, replace=TRUE)
tern$ppl2019 <- sample(20:29, size=8, replace=TRUE)

# see what we just did
tern


# reprocess the tern df for RMark

# specify time intervals again for Robust Design (in case you deleted this above)
intervals  <-  c(0,0,1, # 2015
                 0,0,1, # 2016
                 0,0,1, # 2017
                 0,0,1, # 2018
                 0,0)   # 2019
intervals

# now, process data in RMark format
t.proc = process.data(tern, model = "RDOccupEG", groups = "beach", 
                      time.intervals=intervals, begin.time = 2015)

# create design data
t.ddl = make.design.data(t.proc)

# look at design data
names(t.ddl)

t.ddl$Psi # initial occupancy
# probability that little terns bred on a given beach in 2015

t.ddl$Epsilon # extinction
# probability of an occupied site becoming unoccupied
# probability of a beach that little terns bred on during time t
# was not bred on during time t + 1

t.ddl$Gamma # colonization
# probability of an unoccupied site becoming occupied
# probability of a beach that little terns did not breed on during time t
# being bred on during time t + 1

t.ddl$p # detection probability


# Now let's fit a model
# I usually do this in 2 steps
# First, specify the formula for each model parameter and save as an object
# Then insert these objects for each parameter in the model fitting function 'mark'

# So, let's specify model formulas for each parameter (p, Psi, Epsilon, and Gamma)

# define p model
t.ddl$p # detection probability
p.1 = list(formula =  ~  1)

# define Psi model
t.ddl$Psi # initial occupancy
# let's allow initial occupancy to vary across beaches
Psi.1 = list(formula =  ~ beach)

# define Epsilon model
t.ddl$Epsilon # extinction probability
# let's allow extinction probability to vary by the number of
# people that visit a given beach per day
E.1 = list(formula =  ~ ppl)

# define Gamma model
t.ddl$Gamma # colonization probability
G.1 = list(formula =  ~ 1)


# Now fit the model

# you will need (want) an output folder for the mark files, just so your
# directory does not get cluttered
# Create a new folder called 'models' in your working directory
# set working directory to that folder
setwd("G:/Shared drives/wyo-coop-barrile/Boreal_Toad_Project/RS/Population_Modeling_Sp23/Week_3_Closed_Population_Estimation/models")

# fit model
occ <- mark(t.proc, # processed data
            t.ddl,  # design data
            model.parameters=list(Psi = Psi.1,   # initial occupancy
                                  Gamma = G.1,   # colonization
                                  Epsilon = E.1, # extinciton
                                  p = p.1))      # detection


# check out model output

# beta coefficients
occ$results$beta

# real estimates
occ$results$real


# plot the covariate relationship 
# (between extinction probability and number of people visiting the beach)

# need to specify the indices correctly (very important!)
occ$pims$Epsilon # let's use indices = 9

# create sequence of values to predict over
head(tern)
range(tern[,4:8])
# notice that we specify ppl2015
newdat <- data.frame(ppl2015=seq(12, 29, length.out = 40))

# predict to newdata
pred.cov <- covariate.predictions(occ, data=newdat,indices=c(9))$estimates

# okay, plot it
min(pred.cov$lcl)
max(pred.cov$ucl)
op <- par(mar = c(5,5,4,2) + 0.1) # default is 5,4,4,2
plot(x = pred.cov$covdata, y = pred.cov$estimate, pch=16, 
     ylab = "Extinction Probability",
     xlab = "Number of beach goers (per day)", cex.lab=1.5, cex.axis=1.2, 
     col="darkgray", ylim=c(0,1))
box(lwd = 4, col = 'black')
lines(pred.cov$covdata, pred.cov$estimate, lwd=8, col="blue")
lines(pred.cov$covdata, pred.cov$lcl, lwd=4, lty=2, col="black")
lines(pred.cov$covdata, pred.cov$ucl, lwd=4, lty=2, col="black")





# how about a site-level covariate for initial occupancy?
# we can also add covariate data directly to the design matrix
t.ddl$Psi
t.ddl$Psi$pRock <- NA
t.ddl$Psi$pRock[t.ddl$Psi$beach == "1"] <- 28
t.ddl$Psi$pRock[t.ddl$Psi$beach == "2"] <- 10
t.ddl$Psi$pRock[t.ddl$Psi$beach == "3"] <- 32
t.ddl$Psi$pRock[t.ddl$Psi$beach == "4"] <- 5
t.ddl$Psi$pRock[t.ddl$Psi$beach == "5"] <- 24
t.ddl$Psi$pRock[t.ddl$Psi$beach == "6"] <- 7
t.ddl$Psi$pRock[t.ddl$Psi$beach == "7"] <- 18
t.ddl$Psi$pRock[t.ddl$Psi$beach == "8"] <- 9
t.ddl$Psi


# Now let's fit a model

# define p model
t.ddl$p # detection probability
# let's obtain a different detection probability in each year
p.1 = list(formula =  ~  1)

# define Psi model
t.ddl$Psi # initial occupancy
# let intitial occupancy vary by the percent rock cover at each beach
Psi.1 = list(formula =  ~ pRock)

# define Epsilon model
t.ddl$Epsilon # extinction probability
E.1 = list(formula =  ~ 1)

# define Gamma model
t.ddl$Gamma # colonization probability
G.1 = list(formula =  ~ 1)

# set working directory to that folder
setwd("G:/Shared drives/wyo-coop-barrile/Boreal_Toad_Project/RS/Population_Modeling_Sp23/Week_3_Closed_Population_Estimation/models")

# fit model
occ <- mark(t.proc, # processed data
            t.ddl,  # design data
            model.parameters=list(Psi = Psi.1,   # initial occupancy
                                  Gamma = G.1,   # colonization
                                  Epsilon = E.1, # extinciton
                                  p = p.1))      # detection



# check out model output

# beta coefficients
occ$results$beta

# real estimates
occ$results$real

# we cannot use the covariate.predictions() function
# for data in the design matrix (sort of frustrating)

# instead, we can plot over the real estimates

# get real estimates that we want for plotting
iocc <- occ$results$real[1:8,c(1,3,4)]
iocc
# add covariate values (make sure everything lines up here,
# I'm only adding these values like this because we know
# that the results are in the same order as the covariate
# values from the design data. Be careful, make sure that is true
# when you are doing this!)
iocc$rockcov <- t.ddl$Psi$pRock
# order by cov values
iocc <- iocc[order(iocc$rockcov),]

min(iocc$lcl)
max(iocc$ucl)
op <- par(mar = c(5,5,4,2) + 0.1) # default is 5,4,4,2
plot(x = iocc$rockcov, y = iocc$estimate, pch=16, 
     ylab = "Initial occupancy",
     xlab = "Rock cover (%)", cex.lab=1.5, cex.axis=1.2, 
     col="darkgray", ylim=c(0,1))
box(lwd = 4, col = 'black')
lines(smooth.spline(iocc$rockcov, iocc$estimate, spar=0.55), lwd=8, col="blue")
lines(smooth.spline(iocc$rockcov, iocc$lcl, spar=0.55), lwd=4, lty=2, col="black")
lines(smooth.spline(iocc$rockcov, iocc$ucl, spar=0.55), lwd=4, lty=2, col="black")
# I don't advise that you use smooth.spline when plotting (unless you have a good
# reason and know what you are doing. We are just doing that here for illustrative purposes)








# how about an observation-level covariate for detection?
# again, let's add values to the design data
t.ddl$p
# make up some survey-specific temperature data
t.ddl$p$temp <- sample(15:30, size=nrow(t.ddl$p), replace=TRUE)
t.ddl$p

# Now let's fit a model

# define p model
t.ddl$p # detection probability
# let detection probability vary by temperature
p.1 = list(formula =  ~  temp)

# define Psi model
t.ddl$Psi # initial occupancy
Psi.1 = list(formula =  ~ 1)

# define Epsilon model
t.ddl$Epsilon # extinction probability
E.1 = list(formula =  ~ 1)

# define Gamma model
t.ddl$Gamma # colonization probability
G.1 = list(formula =  ~ 1)

# set working directory to that folder
setwd("G:/Shared drives/wyo-coop-barrile/Boreal_Toad_Project/RS/Population_Modeling_Sp23/Week_3_Closed_Population_Estimation/models")

# fit model
occ <- mark(t.proc, # processed data
            t.ddl,  # design data
            model.parameters=list(Psi = Psi.1,   # initial occupancy
                                  Gamma = G.1,   # colonization
                                  Epsilon = E.1, # extinciton
                                  p = p.1))      # detection



# check out model output

# beta coefficients
occ$results$beta

# real estimates
occ$results$real

# again, we cannot use the covariate.predictions() function
# for data in the design matrix 

# instead, we can plot over the real estimates
# another way to grab the real estimates
plist=get.real(occ,"p",se=TRUE)
# only want a subset of the columns
pvalues <- plist %>% select(estimate, lcl, ucl, time, session, beach)

# add the covariate values for temperature to the results (estimates)
# again, be very careful here! We are only doing it this way
# because we know that the results df is in the same exact order
# as the design data. So we can just add the column. But you will
# want to check and make sure everything lines up before adding the values
pvalues$temp <- t.ddl$p$temp

# order by temperature
pvalues <- pvalues %>% arrange(temp)

# plot it
min(pvalues$lcl)
max(pvalues$ucl)
op <- par(mar = c(5,5,4,2) + 0.1) # default is 5,4,4,2
plot(x = pvalues$temp, y = pvalues$estimate, pch=16, 
     ylab = "Detection Probability",
     xlab = "Temperature (°C)", cex.lab=1.5, cex.axis=1.2, 
     col="darkgray", ylim=c(0,1))
box(lwd = 4, col = 'black')
lines(pvalues$temp, pvalues$estimate, lwd=8, col="blue")
lines(pvalues$temp, pvalues$lcl, lwd=4, lty=2, col="black")
lines(pvalues$temp, pvalues$ucl, lwd=4, lty=2, col="black")


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# END
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#































