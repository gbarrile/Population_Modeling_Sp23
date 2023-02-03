


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# POPULATION MODELING IN ECOLOGY
# ZOO 5890-09
# University of Wyoming
# Spring 2023

# Week 3 - Closed Population Estimation

# Description: code to fit the 'Closed' model for abundance in the 'RMark' package

# Addressing the question: "How does boreal toad abundance vary across ponds?"

# Gabe Barrile - University of Wyoming
# Last updated 02/06/2023
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# ---- Outline -----
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# This script contains the following sections:
# 1) Install and load packages
# 2) Read-in input data
# 3) Format input data for RMark
# 4) Fit several 'Closed' models
# 5) Obtain model predictions for plotting
# 6) Fit Huggins model with individual covariates
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# ---- 1) Install and load packages -----
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# Check if packages are installed.
# If yes, load them.  If not, install, then load.

# RMark for fitting the 'Closed' model for abundance
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

# if you needed to cite the RMark package
citation("RMark")
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# ---- 2) Read-in input data -----
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

# you will need to set working directory to where you saved the 'BorealToad_CaptureRecapture.csv'
setwd()

# read-in the boreal toad capture-mark-recapture data from our three ponds
df <- read.csv("BorealToad_CaptureRecapture.csv")

# take a peek at the data
head(df)

# how many individuals were captured?
n_distinct(df$Tag) # 137 unique toads

# how many unique individuals were captured at each pond?
df %>% 
  group_by(Pond) %>% 
  summarise(n=n_distinct(Tag)) %>% 
  rename(Number.of.Individuals=n) %>% 
  data.frame()

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# ---- 3) Format data for RMark -----
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

# first, remove duplicates (if the same individual was captured twice or more during the same survey)
# identify duplicates
df %>%
  dplyr::group_by(Tag, Survey) %>%
  dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
  dplyr::filter(n > 1L)

# remove duplicates
df <- df %>% distinct(Tag, Survey, .keep_all = TRUE)



# next, create capture histories for each individual

# first, reduce dataframe to columns that we need
m <- df %>% select(Tag, Survey)
# create a variable with a '1' for each time an individual was captured
m$Capture <- rep(1,nrow(m))
# make Tag a factor variable (possibly unnecessary but probably good practice)
m$Tag <- as.factor(as.character(m$Tag))
# order the data frame by Survey
m <- m %>% arrange(Survey)
# pivot data to wide format
y <- m %>% pivot_wider(id_cols = Tag, names_from = Survey, values_from = Capture, values_fill = 0)
y

# create capture history as character string
y <- y %>% unite("ch", c(2:ncol(.)), remove = FALSE, sep = "")

# reduce dataframe to columns that we need
y <- y %>% select(Tag, ch) %>% data.frame()
head(y)

# check if all capture histories are length = 4 (for each of our four visits to each pond)
table(nchar(y$ch))

# add variables to 'y' dataframe

# Pond
y$Pond <- df$Pond[match(y$Tag, df$Tag)]

# SVL
# let's just grab the average SVL of each toad over the study period
svl <- aggregate(SVL ~ Tag, data=df, FUN=mean)
head(svl)

# match by tag number to add SVL to y dataframe
y$SVL <- svl$SVL[match(y$Tag, svl$Tag)]


# boto will be our dataframe that we input into RMark
boto <- data.frame(ch = y$ch, 
                   freq = 1, 
                   pond = y$Pond,
                   svl = y$SVL,
                   tag = y$Tag)

head(boto)

# remove unneeded objects
rm(m,svl,y)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Check capture histories against our field data
# Look at toads with the following tag numbers: 100, 101, and 102
head(df %>% arrange(Tag), 10)
# Do the capture histories ('ch' column in boto) make sense for those three individuals?
head(boto, 3)
# Stop here and make sure you understand the capture histories (the 'ch' column in boto df)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# make pond a factor variable
boto$pond <- as.factor(as.character(boto$pond))
table(boto$pond)

# are we missing any data?
table(is.na(boto)) # no missing data = good!

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# ---- 4) Fit several 'Closed' models in RMark -----
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

# process data for CLOSED MODELS FOR ABUNDANCE
d.proc=process.data(boto, 
                    model="Closed", # Of course, with this model we assume NO Births, NO Deaths, NO Immigration, and NO Emigration during the study period  
                    groups = c("pond"))

# create design data
d.ddl <- make.design.data(d.proc)
# NOTE: the design dataframes are just as important as your raw data!
# design data in this example are fairly simple, 
# but will get more complex with different models

# Let's explore the design data
# see which parameters are estimated in the model
names(d.ddl)

# "p" = capture probability
# "c" = recapture probability
# "f0" = the number of individuals never captured (read more below)

# capture and recapture probabilities can be the same or can be different 
# let's say you catch a rabbit in a trap for the first time 
# (that's your capture probability)
# Then let's say that rabbit subsequently avoids your traps
# In that case, it's very likely that your recapture probability of that rabbit
# is different than the probability of capturing that rabbit the first time
# This is just one example whereby capture and recapture probability can differ

# look at design data for each parameter

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# capture probability
d.ddl$p
# look at columns 'time' and 'pond'
# why do we have 12 rows in this table? 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# recapture probability
d.ddl$c
# all individuals captured during survey 1 were captured for the first time,
# thus recapture probability starts at time = 2 or survey 2

# the number of individuals never captured
d.ddl$f0
# the notation 'f0' originates from the frequency (count) of animals observed 0 times). 
# The f0 parametrization is useful computationally because f0 is bounded on the 
# interval [0, positive infinity], thus forcing the logical constraint that 
# N > the number of individuals marked (M). 
# In fact, MARK uses the f0 parametrization for ease of computation by using 
# the log link function to constrain f0 >= 0, and presents the results in terms of N 
# as a derived parameter.
# In other words, the likelihood is rewritten in MARK in terms of the 
# number of individuals never caught, f0, such that f0 = N - M 
# (M = the number of individuals marked)



# Now let's fit some models
# I usually do this in 2 steps
# First, specify the formula for each model parameter and save as an object
# Then insert these objects for each parameter in the model fitting function 'mark'

# So, let's specify model formulas for each parameter

# Model 1 (constant model)

# p (capture probability)
pc.=list(formula= ~ 1, share = TRUE)
# share = TRUE indicates p = c or capture probability = recapture probability
# ~ 1 or the intercept model is often referred to as the 'constant' model, 
# meaning that capture probability is constant over time and space 
# (e.g., doesn't vary across surveys or at different ponds)

# f0 (number of individuals never captured)
f0.=list(formula= ~ 1)
# this model indicates that the same number of individuals were
# never captured at all ponds
# (e.g., we failed to capture and mark 10 individuals at
# Pond 1, 10 individuals at Pond 2, and 10 at pond 3)
# it does NOT mean that the abundance is the same at all ponds,
# just that the number we failed to capture is the same


# fit Model 1
# you will need (want) an output folder for the mark files, just so your
# directory does not get cluttered
# Create a new folder called 'models' in your working directory
# set working directory to that folder
setwd()

m1 <- mark(d.proc,
           d.ddl, 
           model.parameters = list(p = pc.,
                                   f0= f0.))


# look at model output

# beta coefficients
m1$results$beta
# these are just the intercepts in this case

# real estimates (on the scale we tend to think on)
m1$results$real

# derived parameters
m1$results$derived # what is the estimated abundance at each pond?
# Pond 1 = first row, Pond 2 = second row, Pond 3 = third row


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# how is abundance estimated?
# f0
f0 <- m1$results$real[2,1]
f0
# how many toads did we mark at each pond?
table(boto$pond)
# Add f0 to the number marked at each pond
c(f0 + 44, f0 + 18, f0 + 75)
# compare to the estimated abundances from the derived parameters in the model
m1$results$derived$`N Population Size`[,1]
# the numbers are identical 
# make sure you understand why those numbers are identical

# In other words, the model knows how many toads we tagged at each pond:
table(boto$pond)
# then it adds the estimated number of individuals that we failed to tag
f0
# Note that we can estimate a different f0 for each pond (below)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~




# Model 2

# Obtain different capture probability for each survey at each pond

# We conducted 4 surveys at each of three ponds, so 12 surveys total
# Therefore we should end up with 12 different capture probabilities,
# one for each survey at each site

# share p and c

# p
d.ddl$p
p.timepond =list(formula= ~ time * pond, share = TRUE)
# 'time' and 'pond' in the above formula must be spelled 
# exactly as shown in design data

# Different f0 for each pond
# f0
d.ddl$f0
f0.pond =list(formula= ~ pond)

# fit model
m2 <- mark(d.proc,
           d.ddl, 
           model.parameters = list(p = p.timepond,
                                   f0= f0.pond))


# look at model output

# beta coefficients
m2$results$beta

# real estimates 
m2$results$real

# derived parameters
m2$results$derived

# compare models with AICc
c(m1$results$AICc,m2$results$AICc)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# ---- 5) Obtain model predictions for plotting -----
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

# let's plot the estimated abundance at each pond,
# using estimates from Model 2 (m2)

abund <- m2$results$derived$`N Population Size`
abund$pond <- c("Pond 1", "Pond 2", "Pond 3")
abund$pond <- as.factor(abund$pond)
abund

# plot the values
ggplot(abund, aes(x=pond, y=estimate, color=pond)) +
  geom_errorbar(aes(ymin=lcl, ymax=ucl), 
                width=0.2, size=2, position=position_dodge(0.3)) +
  geom_point(position=position_dodge(0.3), stat="identity", size = 7) +
  xlab(NULL) +
  ylab("Estimated Abundance \n") +
  scale_y_continuous(limits = c(0, 160), breaks = seq(0, 160, 40)) +
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



# let's plot capture probability during each survey at each pond,
# again using estimates from Model 2 (m2)

cap <- m2$results$real[1:12,c(1,3,4)]
cap$pond <- c("Pond 1", "Pond 1", "Pond 1", "Pond 1",
              "Pond 2", "Pond 2", "Pond 2", "Pond 2",
              "Pond 3", "Pond 3", "Pond 3", "Pond 3")
cap$pond <- as.factor(cap$pond)

cap$survey <- c(1,2,3,4,
                1,2,3,4,
                1,2,3,4)

cap$survey <- as.factor(as.character(cap$survey))

cap

# plot the values
ggplot(cap, aes(x=survey, y=estimate, group=pond, color=pond)) +
  geom_point(size=5) + geom_line(size=1.5) +
  xlab("Survey") +
  ylab("Capture probability \n") +
  scale_y_continuous(limits = c(0, 0.8), breaks = seq(0, 0.8, 0.2), expand = c(0,0)) +
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
  theme(legend.title = element_blank())+
  theme(legend.text = element_text(size = 17))+
  theme(legend.position="top")+
  theme(strip.text = element_text(size = 7))


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# ---- 6) Fit Huggins model with individual covariates -----
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

# Now let's incorporate individual covariates (e.g., SVL) into our models

# We will specify the Huggins model

# Huggins models condition abundance out of the likelihood,
# thus permitting the modelling of capture probability as a function of individual covariates

# the parameter f0 is therefore not modeled in the Huggins framework. Not having to model
# this parameter is nice in many cases, so we will use several Huggins models in this course

# okay, process the data
d.proc=process.data(boto, model="Huggins") 
# let's not worry about different ponds in this example, so no groups = c("pond")

# create design data
d.ddl <- make.design.data(d.proc)
# NOTE: the design dataframes are just as important as your raw data!
# design data in this example are fairly simple, 
# but can get more complex with different models

# Let's explore the design data
# see which parameters are estimated in the model
names(d.ddl)

# look at design data for each parameter

# capture probability
d.ddl$p

# recapture probability
d.ddl$c
# all individuals captured during survey 1 were captured for the first time,
# thus recapture probability starts at time = 2 or survey 2

# the number of individuals never captured
d.ddl$f0
# In this model, the likelihood is conditioned on the number of animals detected 
# and f0 therefore drops out of the likelihood. 
# These models contain only p and c, with abundance N estimated as a derived parameter.
# As noted earlier, the primary advantage of the Huggins data type is that 
# individual covariates can be used to model p and c.


# specify models for each parameter

# Model 1

# p (capture probability)
pc.=list(formula= ~ 1, share = TRUE)
# share = TRUE indicates p = c or capture probability = recapture probability
# ~ 1 or the intercept model is often referred to as the 'constant' model, 
# meaning that capture probability is constant over time and space 
# (e.g., doesn't vary across surveys or at different ponds)


# fit Model 3
m3 <- mark(d.proc,
           d.ddl, 
           model.parameters = list(p = pc.))


# look at model output

# beta coefficients
m3$results$beta
# these are just the intercepts in this case

# real estimates (on the scale we tend to think on)
m3$results$real

# derived parameters
m3$results$derived

# how is abundance estimated?

# what is the probability of an individual not being captured at all during our study?
# (1 - p)(1 - p)(1 - p)(1 - p)
# what is p, our capture probability?
p <- m3$results$real$estimate[1]
p
# the probability of an individual not being captured at all during our study?
p.no <- (1 - p)*(1 - p)*(1 - p)*(1 - p)
# so what is the probability of being captured at least once?
p.once <- 1 - p.no
# then we can say that the number of individuals that we marked was equal to
# the probability of being captured at least once times the total population size (N)

# so we need to obtain the total number that we marked 
marked <- nrow(boto)

# here's the equation:      p.once * N = marked
# rearrage to solve for N:  N = marked / p.once

# So N should be:
marked / p.once
# compare to derived estimate from model 
m3$results$derived





# Let's try svl as individual covariate on capture probability

# p (capture probability)
pc.svl =list(formula= ~ svl, share = TRUE)


# fit Model 4
m4 <- mark(d.proc,
           d.ddl, 
           model.parameters = list(p = pc.svl))


# look at model output

# beta coefficients
m4$results$beta

# real estimates (on the scale we tend to think on)
m4$results$real

# derived parameters
m4$results$derived


# plot relationship between capture probability and SVL
range(boto$svl)
# make predictions based on model (m4)
pred.svl <- covariate.predictions(m4, data=data.frame(svl=56:88),indices=c(1))$estimates

min(pred.svl$lcl)
max(pred.svl$ucl)
op <- par(mar = c(5,5,4,2) + 0.1) # default is 5,4,4,2
plot(x = pred.svl$covdata, y = pred.svl$estimate, pch=16, 
     ylab = "Capture Probability",
     xlab = "SVL (mm)", cex.lab=1.5, cex.axis=1.2, 
     col="darkgray", ylim=c(0.1,0.5))
box(lwd = 4, col = 'black')
lines(pred.svl$covdata, pred.svl$estimate, lwd=8, col="blue")
lines(pred.svl$covdata, pred.svl$lcl, lwd=4, lty=2, col="black")
lines(pred.svl$covdata, pred.svl$ucl, lwd=4, lty=2, col="black")


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# END
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#




