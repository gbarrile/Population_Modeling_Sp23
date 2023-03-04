
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# POPULATION MODELING IN ECOLOGY
# ZOO 5890-09
# University of Wyoming
# Spring 2023

# Week 7 - Cormack-Jolly-Seber (CJS) Model to estimate survival

# Description: Steps through an example analysis of a
# Cormack-Jolly-Seber model as implemented in the package `RMark`.

# Good reference to check out:

# Lebreton, J. D., Burnham, K. P., Clobert, J., & Anderson, D. R. (1992). 
# Modeling survival and testing biological hypotheses using marked animals: 
# a unified approach with case studies. Ecological monographs, 62(1), 67-118.

# The code below addresses the question: 
# Does brook trout survival differ in a national park versus land developed for energy extraction?"

# Gabe Barrile - University of Wyoming
# Last updated 03/06/2023
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# ---- Outline -----
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# This script contains the following sections:
# 1) Install and load packages
# 2) Read in field data
# 3) Format data for RMark
# 4) Fit Cormack-Jolly-Seber models
# 5) Prediction and plotting
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# ---- 1) Install and load packages -----
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# Check if packages are installed.
# If yes, load them.  If not, install, then load.

# RMark for fitting the CJS model
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
# ---- 2) Read-in field data -----
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#


# you will need to set working directory to where you saved the 'BrookTrout_EnergyDevelopment.csv'
setwd()

# read-in the brook trout capture-mark-recapture data from the streams that we surveyed 
df <- read.csv("BrookTrout_EnergyDevelopment.csv")

# take a peek at the data
head(df)

# how many areas did we sampled?
n_distinct(df$Location) # 2 areas
unique(df$Location) # (national park and area developed for energy extraction) 

# how many fish did we capture and mark?
n_distinct(df$Tag) # 146 fish

# how many unique individuals were captured at each location?
df %>% 
  group_by(Location) %>% 
  summarise(n=n_distinct(Tag)) %>% 
  rename(Number.of.Individuals=n) %>% 
  data.frame()

# how many surveys per location?
df %>% 
  group_by(Location) %>% 
  summarise(n=n_distinct(Survey)) %>% 
  rename(Number.of.Surveys=n) %>% 
  data.frame()


# So, it seems we have capture-mark-recapture data for brook trout
# from stream surveys at two general locations (national park and land devoted
# to energy development), and we surveyed each location four times

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

# check if all capture histories are length = 4 (for each of our four surveys at each location)
table(nchar(y$ch))

# add variables to 'y' dataframe

# Location: national park or developed for energy extraction
y$Location <- df$Location[match(y$Tag, df$Tag)]

# bt will be our dataframe that we input into RMark
bt <- data.frame(ch = y$ch, 
                 freq = 1, 
                 location = y$Location, 
                 tag = y$Tag)

# remove unneeded objects
rm(y,m)

# so we have df and bt dataframes
head(df)
head(bt)

# make location a factor variable
bt$location <- as.factor(as.character(bt$location))
table(bt$location)

# are we missing any data?
table(is.na(bt)) # no missing data = good!

# let's put in an 'age at first marking covariate'


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Check capture histories against our field data
# Look at our first individual
head(df %>% arrange(Tag), 6)
# Does the capture history ('ch' column in tern) make sense for fish 100? 101? 102?
head(bt, 3)
# Stop here and make sure you understand the capture histories (the 'ch' column in bt df)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# ---- 4) Fit Cormack-Jolly-Seber Models in RMark -----
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

# first, process the data for RMark
# input the model that we want in the 'model =' argument of the process.data function

# here are the list of models in RMark
setup.model(model = "") # 91 models

# As you can see, RMark includes lots of models that can be input into the model= argument below
# Here we use the "CJS" model because we are interested in 
# Cormack-Jolly-Seber models for estimating survival of brook trout.
# Visit this link for a full list of MARK models supported in RMark:
# https://github.com/jlaake/RMark/blob/master/RMark/inst/MarkModels.pdf


# now, process data in RMark format
d.proc=process.data(bt, 
                    model="CJS", 
                    groups = c("location"),
                    begin.time = 2015) 

# create design data
d.ddl <- make.design.data(d.proc)
# NOTE: the design dataframes are just as important as your raw data!

# Let's explore the design data
# see which parameters are estimated in the model
names(d.ddl)

d.ddl$Phi # apparent survival probability
# survival is termed apparent because mortality cannot be separated from emigration
# in other words, we don't really know whether an individual died or just left our study area
# one could make assumptions about a given system and perhaps it is reasonable to assume
# that individuals have a very low probability of moving out of the study area 
# and therefore apparent survival is a close approximation of true survival. 
# just depends on the dynamics of the system

d.ddl$p # capture probability






# Rather than fitting an extremely large model set incorporating all plausible combinations
# for model parameters, a step-down approach is often recommended to identify supported models

# For instance, in step 1, it is often recommended to hold survival at a high dimensionality
# (i.e., varies over time and space) while identifying the most parsimonious structure
# for capture probability. Then, in step 2, you would retain the most parsimonious
# structure for capture probability (from step 1) when fitting and comparing different
# model structures for survival.

# For more information on this approach, see Lebreton et al. (1992) and Doherty et al. (2012):

# Lebreton, J. D., K. P. Burnham, J. Clobert, and D. R. Anderson.
# 1992. “Modeling Survival and Testing Biological Hypotheses
# Using Marked Animals: A Unified Approach with Case
# Studies.” Ecological Monographs 62(1): 67–118.
# 
# Doherty, P. F., G. C. White, and K. P. Burnham. 2012. “Comparison
# of Model Building and Selection Strategies.” Journal of
# Ornithology 152(2): 317–23



# Let's take the approach described above.

# First, let's hold survival at a high dimensionality

# define Phi model
d.ddl$Phi # apparent survival 
# let's allow survival to vary over space and time
Phi.glob = list(formula =  ~ location + time)
Phi.glob = list(formula =  ~ location) # we likely don't have enough data for location and time in this example dataset (let's just try location)

# Now, let's specify some models for capture probability
d.ddl$p # capture probability
# 1. constant
p. = list(formula=~1)
# 2. year (time)
p.t = list(formula=~time)
# 3. year (Time)
p.tnum = list(formula=~Time)
# 4. location (space)
p.s = list(formula=~location)
# 5. 'age'
p.a = list(formula=~age)
# 6. cohort
p.c = list(formula=~cohort)
# 7. year (time) + location
p.ts = list(formula=~time + location)
# 8. year (time) * location
p.txs = list(formula=~time * location)


# fit models

# you will need (want) an output folder for the mark files, just so your
# directory does not get cluttered
# Create a new folder called 'models' in your working directory
# set working directory to that folder
setwd("G:/Shared drives/wyo-coop-barrile/Boreal_Toad_Project/RS/Population_Modeling_Sp23/Week_3_Closed_Population_Estimation/models")

# to fit models, we need:
# our processed data (d.proc)
# our design data (d.ddl)
# model structures for p (capture probability) and Phi (apparent survival probability)
# hold phi at high complexity (Phi.glob) while varying structures for capture prob

# 1. constant
m.     <- mark(d.proc, d.ddl, model.parameters = list(p = p., Phi = Phi.glob))
# 2. year (time)
m.t    <- mark(d.proc, d.ddl, model.parameters = list(p = p.t, Phi = Phi.glob))
# 3. year (Time)
m.tnum <- mark(d.proc, d.ddl, model.parameters = list(p = p.tnum, Phi = Phi.glob))
# 4. location (space)
m.s    <- mark(d.proc, d.ddl, model.parameters = list(p = p.s, Phi = Phi.glob))
# 5. 'age'
m.a    <- mark(d.proc, d.ddl, model.parameters = list(p = p.a, Phi = Phi.glob))
# 6. cohort
m.c    <- mark(d.proc, d.ddl, model.parameters = list(p = p.c, Phi = Phi.glob))
# 7. year (time) + location
m.ts   <- mark(d.proc, d.ddl, model.parameters = list(p = p.ts, Phi = Phi.glob))
# 8. year (time) * location
m.txs <- mark(d.proc, d.ddl, model.parameters = list(p = p.txs, Phi = Phi.glob))


#################################
# Collect models
#################################

#Collect models to create AIC table 
mods <- collect.models(type = "CJS") #grabs all models in workspace
names(mods)
temp <- mods$model.table
temp

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Write custom function to check for singular parameters 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
sing.check <- function(mod.set){
  out <- data.frame()
  for(i in 1:(length(mod.set)-1)){ # -1 b/c otherwise will return value for the table itself
    sing <- mod.set[[i]]$results$singular
    sing <- if(is.null(sing)) "all converged" else (sing)
    mod <- mod.set[[i]]$model.name
    temp <- data.frame(model = mod, mod.num = i, singular = sing)
    temp$singular <- as.character(temp$singular)
    out <- rbind(temp, out)
  }
  return(out)
} 

# Check for singular parameters w/ custom function for RMark model list
sing.out <- sing.check(mods)
sing.out # take a look
sing.out[sing.out$singular != "all converged", ] # mods with params that didn't converge
sing.out$mod.num[sing.out$singular != "all converged"] # just the mod numbers, which you'll need

# Create a new mod set without mods with convergence issues
bad.mods <- sing.out$mod.num[sing.out$singular != "all converged"] # just the bad mod #'s
model.list2 <- remove.mark(mods, bad.mods) # drop the bad mods via the mod #'s

# Look at AIC table
# model.list2$model.table
AICtable.p <- as.data.frame(model.list2$model.table)
AICtable.p

# save top model as object
top.aic <- model.list2$m.c

# get real estimates for capture probability
p.aic=get.real(top.aic, "p", se=TRUE)
p.aic
p.aic[c(1,4,6),c(10,3,5,6)]

# create better aic table
names(AICtable.p)
taby <- AICtable.p[ , c(2,5,6,7,4)]
taby[1,]
class(taby)
names(taby) <- c("Model","AICc","DAICc","Model Weight","K")
taby$`Model Weight` <- round(taby$`Model Weight`, 2)
taby$DAICc <- round(taby$DAICc, 2)
taby$AICc <- round(taby$AICc, 2)
taby 


# okay, so the best structure for capture probability is by 
# cohort (~cohort). However, the null/constant model is not
# much different (delta AIC of ~2), so let's retain the
# constant structure when comparing models of survival probability

# let's remove some objects from the environment
rm(AICtable.p,m.,m.a,m.c,m.s,m.t,m.tnum,m.ts,m.txs,
   model.list2,mods,p.,p.a,p.aic,p.c,p.s,p.t,p.tnum,
   p.ts,p.txs,Phi.glob,sing.out,
   taby,temp,top.aic,bad.mods)



#################################
# Now model apparent survival
#################################

# we know we will retain the constant model for capture probability
p. = list(formula=~1)

# Now, let's specify some models for survival probability
d.ddl$Phi # apparent survival probability

# 1. constant
Phi. = list(formula=~1)
# 2. year (time)
Phi.t = list(formula=~time)
# 3. year (Time)
Phi.tnum = list(formula=~Time)
# 4. location (space)
Phi.s = list(formula=~location)
# 5. 'age'
Phi.a = list(formula=~age)
# 6. cohort
Phi.c = list(formula=~cohort)
# 7. year (time) + location
Phi.ts = list(formula=~time + location)
# 8. year (time) * location
Phi.txs = list(formula=~time * location)


# fit models

# you will need (want) an output folder for the mark files, just so your
# directory does not get cluttered
# Create a new folder called 'models' in your working directory
# set working directory to that folder
setwd("G:/Shared drives/wyo-coop-barrile/Boreal_Toad_Project/RS/Population_Modeling_Sp23/Week_3_Closed_Population_Estimation/models")

# to fit models, we need:
# our processed data (d.proc)
# our design data (d.ddl)
# model structures for p (capture probability) and Phi (apparent survival probability)
# retain best structure for p from step 1 (constant in this case) 
# while varying structures for apparent survival 

# 1. constant
m.     <- mark(d.proc, d.ddl, model.parameters = list(p = p., Phi = Phi.))
# 2. year (time)
m.t    <- mark(d.proc, d.ddl, model.parameters = list(p = p., Phi = Phi.t))
# 3. year (Time)
m.tnum <- mark(d.proc, d.ddl, model.parameters = list(p = p., Phi = Phi.tnum))
# 4. location (space)
m.s    <- mark(d.proc, d.ddl, model.parameters = list(p = p., Phi = Phi.s))
# 5. 'age'
m.a    <- mark(d.proc, d.ddl, model.parameters = list(p = p., Phi = Phi.a))
# 6. cohort
m.c    <- mark(d.proc, d.ddl, model.parameters = list(p = p., Phi = Phi.c))
# 7. year (time) + location
m.ts   <- mark(d.proc, d.ddl, model.parameters = list(p = p., Phi = Phi.ts))
# 8. year (time) * location
m.txs  <- mark(d.proc, d.ddl, model.parameters = list(p = p., Phi = Phi.txs))


#################################
# Collect models
#################################

#Collect models to create AIC table 
mods <- collect.models(type = "CJS") #grabs all models in workspace
names(mods)
temp <- mods$model.table
temp

# Check for singular parameters w/ custom function for RMark model list
sing.out <- sing.check(mods)
sing.out # take a look
sing.out[sing.out$singular != "all converged", ] # mods with params that didn't converge
sing.out$mod.num[sing.out$singular != "all converged"] # just the mod numbers, which you'll need

# Create a new mod set without mods with convergence issues
bad.mods <- sing.out$mod.num[sing.out$singular != "all converged"] # just the bad mod #'s
model.list2 <- remove.mark(mods, bad.mods) # drop the bad mods via the mod #'s

# Look at AIC table
# model.list2$model.table
AICtable.phi <- as.data.frame(model.list2$model.table)
AICtable.phi

# store top model as object
top.aic <- model.list2$m.s

# get real estimates for apparent survival
phi.aic=get.real(top.aic, "Phi", se=TRUE)

# view estimates for survival probability
phi.aic[c(1,7),c(17,3,5,6)] # AIC

# create better aic table
names(AICtable.phi)
taby <- AICtable.phi[ , c(1,5,6,7,4)]
taby[1,]
class(taby)
names(taby) <- c("Model","AICc","DAICc","Model Weight","K")
taby$`Model Weight` <- round(taby$`Model Weight`, 2)
taby$DAICc <- round(taby$DAICc, 2)
taby$AICc <- round(taby$AICc, 2)
taby 


# okay, the best structure for survival probability is location,
# so we will proceed using that model for inference

# look at model output

# you can again check for any singular parameters
top.aic$results$singular

# beta coefficients
top.aic$results$beta

# real estimates
top.aic$results$real

# no derived parameters (sad)
top.aic$results$derived


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# ---- 5) Plotting and predictions -----
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

# let's plot annual survival probability in the National Park versus
# site with energy development
surv <- top.aic$results$real[1:2,c(1,3,4)]

surv$location <- c("Energy Development","National Park")
surv$location <- as.factor(surv$location)
surv

# plot the values
ggplot(surv, aes(x=location, y=estimate)) +
  geom_errorbar(aes(ymin=lcl, ymax=ucl), 
                width=0.2, size=2, position=position_dodge(0.3)) +
  geom_point(position=position_dodge(0.3), stat="identity", size = 7) +
  xlab(NULL) +
  ylab("Apparent survival probability \n") +
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


# what about with the get.real function?
surv=get.real(top.aic, "Phi", se=TRUE)
surv <- surv[c(1,7), c(3,5,6)]

surv$location <- c("Energy Development","National Park")
surv$location <- as.factor(surv$location)
surv

# plot the values
ggplot(surv, aes(x=location, y=estimate)) +
  geom_errorbar(aes(ymin=lcl, ymax=ucl), 
                width=0.2, size=2, position=position_dodge(0.3)) +
  geom_point(position=position_dodge(0.3), stat="identity", size = 7) +
  xlab(NULL) +
  ylab("Apparent survival probability \n") +
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




# Work with the results: estimate difference in survival between sites with energy development
# and sites within the national park

# Estimate the difference in survival rates between the two locations and 
# put a confidence interval on that estimated difference. 

# Here, we use the output from the model and the delta method
# to develop the SE and 95% CI for expressing the uncertainty for the estimated difference. 

# For information on the delta method, refer to Appendix B in the
# Program MARK manual (i.e., Program MARK: A Gentle Introduction)

# store var-cov for real estimates
rr=get.real(top.aic, "Phi", se=TRUE, vcv=TRUE)
sigma=rr$vcv.real # store var-cov in "sigma"

# load package 'msm' that implements the delta method
# then run delta method on difference in survival
library(msm)
s.Energy = rr$estimates$estimate[1]  # first row w/ output for sites with energy development
s.Park = rr$estimates$estimate[7]    # first row w/ output for sites within the national park
Diff = s.Energy - s.Park 
seDiff=deltamethod(~x1-x2,c(s.Energy,s.Park),sigma)
lclDiff=Diff-1.96*seDiff
uclDiff=Diff+1.96*seDiff
phidiff <- data.frame(mean = Diff, se = seDiff, lcl = lclDiff, ucl = uclDiff)
round(phidiff, 3)
# 95% CI does not include zero, providing statistical evidence that 
# brook trout survival is lower at sites with energy development
# compared with sites located within the national park


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# ---- The End -----
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#



