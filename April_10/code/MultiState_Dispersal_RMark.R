
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# POPULATION MODELING IN ECOLOGY
# ZOO 5890-09
# University of Wyoming
# Spring 2023

# Week 12 - Multi-state capture-recapture models to estimate survival 
# and characterize movement behavior (in this case, dispersal rates)

# Description: Steps through an example analysis using functions in the package `RMark`.

# Good reference to check out:

# Chabanne, D. B., Pollock, K. H., Finn, H., & Bejder, L. (2017). 
# Applying the multistate capture–recapture robust design to characterize 
# metapopulation structure. Methods in Ecology and Evolution, 8(11), 1547-1557.

# The code below addresses the question: 
# How does boreal toad dispersal vary among our three study ponds?

# Gabe Barrile - University of Wyoming
# Last updated 04/10/2023
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# ---- Outline -----
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# This script contains the following sections:
# 1) Install and load packages
# 2) Read in field data
# 3) Format data for RMark
# 4) Fit Multi-state models
# 5) Prediction and plotting
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# ---- 1) Install and load packages -----
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# Check if packages are installed.
# If yes, load them.  If not, install, then load.

# RMark for fitting the Robust Design model
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
# ---- 2) Read-in field data -----
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

# you will need to set working directory to where you saved the 'BorealToad_Dispersal.csv'
setwd()

# read-in the boreal toad capture-mark-recapture data
setwd("G:/Shared drives/wyo-coop-barrile/Boreal_Toad_Project/RS/Population_Modeling_Sp23/Week_9b_MultiState_RMark/data")

# read-in data from the csv
df <- read.csv("BorealToad_Dispersal.csv")

# take a peek at the data
head(df)

# first, order the data frame by Tag and Survey
df <- df %>% arrange(Tag, Survey)
head(df)

# how many ponds did we sample?
n_distinct(df$Pond) # 3 ponds
unique(df$Pond) # A, B, and C

# how many toads did we capture and mark?
n_distinct(df$Tag) # 1129 toads

# how many unique individuals were captured at each pond?
df %>% 
  group_by(Pond) %>% 
  summarise(n=n_distinct(Tag)) %>% 
  rename(Number.of.Individuals=n) %>% 
  data.frame()


# create year variable
# create quick function to extract year from the Date column
RIGHT = function(x,n){
  substring(x,nchar(x)-n+1)
}
# now create year column in df
df$Year <- RIGHT(df$Date, 4)
n_distinct(df$Year) # 5 years
unique(df$Year)

# how many surveys per pond?
df %>% 
  group_by(Pond) %>% 
  summarise(n=n_distinct(Survey)) %>% 
  rename(Surveys=n) %>% 
  data.frame()

# how many surveys per pond per year?
df %>% 
  group_by(Pond, Year) %>% 
  summarise(n=n_distinct(Survey)) %>% 
  rename(Surveys=n) %>% 
  data.frame()


# So, it seems we have capture-mark-recapture data for boreal toads
# at three ponds, and we surveyed each pond for five years (2016-2020).
# However, within each year, we surveyed ponds a different numbers of
# times depending on the year. For instance, we surveyed Pond A 9 times
# in 2016 but only 5 times in 2017, whereas we surveyed Pond B 4 times
# in 2016 and 4 times in 2017. So we'll need to account for this
# disparity by fixing various capture probabilties in RMark

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

# remove duplicates (we did not have any in this case)
df <- df %>% distinct(Tag, Survey, .keep_all = TRUE)



# next, create capture histories for each individual

# first, reduce dataframe to columns that we need
m <- df %>% select(Tag, Survey, Pond)
# create a numeric variable for pond (need this to pivot values)
m$nPond <- NA
m$nPond[m$Pond == "A"] <- 1
m$nPond[m$Pond == "B"] <- 2
m$nPond[m$Pond == "C"] <- 3
# make Tag a factor variable (possibly unnecessary but probably good practice)
m$Tag <- as.factor(as.character(m$Tag))
# order the data frame by Survey
m <- m %>% arrange(Survey)
# pivot data to wide format
y <- m %>% pivot_wider(id_cols = Tag, names_from = Survey, values_from = nPond, values_fill = 0)
y

# create capture history as character string
y <- y %>% unite("ch", c(2:ncol(.)), remove = FALSE, sep = "")

# reduce dataframe to columns that we need
y <- y %>% select(Tag, ch) %>% data.frame()
head(y)

# substitute A, B, and C for 1, 2, and 3 (respectively) in the capture history
y$ch <- gsub("1","A", y$ch)
y$ch <- gsub("2","B", y$ch)
y$ch <- gsub("3","C", y$ch)
head(y)

# check if all capture histories are length = 15 (for each of our fifteen surveys at each site)
table(nchar(y$ch))

# bt will be our dataframe that we input into RMark
bt <- data.frame(ch = y$ch, 
                 freq = 1, 
                 tag = y$Tag)

# remove unneeded objects
rm(y,m,RIGHT)

# so we have df and bt dataframes
head(df)
head(bt)

# are we missing any data?
table(is.na(bt)) # no missing data = good!

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Check capture histories against our field data
# Look at our first individual
head(df %>% arrange(Tag), 13)
# Does the capture history ('ch' column in tern) make sense for toad 100? 101?
head(bt, 2)
# Stop here and make sure you understand the capture histories (the 'ch' column in bff)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# ---- 4) Fit Multi-State Models in RMark -----
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

# first, process the data for RMark
# input the model that we want in the 'model =' argument of the process.data function

# here are the list of models in RMark
setup.model(model = "") # 91 models

# As you can see, RMark includes lots of models that can be input into the model= argument below

# Here we use the "HCRDMS" model because we are interested in 
# estimating survival and dispersal, with abundance at each pond as a derived parameter 
# The H in HCRDMS stands for Huggins, which are models that condition abundance out of the likelihood
# such that capture probability can be modeled as a function of individual covariates
# (i.e., these models drop the f0 parameter)
# The C in HCRDMS stands for Closed, meaning that the population is assumed to be closed within primary periods
# The RD in HCRDMS stands for Robust Design
# And finally the MS in HCRDMS stands for Multi-State or Multi-Strata, so we know we are dealing with a model
# that includes multiple states within the capture history

# Visit this link for a full list of MARK models supported in RMark:
# https://github.com/jlaake/RMark/blob/master/RMark/inst/MarkModels.pdf

# specify time intervals for Robust Design
intervals <- c(
  0,0,0,0,0,0,0,0,1, # 2016
  0,0,0,0,0,1,       # 2017
  0,0,0,0,0,1,       # 2018
  0,0,0,0,1,         # 2019
  0,0)               # 2020

intervals

# now, process data in RMark format
d.proc=process.data(bt, 
                    model="HCRDMS",
                    time.intervals=intervals, 
                    begin.time = 2016)

# now create design data from the processed data
d.ddl <- make.design.data(d.proc)
# NOTE: the design dataframes are just as important as your raw data!

# Let's explore the design data
# see which parameters are estimated in the model
names(d.ddl)

d.ddl$S # survival probability

d.ddl$Psi # transition probability
# Psi represents the probability of transitioning between states

d.ddl$p # capture probability

d.ddl$c # recapture probability


# Some notes on our multi-state model

# 'States' in our models refer to individual breeding ponds and
# transition probabilities represent the probability of moving from
# one breeding site to another between seasons.

# This modeling approach assumes that no site transitions occurred
# within a breeding season

# In model parameterization, transition probability is conditional
# on survival (e.g. transition probability Psi(rst) represents the probability
# an individual in state r at time t survives and moves to state s at time t + 1).

# look at transitions in raw capture histories
transition.pairs(bt$ch)

# within years
transition.pairs(substr(bt$ch,1,9)) # 2016
transition.pairs(substr(bt$ch,10,15)) # 2017
transition.pairs(substr(bt$ch,16,21)) # 2018
transition.pairs(substr(bt$ch,22,26)) # 2019
transition.pairs(substr(bt$ch,27,29)) # 2020

# between each year
transition.pairs(substr(bt$ch,1,15)) # 2016-2017
transition.pairs(substr(bt$ch,10,21)) # 2017-2018
transition.pairs(substr(bt$ch,16,26)) # 2018-2019
transition.pairs(substr(bt$ch,22,29)) # 2019-2020


# we need to fix capture probabilities (and recapture probabilities) to zero for surveys that we didn't actually do
d.ddl$p$fix <- NA

# surveys that we did not complete at Pond A
d.ddl$p$fix[d.ddl$p$session == "2017" & d.ddl$p$time == "6" & d.ddl$p$stratum== "A"] <- 0
d.ddl$p$fix[d.ddl$p$session == "2018" & d.ddl$p$time == "6" & d.ddl$p$stratum== "A"] <- 0

# surveys that we did not complete at Pond B
d.ddl$p$fix[d.ddl$p$session == "2016" & d.ddl$p$time == "5" & d.ddl$p$stratum== "B"] <- 0
d.ddl$p$fix[d.ddl$p$session == "2016" & d.ddl$p$time == "6" & d.ddl$p$stratum== "B"] <- 0
d.ddl$p$fix[d.ddl$p$session == "2016" & d.ddl$p$time == "7" & d.ddl$p$stratum== "B"] <- 0
d.ddl$p$fix[d.ddl$p$session == "2016" & d.ddl$p$time == "8" & d.ddl$p$stratum== "B"] <- 0
d.ddl$p$fix[d.ddl$p$session == "2016" & d.ddl$p$time == "9" & d.ddl$p$stratum== "B"] <- 0
d.ddl$p$fix[d.ddl$p$session == "2017" & d.ddl$p$time == "5" & d.ddl$p$stratum== "B"] <- 0
d.ddl$p$fix[d.ddl$p$session == "2017" & d.ddl$p$time == "6" & d.ddl$p$stratum== "B"] <- 0
d.ddl$p$fix[d.ddl$p$session == "2018" & d.ddl$p$time == "5" & d.ddl$p$stratum== "B"] <- 0
d.ddl$p$fix[d.ddl$p$session == "2018" & d.ddl$p$time == "6" & d.ddl$p$stratum== "B"] <- 0

# now do the same for c
d.ddl$c

d.ddl$c$fix <- NA

# surveys that we did not complete at Pond A
d.ddl$c$fix[d.ddl$c$session == "2017" & d.ddl$c$time == "6" & d.ddl$c$stratum== "A"] <- 0
d.ddl$c$fix[d.ddl$c$session == "2018" & d.ddl$c$time == "6" & d.ddl$c$stratum== "A"] <- 0

# surveys that we did not complete at Pond B
d.ddl$c$fix[d.ddl$c$session == "2016" & d.ddl$c$time == "5" & d.ddl$c$stratum== "B"] <- 0
d.ddl$c$fix[d.ddl$c$session == "2016" & d.ddl$c$time == "6" & d.ddl$c$stratum== "B"] <- 0
d.ddl$c$fix[d.ddl$c$session == "2016" & d.ddl$c$time == "7" & d.ddl$c$stratum== "B"] <- 0
d.ddl$c$fix[d.ddl$c$session == "2016" & d.ddl$c$time == "8" & d.ddl$c$stratum== "B"] <- 0
d.ddl$c$fix[d.ddl$c$session == "2016" & d.ddl$c$time == "9" & d.ddl$c$stratum== "B"] <- 0
d.ddl$c$fix[d.ddl$c$session == "2017" & d.ddl$c$time == "5" & d.ddl$c$stratum== "B"] <- 0
d.ddl$c$fix[d.ddl$c$session == "2017" & d.ddl$c$time == "6" & d.ddl$c$stratum== "B"] <- 0
d.ddl$c$fix[d.ddl$c$session == "2018" & d.ddl$c$time == "5" & d.ddl$c$stratum== "B"] <- 0
d.ddl$c$fix[d.ddl$c$session == "2018" & d.ddl$c$time == "6" & d.ddl$c$stratum== "B"] <- 0


# okay, we are ready to specify and fit some models

#### Parameters for the analysis are:
names(d.ddl)

# Specify model 

# Capture probability
# allow capture probability to vary by year and in each pond
# set capture probability to equal recapture probability
head(d.ddl$p)
p.timexpond = list(formula=~session * stratum, share = TRUE)

# Survival 
# allow survival to vary over time and at each pond
# i.e., will estimate a different survival probability between each year and at each pond
head(d.ddl$S)
S.timexpond = list(formula =~time * stratum)

# Transition (dispersal) 
# To determine if individuals disperse from breeding ponds, we fit model that 
# allows for state transitions among all three breeding ponds and permit
# variation in movement probabilities across years. In other words, obtain
# year-specific estimates for the probability an individual moves from one breeding pond
# to each of the other two ponds. The probability of remaining at the same site is
# therefore estimated by subtraction (1 - cumulative probability of movement to
# other two sites).
head(d.ddl$Psi)
Psi.timexpond=list(formula=~-1+stratum:tostratum*time)

# Let's also try another model structure for transition (dispersal)
# To determine if individuals disperse from breeding ponds, fit model that 
# allows for state transitions among all three breeding ponds, but does not permit
# variation in movement probabilities across years.
head(d.ddl$Psi)
Psi.ponds=list(formula=~-1+stratum:tostratum)

# fit models

# you will need (want) an output folder for the mark files, just so your
# directory does not get cluttered
# Create a new folder called 'models' in your working directory
# set working directory to that folder
setwd("G:/Shared drives/wyo-coop-barrile/Boreal_Toad_Project/RS/Population_Modeling_Sp23/Week_3_Closed_Population_Estimation/models")

# to fit models, we need:
# our processed data (d.proc)
# our design data (d.ddl)
# model structures for p (capture probability), S (survival probability)
# and Psi (transition probability)

# fit models
# movement varies among ponds and over time
m.timexpond <- mark(d.proc,
                    d.ddl, 
                    model.parameters=list(S   = S.timexpond,
                                          Psi = Psi.timexpond,
                                          p   = p.timexpond))
# movement varies among ponds
m.pond <- mark(d.proc,
               d.ddl, 
               model.parameters=list(S   = S.timexpond,
                                     Psi = Psi.ponds,
                                     p   = p.timexpond))



# Collect models
pema <- collect.models(type = "HCRDMS") #grabs all models in workspace
names(pema) # 2 models
temp <- pema$model.table
( pema.ModSet <- temp[ ,-c(1:5)] )
pema.ModSet

for(i in 1:length(pema)){
  sing <- pema[[i]]$results$singular
  mod <- pema[[i]]$model.name
  print(mod)
  print(sing) # last will be model.table: NULL
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load function to check for singular parameters
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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
sing.out <- sing.check(pema)
sing.out # take a look
sing.out[sing.out$singular != "all converged", ] # mods with params that didn't converge
sing.out$mod.num[sing.out$singular != "all converged"] # just the mod numbers, which you'll need

# Create a new mod set without mods with convergence issues
bad.mods <- sing.out$mod.num[sing.out$singular != "all converged"] # just the bad mod #'s
model.list2 <- remove.mark(pema, bad.mods) # drop the bad mods via the mod #'s

# Look at AIC table
model.list2$model.table

AICtablep<- as.data.frame(model.list2$model.table)
AICtablep


# with just two models, we don't really need the above singularity code and AIC table
# we coudl instead just check whether the two models converged / had any singular params
m.timexpond$results$singular
m.pond$results$singular

# check model for beta issues
m.pond$results$beta

# check out real estimates
m.pond$results$real

# check out estimates of derived parameters (in this case, pond abundance in each year)
m.pond$results$derived$`N Population Size`

# obtain estimates for transition (i.e., movement) probabilities 
Psilist=get.real(m.pond,"Psi",vcv=TRUE)
# only need the estimates
Psivalues=Psilist$estimates
# only need estimates from a single cohort
# (because cohort was not in the model, so all cohorts have
# the same estimates)
Psivalues <- Psivalues[Psivalues$Cohort==0,]

# let's look at values for Psi
Psivalues # note that there is no AA, BB, CC

# create transition matrix
# use TransitionMatrix to get probability of staying in the same pond (i.e., fidelity) with SEs
a0 <- TransitionMatrix(Psivalues[Psivalues$Time==0,],vcv.real=Psilist$vcv.real)

# now we have a list 
a0

# grab mean estimates for transition probabilities
a1 <- a0[[1]]
# grab lower 95% confidence limit 
a2 <- a0[[3]]
# grab upper 95% confidence limit
a3 <- a0[[4]]
# do some rounding to simplify
a1 <- round(a1,3)
a2 <- round(a2,3)
a3 <- round(a3,3)

# look at mean estimates
a1

# format estimates a bit prior to plotting
# mean
tt0 <- t(a1)
est0 <- c(tt0)
# lcl
tt00 <- t(a2)
est00 <- c(tt00)
# ucl
tt000 <- t(a3)
est000 <- c(tt000)

# create df for plotting
dispersal <- data.frame(estimate=est0, 
                        lcl=est00, 
                        ucl=est000, 
                        Trans=c("AA","AB","AC",
                                "BA","BB","BC",
                                "CA","CB","CC"))

# create fidelity vs dispersal column
dispersal$Fid.Disp <- c("Fidelity","Dispersal","Dispersal",
                        "Dispersal","Fidelity","Dispersal",
                        "Dispersal","Dispersal","Fidelity")


# plot the values
plot <- ggplot(dispersal, aes(x=Trans, y=estimate, group=Fid.Disp, color=Fid.Disp)) +
  geom_errorbar(aes(ymin=lcl, ymax=ucl), 
                width=0.2, size=1.5, position=position_dodge(0.3)) +
  geom_point(position=position_dodge(0.3), stat="identity", size = 4) +
  xlab(NULL) +
  ylab("Transition probability \n") +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  theme_bw()+
  theme(
    legend.position = "none",
    #panel.border = element_blank(),
    panel.background = element_rect(colour = "black", size=1, linetype = "solid"),
    axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
    axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.y = element_text(size = 20, vjust = 1, color = "black"), # spacing from y
    axis.text.x = element_text(size = 18, color = "black"),
    axis.text.y = element_text(size = 12, color = "black")) +
  theme(legend.title = element_blank())+
  theme(legend.text = element_text(size = 17))+
  theme(legend.position="top")+
  theme(strip.text = element_text(size = 7))

# divide plot with lines so it's easier to visualize results
plot <- plot + geom_vline(xintercept = 3.5, linetype=2, size=0.25, col="grey8")
plot <- plot + geom_vline(xintercept = 6.5, linetype=2, size=0.25, col="grey8")

# show plot
plot



# let's plot temporal variation in survival probability at each pond over our study period
surv <- m.pond$results$real[1:12,c(1,3,4)]

# create better label for survival between years
surv$year <- c("2016-2017","2017-2018","2018-2019","2019-2020",
               "2016-2017","2017-2018","2018-2019","2019-2020",
               "2016-2017","2017-2018","2018-2019","2019-2020")
surv$year <- as.factor(surv$year)

# create labels for each pond
surv$Pond <- c("Pond A","Pond A","Pond A","Pond A",
               "Pond B","Pond B","Pond B","Pond B",
               "Pond C","Pond C","Pond C","Pond C")
surv$Pond <- as.factor(surv$Pond)

# use surv df to plot
surv

# plot the values
ggplot(surv, aes(x=year, y=estimate, group=Pond, color=Pond)) +
  geom_errorbar(aes(ymin=lcl, ymax=ucl), 
                width=0.2, size=1.25, position=position_dodge(0.3)) +
  geom_line(position=position_dodge(0.3), size=1.25) +
  geom_point(position=position_dodge(0.3), stat="identity", size = 6.5) +
  xlab(NULL) +
  ylab("Survival probability \n") +
  scale_y_continuous(limits = c(0.1, 1), breaks = seq(0, 1, 0.2), expand = c(0,0)) +
  theme_bw() +
  theme(#text = element_text(size = 18, family = "Times"), # controls all fonts in plot
    panel.background = element_rect(colour = "black", size=1, linetype = "solid"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks.length = unit(0.2,"cm"),
    #axis.title.x = element_text(size = 22, color = "black"),
    axis.title.y = element_text(size = 22, color = "black"),
    axis.text.x = element_text(size = 18, color = "black"),
    axis.text.y = element_text(size = 14, color = "black")) +
  theme(legend.title = element_blank())+
  theme(legend.text = element_text(size = 17))+
  theme(legend.position="top")+
  theme(strip.text = element_text(size = 7))





# let's plot abundance at each pond over our study period
abund <- m.pond$results$derived$`N Population Size`

# create label for year
abund$year <- c("2016","2017","2018","2019","2020",
               "2016","2017","2018","2019","2020",
               "2016","2017","2018","2019","2020")
abund$year <- as.factor(abund$year)

# create labels for each pond
abund$Pond <- c("Pond A","Pond A","Pond A","Pond A","Pond A",
               "Pond B","Pond B","Pond B","Pond B","Pond B",
               "Pond C","Pond C","Pond C","Pond C","Pond C")
abund$Pond <- as.factor(abund$Pond)

# use abund df to plot
abund

# plot the values
ggplot(abund, aes(x=year, y=estimate, group=Pond, color=Pond)) +
  geom_errorbar(aes(ymin=lcl, ymax=ucl), 
                width=0.2, size=1.25, position=position_dodge(0.3)) +
  geom_line(position=position_dodge(0.3), size=1.25) +
  geom_point(position=position_dodge(0.3), stat="identity", size = 6.5) +
  xlab(NULL) +
  ylab("Toad abundance \n") +
  # scale_y_continuous(limits = c(0.1, 1), breaks = seq(0, 1, 0.2), expand = c(0,0)) +
  theme_bw() +
  theme(#text = element_text(size = 18, family = "Times"), # controls all fonts in plot
    panel.background = element_rect(colour = "black", size=1, linetype = "solid"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks.length = unit(0.2,"cm"),
    #axis.title.x = element_text(size = 22, color = "black"),
    axis.title.y = element_text(size = 22, color = "black"),
    axis.text.x = element_text(size = 18, color = "black"),
    axis.text.y = element_text(size = 14, color = "black")) +
  theme(legend.title = element_blank())+
  theme(legend.text = element_text(size = 17))+
  theme(legend.position="top")+
  theme(strip.text = element_text(size = 7))


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# END
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#



