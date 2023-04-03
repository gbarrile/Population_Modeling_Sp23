
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# POPULATION MODELING IN ECOLOGY
# ZOO 5890-09
# University of Wyoming
# Spring 2023

# Week 11 - Reverse-time Pradel models to estimate survival, recruitment, and population growth

# Description: Steps through an example analysis using functions in the package `RMark`.

# Good reference to check out:

# Pradel, R. (1996). Utilization of capture-mark-recapture for the study of recruitment 
# and population growth rate. Biometrics, 703-709. 

# The code below addresses the question: 
# Does annual precipitation and rangeland health influence the survival 
# and recruitment of black-footed ferrets at reintroduction sites?

# Last week we estimated/predicted ferret survival across the landscape to identify
# potential reintroduction sites

# Why might it be important to also consider recruitment, not just survival, when
# considering potential reintroduction sites?

# Gabe Barrile - University of Wyoming
# Last updated 04/03/2023
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# ---- Outline -----
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# This script contains the following sections:
# 1) Install and load packages
# 2) Read in field data
# 3) Format data for RMark
# 4) Fit Reverse-time Pradel models
# 5) Prediction and plotting
# 6) Alternate parameterizations and finite-mixture models
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

# you will need to set working directory to where you saved the 'Ferrets_CMR.csv' and the 'Site_Precip.csv'
setwd()

# read-in the black-footed ferret capture-mark-recapture data
setwd("G:/Shared drives/wyo-coop-barrile/Boreal_Toad_Project/RS/Population_Modeling_Sp23/Week_8_Robust_TempEmig_RMark/data")

# read-in data from the csv
df <- read.csv("Ferrets_CMR.csv")

# take a peek at the data
head(df)

# first, order the data frame by Site, Tag, Primary, Secondary
df <- df %>% arrange(Site, Tag, Primary, Secondary)
head(df)

# how many sites did we sample?
n_distinct(df$Site) # 4 sites
unique(df$Site) 

# how many ferrets did we capture and mark?
n_distinct(df$Tag) # 235 ferrets

# how many unique individuals were captured at each site?
df %>% 
  group_by(Site) %>% 
  summarise(n=n_distinct(Tag)) %>% 
  rename(Number.of.Individuals=n) %>% 
  data.frame()

# for how many years did we sample?
n_distinct(df$Year) # 5 years
unique(df$Year) 



# Now, let's create variable that gives a unique identifier to each primary-secondary survey combinations
# first paste primary and secondary surveys together in new column
df$PrimSec <- paste(df$Primary, df$Secondary, sep = "-")
# order by primary survey (this is not absolutely necessary but I recommend doing it)
df <- df %>% arrange(Primary)
# create new variable with unique identifier for each primary-secondary survey combinations
df <- df %>% group_by(PrimSec) %>% mutate(Total.Survey=cur_group_id())
table(df$PrimSec, df$Total.Survey)
# remove PrimSec column (make sure you do this)
df$PrimSec <- NULL
# now reorder by Site, Tag, Primary, Secondary to see what we did
df <- df %>% arrange(Site, Tag, Primary, Secondary)

# how many surveys per site?
df %>% 
  group_by(Site) %>% 
  summarise(n=n_distinct(Total.Survey)) %>% 
  rename(Surveys=n) %>% 
  data.frame()

# how many surveys per site per year?
df %>% 
  group_by(Site, Year) %>% 
  summarise(n=n_distinct(Total.Survey)) %>% 
  rename(Surveys=n) %>% 
  data.frame()


# So, it seems we have capture-mark-recapture data for black-footed ferrets
# at four reintroduction sites, and we surveyed each location three times
# per year for five years (2015-2019)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# ---- 3) Format data for RMark -----
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

# first, remove duplicates (if the same individual was captured twice or more during the same survey)
# identify duplicates
df %>%
  dplyr::group_by(Tag, Total.Survey) %>%
  dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
  dplyr::filter(n > 1L)

# remove duplicates
df <- df %>% distinct(Tag, Total.Survey, .keep_all = TRUE)



# next, create capture histories for each individual

# first, reduce dataframe to columns that we need
m <- df %>% select(Tag, Total.Survey)
# create a variable with a '1' for each time an individual was captured
m$Capture <- rep(1,nrow(m))
# make Tag a factor variable (possibly unnecessary but probably good practice)
m$Tag <- as.factor(as.character(m$Tag))
# order the data frame by Total.Survey
m <- m %>% arrange(Total.Survey)
# pivot data to wide format
y <- m %>% pivot_wider(id_cols = Tag, names_from = Total.Survey, values_from = Capture, values_fill = 0)
y

# create capture history as character string
y <- y %>% unite("ch", c(2:ncol(.)), remove = FALSE, sep = "")

# reduce dataframe to columns that we need
y <- y %>% select(Tag, ch) %>% data.frame()
head(y)

# check if all capture histories are length = 15 (for each of our fifteen surveys at each site)
table(nchar(y$ch))

# add variables to 'y' dataframe

# Location: national park or developed for energy extraction
y$Site <- df$Site[match(y$Tag, df$Tag)]

# bff will be our dataframe that we input into RMark
bff <- data.frame(ch = y$ch, 
                 freq = 1, 
                 site = y$Site, 
                 tag = y$Tag)

# remove unneeded objects
rm(y,m)

# so we have df and bff dataframes
head(df)
head(bff)

# make site a factor variable
bff$site <- as.factor(as.character(bff$site))
table(bff$site)

# are we missing any data?
table(is.na(bff)) # no missing data = good!

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Check capture histories against our field data
# Look at our first individual
head(df %>% arrange(Site,Tag), 15)
# Does the capture history ('ch' column in tern) make sense for ferret 400? 401? 402?
head(bff, 3)
# Stop here and make sure you understand the capture histories (the 'ch' column in bff)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Add precipitation data to the bff dataframe
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# read-in precip data
ppt <- read.csv("Site_Precip.csv")

# take a peek at the data
ppt

# match up precipitation with capture data

# order the precip data frame by Year
ppt <- ppt %>% arrange(Year)
# pivot data to wide format
ppt_wide <- ppt %>% pivot_wider(id_cols = Site, names_from = Year, values_from = Precipitation, values_fill = NA)

# match precip data with bff cmr data
head(bff)
bff$ppt2015 <- ppt_wide$`2015`[match(bff$site, ppt_wide$Site)]
bff$ppt2016 <- ppt_wide$`2016`[match(bff$site, ppt_wide$Site)]
bff$ppt2017 <- ppt_wide$`2017`[match(bff$site, ppt_wide$Site)]
bff$ppt2018 <- ppt_wide$`2018`[match(bff$site, ppt_wide$Site)]
bff$ppt2019 <- ppt_wide$`2019`[match(bff$site, ppt_wide$Site)]

rm(ppt,ppt_wide)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Add range health data to the bff dataframe
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# read-in range health data
rh <- read.csv("Site_Health.csv")

# take a peek at the data
rh

# match up rh with capture data
head(bff)
bff$rh <- rh$RangeHealth[match(bff$site, rh$Site)]

# check that it matched correctly
table(bff$site, bff$rh)

rm(rh)

head(bff)

# Note: RMark column names cannot be longer than ten characters!!



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# ---- 4) Fit Reverse-Time Pradel Models in RMark -----
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

# first, process the data for RMark
# input the model that we want in the 'model =' argument of the process.data function

# here are the list of models in RMark
setup.model(model = "") # 91 models

# Gamma (seniority) parameterizations
# RDPdGClosed
# RDPdGHuggins
# RDPdGHet
# RDPdGFullHet
# RDPdGHugHet
# RDPdGHugFullHet

# Lambda (L) parameterizations
# RDPdLClosed
# RDPdLHuggins
# RDPdLHet
# RDPdLFullHet
# RDPdLHugHet
# RDPdLHugFullHet

# Recruitment rate (f) parameterizations
# RDPdfClosed
# RDPdfHuggins
# RDPdfHet
# RDPdfFullHet
# RDPdfHugHet
# RDPdfHugFullHet

# As you can see, RMark includes lots of models that can be input into the model= argument below

# Here we use the "RDPdfHuggins" model because we are interested in 
# estimating survival and recruitment, with population growth as a derived parameter 
# The RD in RDPdfHuggins stands for Robust Design
# The Pd stands for Pradel, so we know we are dealing with a reverse-time model
# The f refers to the recruitment parameterization, rather than L (Lambda) or G (Gamma) params
# And finally recall that Huggins models condition abundance out of the likelihood
# such that capture probability can be modeled as a function of individual covariates
# (i.e., these models drop the f0 parameter)

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
d.proc=process.data(bff, 
                    model="RDPdfHuggins",
                    time.intervals=intervals, 
                    groups = c("site"), 
                    begin.time = 2015)

# look at processed data
head(d.proc$data)

# create design data
d.ddl <- make.design.data(d.proc)
# NOTE: the design dataframes are just as important as your raw data!

# Let's explore the design data
# see which parameters are estimated in the model
names(d.ddl)

d.ddl$Phi # apparent survival probability

d.ddl$f # recruitment rate
# The recruitment rate (f) represents the number of individuals added 
# to the breeding population at time t + 1 per animal in the breeding
# population at time t (per capita recruitment rate)

d.ddl$p # capture probability

d.ddl$c # recapture probability

# Results from last week's class suggested that capture probability at our northern
# sites (MT and WY) were similar (~0.43) and higher than capture probability at
# our southern sites (CO and NM), which was ~0.15.
# Thus, let's simplify our spatial variable for capture probability into two groups
# (i.e., northern sites versus southern sites) rather than four sites

d.ddl$p
d.ddl$p$NS <- NA
d.ddl$p$NS[d.ddl$p$site == "MT"] <- "North"
d.ddl$p$NS[d.ddl$p$site == "WY"] <- "North"
d.ddl$p$NS[d.ddl$p$site == "CO"] <- "South"
d.ddl$p$NS[d.ddl$p$site == "NM"] <- "South"
d.ddl$p$NS <- as.factor(d.ddl$p$NS)

d.ddl$c
d.ddl$c$NS <- NA
d.ddl$c$NS[d.ddl$c$site == "MT"] <- "North"
d.ddl$c$NS[d.ddl$c$site == "WY"] <- "North"
d.ddl$c$NS[d.ddl$c$site == "CO"] <- "South"
d.ddl$c$NS[d.ddl$c$site == "NM"] <- "South"
d.ddl$c$NS <- as.factor(d.ddl$c$NS)


# okay, we are ready to specify and fit some models

# Again, the parameters for the analysis are:
names(d.ddl)

# let's assume that capture and recapture probabilities are the same for this lesson
# and let's let capture probability vary by northern versus southern sites
# now let's create some structures for capture probability
head(d.ddl$p)

# specify formula for site
pc.site = list(formula=~NS, share = TRUE)

# let's test our original hypothesis by allowing survival and recruitment
# to vary by annual precipitation and rangeland health
head(d.ddl$S)
head(d.proc$data)

# specify formula for precipitation
covmod = list(formula=~ppt * rh)

# also specify a formula for space and time
stmod = list(formula=~site * time)


# fit models

# you will need (want) an output folder for the mark files, just so your
# directory does not get cluttered
# Create a new folder called 'models' in your working directory
# set working directory to that folder
setwd("G:/Shared drives/wyo-coop-barrile/Boreal_Toad_Project/RS/Population_Modeling_Sp23/Week_3_Closed_Population_Estimation/models")

# to fit models, we need:
# our processed data (d.proc)
# our design data (d.ddl)
# model structures for p (capture probability), Phi (survival probability)
# and f (recruitment rate)

# fit models
# precipitation * rangeland health for both survival and recruitment
m.precip <- mark(d.proc, d.ddl, model.parameters=list(p   = pc.site, 
                                                      Phi = covmod, 
                                                      f   = covmod))

# space * time for both survival and recruitment
m.st <- mark(d.proc, d.ddl, model.parameters=list(p   = pc.site, 
                                                  Phi = stmod, 
                                                  f   = stmod))

# look at model output from covariate model

# you can again check for any singular parameters
m.precip$results$singular

# beta coefficients
m.precip$results$beta

# real estimates
m.precip$results$real

# derived parameters (abundance at each site in each year)
m.precip$results$derived$`N Population Size`

# derived parameters (population growth rate (lambda))
m.precip$results$derived$`Lambda Population Change`



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# ---- 5) Prediction and plotting -----
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

# create plots

# just an fyi from covariate.predictions help file:
# "if data is not specified or all individual covariate values are not specified, 
# the mean individual covariate value is used for prediction."

# so we do not need to manually hold each predictor at mean values (those not being predicted),
# because covariate.predictions does that automatically. 

# plot the interactive effect of precipitation and range health on ferret survival

# ppt
names(bff)
range(bff[,5:9])
# create sequence of values to plot over
x2 <- seq(50,1400,length.out=100)

# rh
names(bff)
range(bff[,10])
# create sequence of values to plot over
y2 <- seq(0,1,length.out=100)

m.precip$pims$Phi

# predict 
pred.matrix1 <- array(NA, dim = c(100, 100)) # Define arrays
for(i in 1:100){
  for(j in 1:100){
    newData1 <- data.frame(ppt2015=x2[i], rh=y2[j])       
    pred <- covariate.predictions(m.precip, data=newData1, indices=1)$estimates
    pred.matrix1[i, j] <- pred$estimate
  }
}

# plot the values
par(mfrow = c(1,1), cex.lab = 1.2)
mapPalette <- colorRampPalette(c("grey", "yellow", "orange", "red"))
image(x=x2, y=y2, z=pred.matrix1, col = mapPalette(100), 
      xlab = "Total Annual Precipitation (mm)", ylab = "Range Health")
contour(x=x2, y=y2, z=pred.matrix1, add = TRUE, lwd = 1, 
        col = "blue", labcex = 1, method = "flattest")
box()
title(main = "Expected survival probability", font.main = 1)


# plot with ggplot
xy <- expand.grid(ppt2015=x2, rh=y2)

predictis=covariate.predictions(m.precip, data=xy, indices=1)$estimates
head(predictis)

# use color-blind friendly fill for plot
library(viridis)

ggplot(predictis, aes(ppt2015, rh, fill= estimate)) + 
  geom_tile() +
  scale_fill_viridis(discrete=FALSE) +
  guides(fill = guide_colourbar(title = "Survival"))




# plot the interactive effect of precipitation and range health on ferret recruitment
m.precip$pims$f

# ppt
names(bff)
range(bff[,5:9])
# create sequence of values to plot over
x2 <- seq(50,1400,length.out=100)

# rh
names(bff)
range(bff[,10])
# create sequence of values to plot over
y2 <- seq(0,1,length.out=100)

# predict 
pred.matrix1 <- array(NA, dim = c(100, 100)) # Define arrays
for(i in 1:100){
  for(j in 1:100){
    newData1 <- data.frame(ppt2015=x2[i], rh=y2[j])      
    pred <- covariate.predictions(m.precip, data=newData1, indices=17)$estimates
    pred.matrix1[i, j] <- pred$estimate
  }
}

# plot the values
par(mfrow = c(1,1), cex.lab = 1.2)
mapPalette <- colorRampPalette(c("grey", "yellow", "orange", "red"))
image(x=x2, y=y2, z=pred.matrix1, col = mapPalette(100), 
      xlab = "Total Annual Precipitation (mm)", ylab = "Range Health")
contour(x=x2, y=y2, z=pred.matrix1, add = TRUE, lwd = 1, 
        col = "blue", labcex = 1, method = "flattest")
box()
title(main = "Expected recruitment rate", font.main = 1)


# plot with ggplot
xy <- expand.grid(ppt2015=x2, rh=y2)

predictis=covariate.predictions(m.precip, data=xy, indices=17)$estimates
head(predictis)

# use color-blind friendly fill for plot
library(viridis)

ggplot(predictis, aes(ppt2015, rh, fill= estimate)) + 
  geom_tile() +
  scale_fill_viridis(discrete=FALSE) +
  guides(fill = guide_colourbar(title = "Recruitment"))





# look at model output from spatiotemporal model

# you can again check for any singular parameters
m.st$results$singular

# real estimates
m.st$results$real

# derived parameters (abundance at each site in each year)
m.st$results$derived$`N Population Size`

# derived parameters (population growth rate (lambda))
m.st$results$derived$`Lambda Population Change`

# Recall that survival + recruitment = lambda (discrete population growth)
m.st$results$real
surv <- data.frame(Site = rep(c("CO","MT","NM","WY"), each=4),
                   Year = rep(c("15-16","16-17","17-18","18-19"), times=4),
                   Value = m.st$results$real[c(1:16),c(1)],
                   Variable = "Survival",
                   ybegin = m.st$results$derived$`Lambda Population Change`$lcl,
                   yend = m.st$results$derived$`Lambda Population Change`$ucl)
  

m.st$results$real
rec <- data.frame(Site = rep(c("CO","MT","NM","WY"), each=4),
                   Year = rep(c("15-16","16-17","17-18","18-19"), times=4),
                   Value = m.st$results$real[c(17:32),c(1)],
                   Variable = "Recruitment",
                   ybegin = m.st$results$derived$`Lambda Population Change`$lcl,
                   yend = m.st$results$derived$`Lambda Population Change`$ucl)

# combine dfs
survrec <- rbind(surv, rec)

# use 'survrec' for plotting

# plot survival, recruitment, and lambda 
popgrowth <- ggplot(survrec, aes(x = Year, y = Value, fill = Variable)) + 
  geom_bar(stat = "identity", color = "white", size = 0.1) +
  facet_wrap(~Site,nrow = 1)+
  scale_fill_manual(values = c("grey8","grey55")) +
  geom_errorbar(aes(ymax=ybegin , ymin=yend), width = 0.3, 
                color = "black", size = 0.5) +
  xlab(NULL) +
  ylab(expression(paste("Population Growth Rate (", lambda, ")"))) +
  scale_y_continuous(limits = c(0, 2.8), breaks = seq(0, 2.8, 0.2)) +
  theme_bw() +
  theme(#text = element_text(size = 18, family = "Times"), # controls all fonts in plot
    #panel.border=element_rect(size=1, color="black"),
    panel.background = element_rect(colour = "black", linewidth=1, linetype = "solid"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.y = element_text(size = 20, color = "black"), # spacing from y
    axis.text.x = element_text(size = 16, color = "black"),
    axis.text.y = element_text(size = 14, color = "black")) +
  theme(legend.title = element_blank())+
  theme(legend.text = element_text(size = 18))+
  theme(strip.text = element_text(size = 18))+
  theme(axis.text.x=element_text(size = 16))+
  theme(axis.text.x=element_text(angle=45, hjust=1))

# change legend specs
popgrowth <- popgrowth + theme(legend.position = c(0.35, 0.92))
popgrowth <- popgrowth + theme(legend.key.size = unit(1.5,"line"))
popgrowth <- popgrowth + theme(#legend.justification = "bottom",
  legend.spacing.x = unit(1, "pt"),
  #legend.spacing.y = unit(0, "pt"),
  legend.margin=margin(c(-5,-5,-5,-5)))

# include horizontal line at Lambda = 1 to show where population is stable
popgrowth <- popgrowth + geom_hline(yintercept = 1, linetype=2, size=0.25, col="grey8")

popgrowth

# Reminder:
# In the figure, survival is apparent because we cannot separate mortality from emigration
# and for recruitment, we cannot distinguish between births (in situ rec.) and immigration

# Do we care if animals are leaving the population through mortality versus emigration when
# thinking about population growth at a reintroduction site?

# Do we care if animals are entering the population through immigration versus births when
# thinking about population growth at a reintroduction site?


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# ---- 6) Alternate parameterizations and finite-mixture models -----
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

# Different recruitment rate parameterizations
# RDPdfClosed
# RDPdfHuggins
# RDPdfHet
# RDPdfFullHet
# RDPdfHugHet
# RDPdfHugFullHet


# RDPdfClosed
# 1. survival (Phi)
# 2. recruitment (f)
# 3. capture probability (p)
# 4. recapture probability (c)
# 5. number of individuals never captured (f0)

# RDPdfHuggins
# 1. survival (Phi)
# 2. recruitment (f)
# 3. capture probability (p)
# 4. recapture probability (c)


# FINITE MIXTURE MODELS (account for individual heterogeneity in capture probabilities)

# RDPdfHet
# 1. survival (Phi)
# 2. recruitment (f)
# 3. capture probability (p)
# 4. number of individuals never captured (f0)
# 5. pi (probability that an animal is a member of a certain capture probability grouping)

# RDPdfFullHet
# 1. survival (Phi)
# 2. recruitment (f)
# 3. capture probability (p)
# 4. recapture probability (c)
# 5. number of individuals never captured (f0)
# 6. pi (probability that an animal is a member of a certain capture probability grouping)

# RDPdfHugHet
# 1. survival (Phi)
# 2. recruitment (f)
# 3. capture probability (p)
# 4. pi (probability that an animal is a member of a certain capture probability grouping)


# RDPdfHugFullHet
# 1. survival (Phi)
# 2. recruitment (f)
# 3. capture probability (p)
# 4. recapture probability (c)
# 5. pi (probability that an animal is a member of a certain capture probability grouping)


# as an example, process data in RMark format
d.proc=process.data(bff, 
                    model="RDPdfFullHet",
                    time.intervals=intervals, 
                    groups = c("site"), 
                    begin.time = 2015)

# look at processed data
head(d.proc$data)

# create design data
d.ddl <- make.design.data(d.proc)
# NOTE: the design dataframes are just as important as your raw data!

# Let's explore the design data
# see which parameters are estimated in the model
names(d.ddl)

d.ddl$p # now includes a 'mixture' column


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# END
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#





