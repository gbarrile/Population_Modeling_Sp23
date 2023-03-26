
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# POPULATION MODELING IN ECOLOGY
# ZOO 5890-09
# University of Wyoming
# Spring 2023

# Week 10 - Robust Design Model to estimate survival while accounting for temporary emigration

# Description: Steps through an example analysis using functions in the package `RMark`.

# Good reference to check out:

# Kendall, W. L., Nichols, J. D., & Hines, J. E. (1997). 
# Estimating temporary emigration using capture–recapture data 
# with Pollock’s robust design. Ecology, 78(2), 563-578. 

# The code below addresses the question: 
# Does annual precipitation influence the survival of black-footed ferrets at reintroduction sites?

# Gabe Barrile - University of Wyoming
# Last updated 03/27/2023
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# ---- Outline -----
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# This script contains the following sections:
# 1) Install and load packages
# 2) Read in field data
# 3) Format data for RMark
# 4) Fit Robust Design models
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


# add precipitation data to the bff dataframe

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

# Note: column names in RMark cannot be longer than ten characters!!



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# ---- 4) Fit Robust Design Models with Temporary Emigration in RMark -----
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

# first, process the data for RMark
# input the model that we want in the 'model =' argument of the process.data function

# here are the list of models in RMark
setup.model(model = "") # 91 models

# As you can see, RMark includes lots of models that can be input into the model= argument below
# Here we use the "RDHuggins" model because we are interested in 
# estimating survival while accounting for temporary emigration.
# The RD in RDHuggins stands for Robust Design
# And recall that Huggins models condition abundance out of the likelihood
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
                    model="RDHuggins",
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

d.ddl$S # survival probability

d.ddl$GammaDoublePrime # temporary emigration
# the probability that an animal that was in the local population at time i 
# (i.e, available for capture) moves off and is unavailable for capture at i+1 (but is alive)

d.ddl$GammaPrime # temporary emigration
# the probability that an animal that was not in the local population at i 
# (and is unavailable for capture) remains outside the local population
# (and unavailable) at i+1 . Again, we assume this animal is alive.

# In other words...
# GammaDoublePrime = animal moves off of study area 
# VS.
# GammaPrime = animal stays off of study area given it moved off previously

d.ddl$p # capture probability

d.ddl$c # recapture probability


# Let's say we were not able to conduct a third survey at our site
# in Colorado during 2016

# we would need to fix capture (and recapture) probabilities to zero for this survey
# capture probability first
d.ddl$p$fix <- NA

d.ddl$p$fix[d.ddl$p$session == "2016" & d.ddl$p$time == "3" & d.ddl$p$site == "CO"] <- 0

# now do the same thing for recapture probability
d.ddl$c$fix <- NA

d.ddl$c$fix[d.ddl$c$session == "2016" & d.ddl$c$time == "3" & d.ddl$c$site == "CO"] <- 0


# remove objects that we don't need
rm(intervals)

# To avoid confounding gamma parameters (if you want to fit a fully time-dependent model
# for temporary emigration), the last two gamma'' and the last two gamma' values should be
# set to equal one another

d.ddl$GammaDoublePrime
d.ddl$GammaDoublePrime$tbinf <- NA
d.ddl$GammaDoublePrime$tbinf[d.ddl$GammaDoublePrime$time == "2015"] <- "2015"
d.ddl$GammaDoublePrime$tbinf[d.ddl$GammaDoublePrime$time == "2016"] <- "2016"
d.ddl$GammaDoublePrime$tbinf[d.ddl$GammaDoublePrime$time == "2017"] <- "2017"
d.ddl$GammaDoublePrime$tbinf[d.ddl$GammaDoublePrime$time == "2018"] <- "2017"
d.ddl$GammaDoublePrime$tbinf <- as.factor(d.ddl$GammaDoublePrime$tbinf)

# do the same for GammaPrime
d.ddl$GammaPrime
d.ddl$GammaPrime$tbinf <- NA
# Note that there is no 2015 for GammaPrime. Why do you think that is?
d.ddl$GammaPrime$tbinf[d.ddl$GammaPrime$time == "2016"] <- "2016"
d.ddl$GammaPrime$tbinf[d.ddl$GammaPrime$time == "2017"] <- "2017"
d.ddl$GammaPrime$tbinf[d.ddl$GammaPrime$time == "2018"] <- "2017"
d.ddl$GammaPrime$tbinf <- as.factor(d.ddl$GammaPrime$tbinf)


# okay, we are ready to specify and fit some models

# Again, the parameters for the analysis are:
names(d.ddl)

# let's assume that capture and recapture probabilities are the same for this lesson
# and let's let capture probability vary by site
# now let's create some structures for capture probability
head(d.ddl$p)

# specify formula for site
pc.site = list(formula=~site, share = TRUE)

# let's test our original hypothesis by allowing survival to vary by annual precipitation
head(d.ddl$S)
head(d.proc$data)

# specify formula for precipitation
S.ppt = list(formula=~ppt)

# next, holding our formulas for capture probability and survival the same,
# let's try some different structures for temporary emigration

# We will compare models with no emigration, random emigration, and Markovian emigration
# If temporary emigration is random, then GammaDoublePrime = GammaPrime
d.ddl$GammaDoublePrime

# Random Emigration (share the gamma parameters)
# 1. constant
GS. = list(formula=~1, share = TRUE)
# 2. time
GS.t = list(formula=~tbinf, share = TRUE)
# 3. space
GS.s = list(formula=~site, share = TRUE)
# 4. time + space
GS.ts = list(formula=~tbinf + site, share = TRUE)
# 5. time * space
GS.txs = list(formula=~tbinf * site, share = TRUE)

# Markovian Emigration (DO NOT share the gamma parameters)
# 1. constant
GDP. = list(formula=~1)
GP. = list(formula=~1)
# 2. time
GDP.t = list(formula=~tbinf)
GP.t = list(formula=~tbinf)
# 3. space
GDP.s = list(formula=~site)
GP.s = list(formula=~site)
# 4. time + space
GDP.ts = list(formula=~tbinf + site)
GP.ts = list(formula=~tbinf + site)
# 5. time * space
GDP.txs = list(formula=~tbinf * site)
GP.txs = list(formula=~tbinf * site)


# No Temporary Emigration
# 17. no temp emig
GDP.no = list(formula =~1, fixed = 0)
GP.no = list(formula =~1, fixed = 0)



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
# and the temporary emigration parameters (gamma parameters)

# Random Emigration models
# constant
rand. <- mark(d.proc, d.ddl, model.parameters=list(S = S.ppt, GammaDoublePrime = GS., p = pc.site))
# time
rand.t <- mark(d.proc, d.ddl, model.parameters=list(S = S.ppt, GammaDoublePrime = GS.t, p = pc.site))
# space
rand.s <- mark(d.proc, d.ddl, model.parameters=list(S = S.ppt, GammaDoublePrime = GS.s, p = pc.site))
# time + space
rand.ts <- mark(d.proc, d.ddl, model.parameters=list(S = S.ppt, GammaDoublePrime = GS.ts, p = pc.site))
# time * space
rand.txs <- mark(d.proc, d.ddl, model.parameters=list(S = S.ppt, GammaDoublePrime = GS.txs, p = pc.site))

# Markovian Emigration models
# constant
marko. <- mark(d.proc, d.ddl, model.parameters=list(S = S.ppt, GammaDoublePrime = GDP., GammaPrime= GP., p = pc.site))
# time
marko.t <- mark(d.proc, d.ddl, model.parameters=list(S = S.ppt, GammaDoublePrime = GDP.t, GammaPrime= GP.t, p = pc.site))
# space
marko.s <- mark(d.proc, d.ddl, model.parameters=list(S = S.ppt, GammaDoublePrime = GDP.s, GammaPrime= GP.s, p = pc.site))
# time + space
marko.ts <- mark(d.proc, d.ddl, model.parameters=list(S = S.ppt, GammaDoublePrime = GDP.ts, GammaPrime= GP.ts, p = pc.site))
# time * space
marko.txs <- mark(d.proc, d.ddl, model.parameters=list(S = S.ppt, GammaDoublePrime = GDP.txs, GammaPrime= GP.txs, p = pc.site))

# No Temporary Emigration model
no.emig <- mark(d.proc, d.ddl, model.parameters=list(S = S.ppt, GammaDoublePrime = GDP.no, GammaPrime= GP.no, p = pc.site))


#################################
# Collect models
#################################

#Collect models to create AIC table 
mods <- collect.models(type = "RDHuggins") #grabs all models in workspace
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
AICtable <- as.data.frame(model.list2$model.table)
AICtable

# save top model as object
top.aic <- model.list2$rand.


# look at model output

# you can again check for any singular parameters
top.aic$results$singular

# beta coefficients
top.aic$results$beta

# real estimates
top.aic$results$real

# derived parameters (abundance at each site in each year)
top.aic$results$derived$`N Population Size`


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# ---- 5) Plotting and predictions -----
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# let's plot the relationship between precipitation and survival

# need to specify the indices correctly (very important!)
top.aic$pims$S # let's use indices = 1

# create sequence of values to predict over
head(bff)
range(bff[,5:9])
# notice that we specify ppt2015
newdat <- data.frame(ppt2015=seq(90, 1350, length.out = 80))

# predict to newdata
pred.cov <- covariate.predictions(top.aic, data=newdat, indices=c(1))$estimates

# okay, plot it
min(pred.cov$lcl)
max(pred.cov$ucl)
op <- par(mar = c(5,5,4,2) + 0.1) # default is 5,4,4,2
plot(x = pred.cov$covdata, y = pred.cov$estimate, pch=16, 
     ylab = "Annual Survival Probability",
     xlab = "Total Annual Precipitation (mm)", cex.lab=1.5, cex.axis=1.2, 
     col="darkgray", ylim=c(0,1))
box(lwd = 4, col = 'black')
lines(pred.cov$covdata, pred.cov$estimate, lwd=8, col="blue")
lines(pred.cov$covdata, pred.cov$lcl, lwd=4, lty=2, col="black")
lines(pred.cov$covdata, pred.cov$ucl, lwd=4, lty=2, col="black")




# let's visualize abundance over time at each site
popsize <- top.aic$results$derived$`N Population Size`

# add a column for year
popsize$Year <- rep(c("2015","2016","2017","2018","2019"), times=4)

# add a column for site
d.proc$group.covariates
popsize$Site <- c(rep("CO", times=5),
                  rep("MT", times=5),
                  rep("NM", times=5),
                  rep("WY", times=5))

# turn both columns into factors
popsize$Year <- as.factor(popsize$Year)
popsize$Site <- as.factor(popsize$Site)

popsize

# maybe also have a column for year as a numeric variable
popsize$Year2 <- as.numeric(as.character(popsize$Year))

# create plot
min(popsize$lcl)
max(popsize$ucl)

ggplot(popsize, aes(x=Year, y=estimate, group=Site, color=Site)) +
  geom_point(size=5) + geom_line(linewidth=2) +
  # geom_errorbar(aes(ymin = lcl, ymax = ucl), width=.05, size=0.5) +
  xlab("Year") +
  ylab("Abundance \n") +
  #scale_y_continuous(limits = c(5, 140), breaks = seq(0, 140, 20), expand = c(0,0)) +
  #scale_x_discrete(expand = expand_scale(mult = c(0.45,0.55))) +
  # theme_bw() +
  theme(#text = element_text(size = 18, family = "Times"), # controls all fonts in plot
    panel.background = element_rect(colour = "black", size=1, linetype = "solid"),
    #axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
    #axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"),
    #panel.grid.major = element_blank(),
    #panel.grid.minor = element_blank(),
    axis.ticks.length = unit(0.2,"cm"),
    axis.title.y = element_text(size = 22, vjust = 1, color = "black"),
    axis.title.x = element_text(size = 22, vjust = 1, color = "black"),
    axis.text.x = element_text(size = 16, color = "black"),
    axis.text.y = element_text(size = 16, color = "black"))






# predict back to the landscape to identify promising reintroduction sites

# use raster for average precipitation across the range of potential sites

# set working directory to the data folder for this week
setwd("G:/Shared drives/wyo-coop-barrile/Boreal_Toad_Project/RS/Population_Modeling_Sp23/Week_8_Robust_TempEmig_RMark/data")

# read-in the precip raster
library(raster)
avgppt <- raster("avgprecip.tif")
res(avgppt) # 500 m x 500 m resolution (grid size)
crs(avgppt) # +proj=utm +zone=13 +datum=NAD83 +units=m +no_defs 

# plot the raster
plot(avgppt)

# get min value for masking later
min(values(avgppt), na.rm = TRUE)

# convert nas to 0 in raster
table(is.na(values(avgppt)))
avgppt[is.na(avgppt)] <- 0

# aggregate raster to larger resolution
avgppt <- aggregate(avgppt, fact=50)
res(avgppt) # we now have a 25000 m x 25000 m resolution (grid size)

# plot again
plot(avgppt)

# visulaize with mapview
library(mapview)
mapview(avgppt)

# change name of raster layer
names(avgppt) <- c("avgppt")

# turn raster into data frame
dat <- as.data.frame(avgppt, xy=TRUE)

# any nas?
table(is.na(dat))

# look at dat df
head(dat)

# Get predictions of survival probability for each precip value in the data
# could use ppt2015, ppt2016, ppt2017, ppt2018...but need to specify the correct indices in covariate.predictions (see below)
newData <- data.frame(ppt2015 = dat$avgppt)

# create a loop for large datasets as they may take up too much memory in your computer if only using one iteration
newData$uniq <- rep(1:1000, each=1000)[1:nrow(newData)] 

# setup for loop
dataoutput <- data.frame()
for(x in unique(newData$uniq)){
  data1 <- newData %>% filter(uniq == x)
  data <- covariate.predictions(top.aic,data=data1,indices=1)$estimates # still using the covariate.predictions functions, just like above
  temp<-data
  # Append data to create dataframe
  dataoutput <- rbind(dataoutput, temp)
}

# Prepare raster coordinates and produce map
head(dataoutput)

table(is.na(dataoutput)) # no missing data

# Define new data frame with coordinates and outcome to be plotted
PARAM <- data.frame(x = dat$x, y = dat$y, z = dataoutput$estimate)
predRast <- rasterFromXYZ(PARAM)     # convert into raster object

# assign the same crs to predRast that the precip raster has
crs(avgppt)
crs(predRast) <- "+proj=utm +zone=13 +datum=NAD83 +units=m +no_defs" 

# Mask prediction raster to exclude anything less than 90 mm of rainfall (that was the minimum precip during our study)
pptmask <- rasterFromXYZ(cbind(dat$x, dat$y, dat$avgppt))
pptmask[pptmask < 90] <- NA
crs(pptmask) <- "+proj=utm +zone=13 +datum=NAD83 +units=m +no_defs" 
# make prediction raster to the new precip raster
predRast2 <- mask(predRast, pptmask)

# visualize survival surface with mapview
mapview(predRast2)

# visualize survival and precipitation rasters side by side
par(mfrow = c(1,2))
mapPalette <- colorRampPalette(c("grey", "yellow", "orange", "red"))
plot(predRast2, col = mapPalette(100), axes = F, box = F, main = "BFF Survival Probability")
plot(pptmask, col = mapPalette(100), axes = F, box = F, main = "Average Annual Precip (mm)")


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# END
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#







