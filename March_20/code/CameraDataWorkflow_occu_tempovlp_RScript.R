###############################################################################
#Example Code: Camera data cleaning, occupancy models, and temporal activity
#Molly Caldwell
#20 March 2023
###############################################################################

#clear environment
rm(list = ls())

#set working directory
setwd("C:/Users/molly/OneDrive - University of Wyoming/Documents/UWyo/Teaching/Gabe's Pop Model Guest Lec_3.2023/") 

#load libraries
library(tidyverse)
library(lubridate)


#load camera data sheets########################################################
#first list files matching property name (BCHER)
##change file directory to where you have data saved
file_list <- list.files("./Data/", pattern = "BCHER", full.names = TRUE) 
  
#function to read in csv's
##need to set all columns to character to load all data correctly
read.csv.ts <- function(data){
  read.csv(data, colClasses = "character")
}
  
#create data frame of all BCHER data sheets
data <- map_dfr(file_list, read.csv.ts) #you should have about 16,000 observations
  

#clean camera data##############################################################
#look at data
glimpse(data) #we have an extra datetime column due to formatting differences

#look at formatting of date and time columns
unique(data$DateTime) #Y-m-d H:M:S
unique(data$Date) #d-b-Y
unique(data$Time) #H:M:S

#Combine data from the different date and time columns
  ##first, format the Date column
  data <- data %>% 
    mutate(Date = as.POSIXct(Date, format = "%d-%b-%Y")) #see https://www.stat.berkeley.edu/~s133/dates.html for date formatting info
  
  ##replace missing data in DateTime with combined Date and Time columns
  data <- data %>% 
    mutate(DateTime = if_else(is.na(DateTime), paste(Date, Time, sep = " "), DateTime)) %>% 
    mutate(DateTime = as.POSIXct(DateTime, format = "%Y-%m-%d %H:%M:%S")) #format DateTime
  
  ##check for NAs in DateTime
  data %>% summarize(NAs = sum(is.na(DateTime))) #we no longer have NAs
 
#Check for number individual and species variable malfunctions/mistakes in data 
unique(data$NumIndividual1)#looks ok
unique(data$NumIndividual2)#looks ok
unique(data$Species1) #looks ok
unique(data$Species2) #looks ok

#select only variables we want to keep
data <- data %>% 
  select(c("Species1", "Species2", "NumIndividual1", "NumIndividual2",
           "CameraNumber", "DateTime"))

#combine species1 and 2 (and number individuals) into single variables
  ##create dataset with just species 1 data
  s1 <- data %>%
    select(everything(), -Species2, -NumIndividual2) %>%
    dplyr::rename(Species = Species1, Num_individuals = NumIndividual1) #rename species and num_indiv
  
  ##create dataset with just species 2 data
  s2 <- data %>%
    select(everything(), -Species1, -NumIndividual1) %>%
    dplyr::rename(Species = Species2, Num_individuals = NumIndividual2) #rename species and num_indiv
  
  ##combine datasets
  data <- rbind(s1, s2)
  
  ##remove empty detections
  data <- data %>% 
    filter(Species != "")

#remove duplicates##############################################################
#these cameras were set to take 3 photos per infrared trigger
#and triggered again right away if animals passed in front of the camera again
#I didn't want any repeats, so I removed any detections of the same species within 30 min (after trying a few different cutoffs)

#create delay variable of minutes from previous occurrence of same species at same camera
data <- data %>%
    group_by(CameraNumber, Species) %>% #group delay per camera and species
    arrange(CameraNumber, Species, DateTime) %>% #arrange data in order by camera, species, and datetime
    mutate(Species_Delay = difftime(DateTime, lag(DateTime), units = "mins")) %>% #create variable of species delay time
    mutate(Species_Delay = as.numeric(Species_Delay)) #set delay time as numeric
  
  
#filter out duplicated species occurrences within 30 minute time cutoff
data <- data %>% 
  filter(Species_Delay > 30) #only keep detections more than 30 min apart of same species

#group similar species##########################################################
#we are going to group similar species (rodents and birds) into the same species category
#pooling detections can sometimes help with analyses, but make sure you think about what you're doing

#group all rodent detections to species name rodent, and all birds to bird
  ##first look at what species there are
  unique(data$Species) 

  ##now recategorize rodents and birds
  data <- data %>% 
    mutate(Species = if_else(Species %in% c("Douglas squirrel", "chipmunk"), "rodent",
                     if_else(Species %in% c("songbird", "water bird"), "bird",
                             Species)))

  #for the purpose of our analyses, we will remove none and unknown detections
  data <- data %>% 
    filter(Species != "none" & Species != "unknown")
  
#summarize number of (unduplicated) detections per species
detect_summ <- data %>% 
  group_by(Species) %>% 
  summarize(num_det = n())

detect_summ  

#Occupancy Models##############################################################
library(unmarked)
library(camtrapR)

#Step 1: create detection history tables for species of interest
#For this example I am just doing deer
#but a detection history should be made for every species you want to run a model on

#create dataset of the maximum and minimum dates per site (should generally be your setup/takedown dates)
site_dates <- data %>%
  group_by(CameraNumber) %>%
  summarize(Retrieval = date(max(DateTime)), Setup = date(min(DateTime)))

#create camera operation matrix
camop <- cameraOperation(CTtable = site_dates, #start and end dates per camera dataset
                         stationCol = "CameraNumber", #the column that has site ID
                         setupCol = "Setup", #column with start date
                         retrievalCol = "Retrieval", #column with end date
                         hasProblems = FALSE, #can specify dates of malfunctions
                         dateFormat = "%Y-%m-%d") #format of date in CTtable


#create detection history table for deer 
DH_deer <- detectionHistory(recordTable = data, #camera data
                                 camOp = camop, #camera operation matrix
                                 stationCol = "CameraNumber", #siteID column
                                 speciesCol = "Species", #species column
                                 recordDateTimeCol = "DateTime", #datetime column
                                 day1 = "survey", #when detection history should begin, survey denotes first day in camop
                                 datesAsOccasionNames = FALSE, 
                                 includeEffort = TRUE, #include number of active camera days per station
                                 scaleEffort = FALSE, #scale and center effort matrix to mean of 0
                                 species = "deer", #species to make detection history of- you can change this for any species
                                 occasionLength = 1, #length (in days) to calculate detection over
                                 timeZone= "UTC")

#Step 2: create observation and site covariates dataset
##the tricky thing with camera data is that the photo observation data doesn't match the dimensions/ structure of the detection history
##but, the observation and site covariates do need to match the detection history dimensions (number of occasions, and order of sites)
##so, it takes a bit of rearranging to get covariates in a usable format

#create observation covariates dataset (these vary by occasion, e.g. day/season)
  ##we will just be doing month for this example
  ##first identify how many sites/stations you have (need covariates per site)
  num_sites <- length(unique(data$CameraNumber))
  
  ##now, sequence from the minimum date of surveys to the maximum date (corresponding to length of detection history)
  dates <- seq(date(min(data$DateTime)), by = "day", length.out = ncol(DH_deer[["detection_history"]]))
  
  ##convert dates to month
  month <- map_dbl(dates, month)
  
  ##repeat month by number of camera sites
  month_site <- rep(month, num_sites)
  
  ##create observation covariate dataframe to use in model
  obscov <- data.frame(month = month_site)

#create site covariates dataset (these vary by site, but not occasion, e.g habitat/property/etc.)
  ##read in site covariates csv
  BCHER_sc <- read.csv("./Data/ButteCreek_sitecovariates.csv") #we want to pull the LocationID (landcover type)
  
  ##site covariates have to be in order of detection history cameras (which are 10,11,12,13,14,15,4,7,9 if we look at det hist table)
  ##so, we need to re-order camera number in BCHER_sc
  BCHER_sc <- BCHER_sc %>% 
    mutate(CameraNumber = factor(CameraNumber, 
              levels = c("10", "11", "12", "13", "14", "15", "4", "7", "9"))) %>% 
    arrange(CameraNumber)
  
  ##now, we can pull LoacationID (landcover type)
  landcov <- BCHER_sc$LocationID
  
  ##we may also be interested in site-level detection summaries as covariates
  ##such as rate of human visitation per site during survey duration
  ##so, we can calculate that per site from camera data and then add as covariate
  ##for detection rates, we first need to calculate number active survey days per site
    #let's calculate that from the site_dates data
  active_survey <- site_dates %>% 
    group_by(CameraNumber) %>% 
    summarize(surv_days = Retrieval - Setup)
  
    #now, calculate the number of human detections per site
  #you can change this to any species to find detection rates
    human_det <- data %>% 
      filter(Species == "human") %>% 
      group_by(CameraNumber) %>% 
      summarize(num_hum = n())
    
    #join together survey days and human detections, and calculate human detection rate (num_hum/surv_days)
    humDR_df <- left_join(active_survey, human_det, by = "CameraNumber") %>% 
      mutate(humDR = num_hum/as.numeric(surv_days))
    
    #make sure camera number is arranged the same as the detection history
    humDR_df <- humDR_df %>% 
      mutate(CameraNumber = factor(CameraNumber, 
        levels = c("10", "11", "12", "13", "14", "15", "4", "7", "9"))) %>% 
      arrange(CameraNumber)
  
  ##finally, create a site covariate dataframe with landcover and human detection rate
  sitecov <- data.frame(landcov = landcov, humDR = humDR_df$humDR)
  
#Step 3: create unmarked frame to use in occupancy model
  #need to combine detection history, observation, and site covariates into unmarked frame
  deer_frame <- unmarkedFrameOccu(y = DH_deer$detection_history, #take just the detection history table
                                  obsCovs = obscov, siteCovs = sitecov)
  
#Step 3: Run occupancy models
#We will be doing single season, single species occupancy models on deer
  ##model with no covariates
  null_model <- occu(~1~1, data = deer_frame)
  summary(null_model)
  
  ##when cameras are close together (like our sample data), we are violating occupancy assumptions
  ##and we have so many detections of deer per site that occupancy may not be a useful metric
  ##but detection can also serve as likelihood for detecting that species dependent on covariates per site
  ##(just be careful how you interpret it and how your cameras are set up)
  ##in these cases, we can leave occupancy (on right side) as a ~1 with no covariates
  
  ##model with site and observation covariates for detection probability
  det_model <- occu(~month + humDR + landcov ~1, data = deer_frame) 
  summary(det_model)
  
  ##if we do want to model both detection and occupancy, it can look like this
  ##note that occupancy should only be modeled with site covariates (not observation)
  occ_model <- occu(~humDR + month + landcov ~landcov, data = deer_frame)
  summary(occ_model)
  

#Temporal activity and overlap#################################################
#occupancy models describe spatial patterns of animal camera detections
#but we often also want to know temporal patterns of detections
#we can do this following methods of Ridout and Linkie 2009 using the overlap package
library(overlap)

#Step 1: convert time into radians
  ##radian time allows us to model time as circular
  data <- data %>%  #convert datetime to lubridate format
    mutate(Time = hms(format(DateTime, format = "%H:%M:%S"))) %>% #pull time from date time
    mutate(Time_day = (hour(Time)*3600 + minute(Time)*60 + 
                         second(Time))/86400) %>%  #create decimal daily time variable  
    mutate(Time_rad = Time_day * 2 * pi) #create radian time variable
  
#Step 2: Create a radian time dataset for each species of interest
  #The overlap package runs on a dataset per species, with radian time
  #so, we need to get our data into the correct format
  
  #function for making data frame of radian time for each species
  rad.sp.df <- function(CT_df){ 
    species = unique(CT_df$Species) #create vector of species to loop through
    listdf <- list() #creating an empty list to save each species' data to
    for (i in species){
      A <- CT_df[CT_df$Species == i, ] #subsetting data by each species
      rad <- A$Time_rad #taking the subsetted radian times
      if (length(rad)>1) { #if the radian times have more than 1 entry
        r <- as.vector(rad) #then turn radian time into a vector
        names(r) <- i #name vector with the species name
        listdf[[i]] <- r  #add that vector to our list
      }
    }
    return(listdf)
  }
  
  #run function to create data frame of radian time for each species in data
  data_rad <- rad.sp.df(data)

#Step 3: Plot kernel densities of radian times per species
  #function for kernel density plots for each species
  sp.densityPlot <- function(sp_rad_data){ #uses species radian data list made in step 2
      # Set up a plotting space
      par(mfrow = c(2,2))  #rows and columns (2,2), 4 species at a time, can change this

      # Create the loop.vector (all the columns)
      sp <- names(sp_rad_data) #names of species in the list
      print(sp)
      #Loop over species names for density plots
      for (i in 1:length(sp)) {
      G <- overlap::densityPlot(as.numeric(sp_rad_data[[i]]), rug = TRUE, main = sp[i]) #density plot from overlap package
      }
      G
  }

  #now, run plotting function on radian data we made
  sp.densityPlot(data_rad)

#Step 4: Calculating overlap between species' temporal activity patterns
  ##decide what species to run the overlap for- sample size is important
  ##for this example, we will be using deer and humans
  
  #first step is to resample the radian data to allow us to calculate confidence intervals
  #overlap uses nonparametric bootstrapping to generate resamples
  rs_deer <- resample(data_rad[["deer"]], 1000) #resample 1000 times, can change this to any species
  rs_human <- resample(data_rad[["human"]], 1000) #can change this to any species
  
  #now, calculate coeficient of overlap between deer and human activity
  coef_ol <- overlapEst(data_rad[["deer"]], data_rad[["human"]],
                        type = "Dhat4") #Dhat4 method is for samples larger for 50
  
  coef_ol #look at output
  
  #calculate coefficient of overlap for bootstrapped data
  bs_est <- bootEst(rs_deer, rs_human, type = "Dhat4")
  bs_coef_ol <- mean(bs_est) #take mean of all bootstrap coefficient estimates
  
  bs_coef_ol #look at output
  
  #and calculate coefficient of overlap confidence interval
  CI <- bootCI(coef_ol, bs_est, conf = 0.95)['basic0',]
  
  CI #look at output

  #overlap plot for human and deer
  overlapPlot(data_rad[["deer"]], data_rad[["human"]], linecol = c("green", "black"),
              olapcol = "lightgrey", rug = TRUE, main = "")
  legend("topleft", c("Deer", "Human"), lty=1, col=c("green", "black"), bg="white")
  
  