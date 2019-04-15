# Google's R Style Guide states that maximum line length is eighty characters.80
################################################################################

# 2019 Regional Transportation Commission of Southern Nevada, MPO
# Grant C. Shirts, Transportation Planner, Modeling & Analysis Division

# Used to manipulate and analyze transportation data collected by INRIX, Inc.
# Outputs used for the Congestion Management Process

################################################################################

# Set the working directory (wd) a.k.a. file folder. Use "//" (double slash)
setwd("S://CMP//2019//Data//Bottleneck")

# Check that the working directory was set successfully
cat("Your current working directory is", getwd())

# Set file path where packages are stored and check if successful. Use "//"
cat("Your current library trees are:", 
		.libPaths("S://CMP//Methodologies//R//Packages"))

################################################################################

# Install packages (this updates them if they are already installed)
install.packages("plyr", dependencies = TRUE)
install.packages("psych", dependencies = TRUE)
install.packages("tidyr", dependencies = TRUE)

# Call packages. Don't worry about warnings as long as packages are called to R
library(plyr)
library(psych)
library(tidyr)

################################################################################
################################################################################
################################################################################
#				Load and Prepare Data		         				           #
################################################################################
################################################################################
################################################################################

# initialize while loop
fileext <- ".csv"
pathoutput <- "S://CMP//2019//Data//Bottleneck//Output//"
pathinput <- "S://CMP//2019//Data//Bottleneck//Input//"
nameoutput <- "BottleneckRankingTable."
nameinput <- "BottleneckRankingTable"
seasondef <- c('Winter', 'Spring', 'Early Summer', 'Late Summer', 'Fall', 
               'Holiday')
seasonabbr <- c('win.', 'spr.', 'esum.', 'lsum.', 'fal.', 'hol.')
seasoncode <- c('1', '2', '3', '4', '5', '6')
nseasoncode <- length(seasoncode)
iseasoncode <- 3 # interchange data only in winter and spring of 2015
yeartxt <- c('2015', '2016', '2017', '2018')
yearint <- c(2015, 2016, 2017, 2018)
year <- c('15', '16', '17', '18')
nyear <- length(year)
iyear <- 1

# Calculate TMC and Segment Performance Measures
while (iyear <= nyear) {
  while(iseasoncode <= nseasoncode) {
    inputid <- paste(pathinput, seasoncode[iseasoncode], nameinput, year[iyear], 
                   fileext, sep = "")
    outputid <- as.name(paste(seasonabbr[iseasoncode], year[iyear], 
                              sep = ""))
    
    # Import data downloaded from Bottleneck Ranking
    imported <- read.csv(inputid, stringsAsFactors = FALSE, header = TRUE)
    
    # Manipulate Fields and Data Types
    imported$X <- NULL
    imported$year <- yearint[iyear]
    imported$yeartxt <- yeartxt[iyear] # Default GIS Import Reverts To Number!
    imported$season <- seasondef[iseasoncode]
    imported$seasoncode <- iseasoncode
    imported$Base.Impact <- as.numeric(gsub(",", "", imported$Base.Impact))
    imported$Speed.differential <- as.numeric(gsub(",", "", 
                                                   imported$Speed.differential))
    imported$Congestion <- as.numeric(gsub(",", "", imported$Congestion))
    imported$TOTAL.DELAY <- as.numeric(gsub(",", "", imported$TOTAL.DELAY))
    imported$Head.Location <- factor(imported$Head.Location..approximate.)
    imported <- eval(separate(data = imported, 
                              col = Head.Location..approximate., 
                              into = c("road", "intersection"), sep = " @ "))
    imported$AvgDailyDuration <- imported$Average.daily.duration
    imported$AvgDailyDuration <- gsub(" m", "", imported$AvgDailyDuration)
    imported <- eval(separate(data = imported, 
                              col = AvgDailyDuration, 
                              into = c("AvgDailyDurationHrs", "AvgDailyDurationMins"), sep = " h "))
    imported$AvgDailyDurationHrs <- as.numeric(imported$AvgDailyDurationHrs)
    imported$AvgDailyDurationMins <- as.numeric(imported$AvgDailyDurationMins)
    
    # Error Handling
    imported$AvgDailyDurationMinsFix <- ifelse(is.na(imported$AvgDailyDurationMins), 
                                               imported$AvgDailyDurationHrs, 
                                               as.numeric(""))
    
    imported$AvgDailyDurationHrs <- ifelse(is.na(imported$AvgDailyDurationMinsFix), 
                                           imported$AvgDailyDurationHrs, 
                                           as.numeric(""))
    
    imported$AvgDailyDurationMins <- ifelse(is.na(imported$AvgDailyDurationMinsFix), 
                                            imported$AvgDailyDurationMins, 
                                            imported$AvgDailyDurationMinsFix)
    
    imported$AvgDailyDurationMinsFix <- NULL

    # Calculate Total Average Daily Duration
    imported$AvgDailyDurationTotalMinsFix <- imported$AvgDailyDurationHrs * 60
    imported$AvgDailyDurationTotalMins <- (imported$AvgDailyDurationMins + 
                                           imported$AvgDailyDurationTotalMinsFix)
    imported$AvgDailyDurationTotalMinsFix <- NULL
    imported$AvgDailyDurationTotalMins <- ifelse(is.na(imported$AvgDailyDurationTotalMins), 
                                                 imported$AvgDailyDurationMins, 
                                                 imported$AvgDailyDurationTotalMins)
    
    # Create Dataframe
    assign(paste(outputid), imported)
    rm(imported)
    iseasoncode = iseasoncode + 1
  }
  iseasoncode = 1
  iyear = iyear + 1
}

################################################################################
# combine all data.frames into one
alldf <- names(which(unlist(eapply(.GlobalEnv,is.data.frame))))
listofdataframes <- lapply(setNames(alldf, alldf), 
                           function(x) if (class(get(x)) == "data.frame") 
                             get(x))
bottlenecks <- rbind.fill(listofdataframes)
rm(listofdataframes)

# Create a csv.
write.csv(bottlenecks, 
          "S://CMP//2019//Data//Bottleneck//Output//BottlenecksThrough18.csv")
rm(bottlenecks)