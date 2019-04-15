# Google's R Style Guide states that maximum line length is eighty characters.80
################################################################################

# 2019 Regional Transportation Commission of Southern Nevada, MPO
# Grant C. Shirts, Transportation Planner, Modeling & Analysis Division

# Used to manipulate and analyze transportation data collected by INRIX, Inc.
# Outputs used for the Congestion Management Process

################################################################################

# Set the working directory (wd) a.k.a. file folder. Use "//" (double slash)
setwd("S://CMP//2019//Data//Incident")

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
#                           Load and Prepare Data		         				           #
################################################################################
################################################################################
################################################################################

# initialize while loop
fileext <- ".csv"
pathoutput <- "S://CMP//2019//Data//Incident//Output//"
pathinput <- "S://CMP//2019//Data//Incident//Input//"
nameoutput <- "WaycareIncidentsThrough2018"
nameinput <- " Peak - 01_01_2016 - 12_31_2018"
ampm <- c("AM", "PM")
nampm <- length(ampm)
iampm <- 1

while (iampm <= nampm){
  inputid <- paste(pathinput, ampm[iampm], nameinput, fileext, sep = "")
  
  # Import data downloaded from Incidents
  imported <- read.csv(inputid, stringsAsFactors = FALSE, header = TRUE)
  
  # Adjust Field Types
  imported$Date...time <- as.POSIXct(imported$Date...time, 
                                      format="%Y-%m-%d %H:%M %p")
  imported$Discovery.time <- as.POSIXct(imported$Discovery.time,
                                        format="%Y-%m-%d %H:%M %p")
  imported$Response.unit.arrived.time <- 
    as.POSIXct(imported$Response.unit.arrived.time, 
               format="%Y-%m-%d %H:%M %p")
  imported$Roadway.clearance.time <- 
    as.POSIXct(imported$Roadway.clearance.time, 
               format="%Y-%m-%d %H:%M %p")
  imported$Incident.clearance.time <- 
    as.POSIXct(imported$Incident.clearance.time, 
               format="%Y-%m-%d %H:%M %p")
  imported$ï..Type <- as.factor(imported$ï..Type)
  imported$iType <- imported$ï..Type
  imported$ï..Type <- NULL
  imported$Source <- as.factor(imported$Source)
  
  # Prepare Category Selector Fields
  imported$peakhour <- paste(ampm[iampm], "Peak")
  imported$year <- as.numeric(format(imported$Date...time, '%Y'))
  imported$yeartxt <- format(imported$Date...time, '%Y')
  imported$season <- 
    ifelse(format(imported$Date...time, '%m') %in% c('01','02'), "Winter", 
           ifelse(format(imported$Date...time, '%m') %in% c('03','04'), 
                  "Spring", 
           ifelse(format(imported$Date...time, '%m') %in% c('05','06'), 
                  "Early Summer", 
           ifelse(format(imported$Date...time, '%m') %in% c('07','08'), 
                  "Late Summer", 
           ifelse(format(imported$Date...time, '%m') %in% c('09','10'), 
                  "Fall", 
           ifelse(format(imported$Date...time, '%m') %in% c('11','12'), 
                  "Holiday", 
                  NA))))))
  imported$seasoncode <- 
    ifelse(format(imported$Date...time, '%m') %in% c('01','02'), 1, 
           ifelse(format(imported$Date...time, '%m') %in% c('03','04'), 2, 
           ifelse(format(imported$Date...time, '%m') %in% c('05','06'), 3, 
           ifelse(format(imported$Date...time, '%m') %in% c('07','08'), 4, 
           ifelse(format(imported$Date...time, '%m') %in% c('09','10'), 5, 
           ifelse(format(imported$Date...time, '%m') %in% c('11','12'), 6, 
                  NA))))))
           
################################################################################

  imported$Title2 <- toupper(imported$Title)
  imported$Title2 <- gsub("RAINBOW", "R_____W", imported$Title2)
  
  imported$direction1 <- ifelse(grepl("TO GO NB", imported$Subtitle), 
                                "NORTHBOUND", 
                           ifelse(grepl("TO GO EB", imported$Subtitle), 
                                  "EASTBOUND", 
                           ifelse(grepl("TO GO SB", imported$Subtitle), 
                                  "SOUTHBOUND", 
                           ifelse(grepl("TO GO WB", imported$Subtitle), 
                                  "WESTBOUND", 
                                  NA))))
  
  imported$direction2 <- ifelse(grepl("NB", imported$Subtitle), "NORTHBOUND", 
                         ifelse(grepl("EB", imported$Subtitle), "EASTBOUND", 
                         ifelse(grepl("SB", imported$Subtitle), "SOUTHBOUND", 
                         ifelse(grepl("WB", imported$Subtitle), "WESTBOUND", 
                                                     NA))))
  
  imported$direction3 <- ifelse(is.na(imported$direction1), 
                                imported$direction2, 
                                imported$direction1)
  
  imported$direction4 <- ifelse(grepl("NB", imported$Title2), "NORTHBOUND", 
                            ifelse(grepl("EB", imported$Title2), "EASTBOUND", 
                            ifelse(grepl("SB", imported$Title2), "SOUTHBOUND", 
                            ifelse(grepl("WB", imported$Title2), "WESTBOUND", 
                                   NA))))
  
  imported$direction <- ifelse(is.na(imported$direction3), 
                                imported$direction4, 
                                imported$direction3)
  
  imported$direction1 <- NULL
  imported$direction2 <- NULL
  imported$direction3 <- NULL
  imported$direction4 <- NULL
  imported$Title2 <- NULL
  
################################################################################

  # Calculate Incident Performance
  imported$ResponseTimeMins <- abs((imported$Response.unit.arrived.time - 
                                  imported$Discovery.time) / 60)
  imported$ClearanceTimeMins <- abs((imported$Roadway.clearance.time - 
                                   imported$Response.unit.arrived.time) / 60)
  imported$TotalTimeMins <- (imported$ResponseTimeMins + 
                               imported$ClearanceTimeMins)
  
  # Quality Control
  imported$TotalTimeMins <- ifelse(is.na(imported$TotalTimeMins), 
         (abs(imported$Roadway.clearance.time - imported$Discovery.time) / 60),
         imported$TotalTimeMins)
  
  imported$TotalTimeMins <- ifelse(is.na(imported$TotalTimeMins), 
                                   imported$ResponseTimeMins,
                                   imported$TotalTimeMins)
  
################################################################################
  
  # Create Dataframe
  assign(paste(ampm[iampm], nameoutput, sep = ""), imported)
  rm(imported)
  
  iampm = iampm + 1
}

# combine all data.frames into one
alldf <- names(which(unlist(eapply(.GlobalEnv,is.data.frame))))
listofdataframes <- lapply(setNames(alldf, alldf), 
                           function(x) if (class(get(x)) == "data.frame") 
                             get(x))
Incidents <- rbind.fill(listofdataframes)
rm(listofdataframes)

# Create a csv.
write.csv(Incidents, 
          "S://CMP//2019//Data//Incident//Output//IncidentsThrough18.csv", na = "")
#rm(Incidents)
