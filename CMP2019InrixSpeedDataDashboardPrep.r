# Google's R Style Guide states that maximum line length is eighty characters.80
################################################################################

# 2019 Regional Transportation Commission of Southern Nevada, MPO
# Grant C. Shirts, Transportation Planner, Modeling & Analysis Division

# Used to manipulate and analyze transportation data collected by INRIX, Inc.
# Outputs used for the Congestion Management Process

################################################################################

# Set the working directory (wd) a.k.a. file folder. Use "//" (double slash)
setwd("S://CMP//2019//Data//Output")

# Check that the working directory was set successfully
cat("Your current working directory is", getwd())

# Set file path where packages are stored and check if successful. Use "//"
cat("Your current library trees are:", 
		.libPaths("S://CMP//Methodologies//R//Packages"))

################################################################################

# Install packages (this updates them if they are already installed)
install.packages("plyr", dependencies = TRUE)
install.packages("psych", dependencies = TRUE)

# Call packages. Don't worry about warnings as long as packages are called to R
library(plyr)
library(psych)

################################################################################
################################################################################
################################################################################
#		                            Load and Prepare Data			         				     #
################################################################################
################################################################################
################################################################################

# Load data downloaded from Massive Data Downloader from INRIX RITIS
# All fields for INRIX data for Clark, Nevada (3664 TMCs) for Tuesdays, 
# Wednesdays, and Thursdays from Jan 01, 2009 through Dec 27, 2018 between 
# [07:00-08:59] and [16:00-17:59], averaged to every 5th minute
inrix <- read.csv(
"S://CMP//2019//Data//Input//Readings.csv", 
                  header = TRUE)

# Remove Unneeded data to save computer memory 
inrix$confidence_score <- NULL

# Remove Unneeded data to save computer memory
inrix$cvalue <- NULL

################################################################################

# Assign Year
inrix$year <- as.character(substr(inrix$measurement_tstamp, 1, 4))

# Assign Seasons
# Calculation Prep
inrix$month <- as.numeric(as.character(substr(inrix$measurement_tstamp, 6, 7)))
 
# Assign "winter"
inrix$season <- ifelse(inrix$month == 1, "Winter",
                       ifelse(inrix$month == 2, "Winter", ""))

# Assign "winter"
inrix$seasoncode <- ifelse(inrix$month == 1, 1,
                           ifelse(inrix$month == 2, 1, 0))

# Assign "spring"
inrix$season <- ifelse(inrix$month == 3, "Spring",
                       ifelse(inrix$month == 4, "Spring", inrix$season))

# Assign "spring"
inrix$seasoncode <- ifelse(inrix$month == 3, 2,
                           ifelse(inrix$month == 4, 2, inrix$seasoncode))

# Assign "early summer"
inrix$season <- ifelse(inrix$month == 5, "Early Summer",
                       ifelse(inrix$month == 6, "Early Summer", inrix$season))

# Assign "early summer"
inrix$seasoncode <- ifelse(inrix$month == 5, 3,
                           ifelse(inrix$month == 6, 3, inrix$seasoncode))

# Assign "late summer"
inrix$season <- ifelse(inrix$month == 7, "Late Summer",
                       ifelse(inrix$month == 8, "Late Summer", inrix$season))

# Assign "late summer"
inrix$seasoncode <- ifelse(inrix$month == 7, 4,
                           ifelse(inrix$month == 8, 4, inrix$seasoncode))

# Assign "fall"
inrix$season <- ifelse(inrix$month == 9, "Fall",
                       ifelse(inrix$month == 10, "Fall", inrix$season))

# Assign "fall"
inrix$seasoncode <- ifelse(inrix$month == 9, 5,
                           ifelse(inrix$month == 10, 5, inrix$seasoncode))

# Assign "holiday season"
inrix$season <- ifelse(inrix$month == 11, "Holiday",
                       ifelse(inrix$month == 12, "Holiday", inrix$season))

# Assign "holiday season"
inrix$seasoncode <- ifelse(inrix$month == 11, 6,
                           ifelse(inrix$month == 12, 6, inrix$seasoncode))

# Clean-up data
inrix$month <- NULL

################################################################################

# Assign Rush Hour
# Calc Prep
inrix$hour <- as.numeric(as.character(substr(inrix$measurement_tstamp, 12, 13)))

# Assign AM Peak
inrix$peakhour <- ifelse(inrix$hour == 7, "AM Peak",
                         ifelse(inrix$hour == 8, "AM Peak", ""))

# Assign PM Peak
inrix$peakhour <- ifelse(inrix$hour == 16, "PM Peak",
                         ifelse(inrix$hour == 17, "PM Peak",
                                inrix$peakhour))

# Clean-up data
inrix$hour <- NULL

################################################################################
################################################################################
################################################################################
#		                            Subset Data			              						     #
################################################################################
################################################################################
################################################################################

# Create subset of data from the year 2009 (09)
#base.09 <- subset(inrix, year == 2009) 
#rm(base.09) # No Data

# Create subset of data from the year 2010 (10)
#base.10 <- subset(inrix, year == 2010)
#rm(base.10) # No Data

# Create subset of data from the year 2011 (11)
#base.11 <- subset(inrix, year == 2011) # Only Interchanges

# Create subset of data from the year 2012 (12)
#base.12 <- subset(inrix, year == 2012) # Only Interchanges

# Create subset of data from the year 2013 (13)
#base.13 <- subset(inrix, year == 2013) # Only Interchanges

# Create subset of data from the year 2014 (14)
#base.14 <- subset(inrix, year == 2014) # Only Interchanges

# Create subset of data from the year 2015 (15)
base.15 <- subset(inrix, year == 2015)

# Create subset of data from the year 2016 (16)
base.16 <- subset(inrix, year == 2016)

# Create subset of data from the year 2017 (17)
base.17 <- subset(inrix, year == 2017)

# Create subset of data from the year 2018 (18)
base.18 <- subset(inrix, year == 2018)

# Clean-up data
rm(inrix)

################################################################################
################################################################################
# 2015 Subsets
################################################################################
################################################################################

# Convert data type of date data (String to POSIXct).
base.15$measurement_tstamp <- as.POSIXct(base.15$measurement_tstamp,
                                         format="%Y-%m-%d %H:%M:%S")

# Create subset of data from the "winter" season of 15
#win.15 <- subset(base.15, # Only Interchanges
#                 format(measurement_tstamp, '%m') %in%
#                   c('01','02'))

# Create subset of data from the "spring" season of 15
#spr.15 <- subset(base.15, # Only Interchanges
#                 format(measurement_tstamp, '%m') %in% 
#                   c('03','04'))

# Create subset of data from the "early summer" season of 15
esum.15 <- subset(base.15, # Kicks in Late May
                  format(measurement_tstamp, '%m') %in% 
                    c('05','06'))

# Create subset of data from the "late summer" season of 15
lsum.15 <- subset(base.15, 
                  format(measurement_tstamp, '%m') %in% 
                    c('07','08'))

# Create subset of data from the "fall" season of 15
fal.15 <- subset(base.15, 
                 format(measurement_tstamp, '%m') %in% 
                   c('09','10'))

# Create subset of data from the "holiday" season of 15
hol.15 <- subset(base.15, 
                 format(measurement_tstamp, '%m') %in% 
                   c('11','12'))

# Clean-up Data
rm(base.15)

################################################################################

# Create subset of data from weekday morning "rush hour" in "winter" 15
#am.wek.win.15 <- subset(win.15, 
#                        format(measurement_tstamp, '%H') %in% 
#                          c('07','08'))  # 7-9 a.m.

# Create subset of data from weekday morning "rush hour" in "spring" 15
#am.wek.spr.15 <- subset(spr.15, 
#                        format(measurement_tstamp, '%H') %in% 
#                          c('07','08'))  # 7-9 a.m.

# Create subset of data from weekday morning "rush hour" in "early summer" 15
am.wek.esum.15 <- subset(esum.15, 
                         format(measurement_tstamp, '%H') %in% 
                           c('07','08'))  # 7-9 a.m.

# Create subset of data from weekday morning "rush hour" in "late summer" 15
am.wek.lsum.15 <- subset(lsum.15, 
                         format(measurement_tstamp, '%H') %in% 
                           c('07','08'))  # 7-9 a.m.

# Create subset of data from weekday morning "rush hour" in "fall" 15
am.wek.fal.15 <- subset(fal.15, 
                        format(measurement_tstamp, '%H') %in% 
                          c('07','08'))  # 7-9 a.m.

# Create subset of data from weekday morning "rush hour" in "fall" 15
am.wek.hol.15 <- subset(hol.15, 
                        format(measurement_tstamp, '%H') %in% 
                          c('07','08'))  # 7-9 a.m.

################################################################################

# Create subset of data from weekday afternoon "rush hour" in "winter" 15
#pm.wek.win.15 <- subset(win.15, 
#                        format(measurement_tstamp, '%H') %in% 
#                          c('16','17'))  # 4-6 p.m.

# Clean-up Data
#rm(win.15)

# Create subset of data from weekday afternoon "rush hour" in "spring" 15
#pm.wek.spr.15 <- subset(spr.15, 
#                        format(measurement_tstamp, '%H') %in% 
#                          c('16','17'))  # 4-6 p.m.

# Clean-up Data
#rm(spr.15)

# Create subset of data from weekday afternoon "rush hour" "early summmer" 15
pm.wek.esum.15 <- subset(esum.15, 
                         format(measurement_tstamp, '%H') %in% 
                           c('16','17'))  # 4-6 p.m.

# Clean-up Data
rm(esum.15)

# Create subset of data from weekday afternoon "rush hour" "late summer" 15
pm.wek.lsum.15 <- subset(lsum.15, 
                         format(measurement_tstamp, '%H') %in% 
                           c('16','17'))  # 4-6 p.m.

# Clean-up Data
rm(lsum.15)

# Create subset of data from weekday afternoon "rush hour" in "spring" 15
pm.wek.fal.15 <- subset(fal.15, 
                        format(measurement_tstamp, '%H') %in% 
                          c('16','17'))  # 4-6 p.m.

# Clean-up Data
rm(fal.15)

# Create subset of data from weekday afternoon "rush hour" in "spring" 15
pm.wek.hol.15 <- subset(hol.15, 
                        format(measurement_tstamp, '%H') %in% 
                          c('16','17'))  # 4-6 p.m.

# Clean-up Data
rm(hol.15)

################################################################################
################################################################################
# 2016 Subsets
################################################################################
################################################################################

# Convert data type of date data (String to POSIXct).
base.16$measurement_tstamp <- as.POSIXct(base.16$measurement_tstamp,
                                         format="%Y-%m-%d %H:%M:%S")

# Create subset of data from the "winter" season of 15
win.16 <- subset(base.16, 
                 format(measurement_tstamp, '%m') %in%
                   c('01','02'))

# Create subset of data from the "spring" season of 15
spr.16 <- subset(base.16, 
                 format(measurement_tstamp, '%m') %in% 
                   c('03','04'))

# Create subset of data from the "early summer" season of 15
esum.16 <- subset(base.16, 
                  format(measurement_tstamp, '%m') %in% 
                    c('05','06'))

# Create subset of data from the "late summer" season of 15
lsum.16 <- subset(base.16, 
                  format(measurement_tstamp, '%m') %in% 
                    c('07','08'))

# Create subset of data from the "fall" season of 15
fal.16 <- subset(base.16, 
                 format(measurement_tstamp, '%m') %in% 
                   c('09','10'))

# Create subset of data from the "holiday" season of 15
hol.16 <- subset(base.16, 
                 format(measurement_tstamp, '%m') %in% 
                   c('11','12'))

# Clean-up Data
rm(base.16)

################################################################################

# Create subset of data from weekday morning "rush hour" in "winter" 15
am.wek.win.16 <- subset(win.16, 
                        format(measurement_tstamp, '%H') %in% 
                          c('07','08'))  # 7-9 a.m.

# Create subset of data from weekday morning "rush hour" in "spring" 15
am.wek.spr.16 <- subset(spr.16, 
                        format(measurement_tstamp, '%H') %in% 
                          c('07','08'))  # 7-9 a.m.

# Create subset of data from weekday morning "rush hour" in "early summer" 15
am.wek.esum.16 <- subset(esum.16, 
                         format(measurement_tstamp, '%H') %in% 
                           c('07','08'))  # 7-9 a.m.

# Create subset of data from weekday morning "rush hour" in "late summer" 15
am.wek.lsum.16 <- subset(lsum.16, 
                         format(measurement_tstamp, '%H') %in% 
                           c('07','08'))  # 7-9 a.m.

# Create subset of data from weekday morning "rush hour" in "fall" 15
am.wek.fal.16 <- subset(fal.16, 
                        format(measurement_tstamp, '%H') %in% 
                          c('07','08'))  # 7-9 a.m.

# Create subset of data from weekday morning "rush hour" in "fall" 15
am.wek.hol.16 <- subset(hol.16, 
                        format(measurement_tstamp, '%H') %in% 
                          c('07','08'))  # 7-9 a.m.

################################################################################

# Create subset of data from weekday afternoon "rush hour" in "winter" 15
pm.wek.win.16 <- subset(win.16, 
                        format(measurement_tstamp, '%H') %in% 
                          c('16','17'))  # 4-6 p.m.

# Clean-up Data
rm(win.16)

# Create subset of data from weekday afternoon "rush hour" in "spring" 15
pm.wek.spr.16 <- subset(spr.16, 
                        format(measurement_tstamp, '%H') %in% 
                          c('16','17'))  # 4-6 p.m.

# Clean-up Data
rm(spr.16)

# Create subset of data from weekday afternoon "rush hour" "early summmer" 15
pm.wek.esum.16 <- subset(esum.16, 
                         format(measurement_tstamp, '%H') %in% 
                           c('16','17'))  # 4-6 p.m.

# Clean-up Data
rm(esum.16)

# Create subset of data from weekday afternoon "rush hour" "late summer" 15
pm.wek.lsum.16 <- subset(lsum.16, 
                         format(measurement_tstamp, '%H') %in% 
                           c('16','17'))  # 4-6 p.m.

# Clean-up Data
rm(lsum.16)

# Create subset of data from weekday afternoon "rush hour" in "spring" 15
pm.wek.fal.16 <- subset(fal.16, 
                        format(measurement_tstamp, '%H') %in% 
                          c('16','17'))  # 4-6 p.m.

# Clean-up Data
rm(fal.16)

# Create subset of data from weekday afternoon "rush hour" in "spring" 15
pm.wek.hol.16 <- subset(hol.16, 
                        format(measurement_tstamp, '%H') %in% 
                          c('16','17'))  # 4-6 p.m.

# Clean-up Data
rm(hol.16)

################################################################################
################################################################################
# 2017 Subsets
################################################################################
################################################################################

# Convert data type of date data (String to POSIXct).
base.17$measurement_tstamp <- as.POSIXct(base.17$measurement_tstamp,
                                         format="%Y-%m-%d %H:%M:%S")

# Create subset of data from the "winter" season of 15
win.17 <- subset(base.17, 
                 format(measurement_tstamp, '%m') %in%
                   c('01','02'))

# Create subset of data from the "spring" season of 15
spr.17 <- subset(base.17, 
                 format(measurement_tstamp, '%m') %in% 
                   c('03','04'))

# Create subset of data from the "early summer" season of 15
esum.17 <- subset(base.17, 
                  format(measurement_tstamp, '%m') %in% 
                    c('05','06'))

# Create subset of data from the "late summer" season of 15
lsum.17 <- subset(base.17, 
                  format(measurement_tstamp, '%m') %in% 
                    c('07','08'))

# Create subset of data from the "fall" season of 15
fal.17 <- subset(base.17, 
                 format(measurement_tstamp, '%m') %in% 
                   c('09','10'))

# Create subset of data from the "holiday" season of 15
hol.17 <- subset(base.17, 
                 format(measurement_tstamp, '%m') %in% 
                   c('11','12'))

# Clean-up Data
rm(base.17)

################################################################################

# Create subset of data from weekday morning "rush hour" in "winter" 15
am.wek.win.17 <- subset(win.17, 
                        format(measurement_tstamp, '%H') %in% 
                          c('07','08'))  # 7-9 a.m.

# Create subset of data from weekday morning "rush hour" in "spring" 15
am.wek.spr.17 <- subset(spr.17, 
                        format(measurement_tstamp, '%H') %in% 
                          c('07','08'))  # 7-9 a.m.

# Create subset of data from weekday morning "rush hour" in "early summer" 15
am.wek.esum.17 <- subset(esum.17, 
                         format(measurement_tstamp, '%H') %in% 
                           c('07','08'))  # 7-9 a.m.

# Create subset of data from weekday morning "rush hour" in "late summer" 15
am.wek.lsum.17 <- subset(lsum.17, 
                         format(measurement_tstamp, '%H') %in% 
                           c('07','08'))  # 7-9 a.m.

# Create subset of data from weekday morning "rush hour" in "fall" 15
am.wek.fal.17 <- subset(fal.17, 
                        format(measurement_tstamp, '%H') %in% 
                          c('07','08'))  # 7-9 a.m.

# Create subset of data from weekday morning "rush hour" in "fall" 15
am.wek.hol.17 <- subset(hol.17, 
                        format(measurement_tstamp, '%H') %in% 
                          c('07','08'))  # 7-9 a.m.

################################################################################

# Create subset of data from weekday afternoon "rush hour" in "winter" 15
pm.wek.win.17 <- subset(win.17, 
                        format(measurement_tstamp, '%H') %in% 
                          c('16','17'))  # 4-6 p.m.

# Clean-up Data
rm(win.17)

# Create subset of data from weekday afternoon "rush hour" in "spring" 15
pm.wek.spr.17 <- subset(spr.17, 
                        format(measurement_tstamp, '%H') %in% 
                          c('16','17'))  # 4-6 p.m.

# Clean-up Data
rm(spr.17)

# Create subset of data from weekday afternoon "rush hour" "early summmer" 15
pm.wek.esum.17 <- subset(esum.17, 
                         format(measurement_tstamp, '%H') %in% 
                           c('16','17'))  # 4-6 p.m.

# Clean-up Data
rm(esum.17)

# Create subset of data from weekday afternoon "rush hour" "late summer" 15
pm.wek.lsum.17 <- subset(lsum.17, 
                         format(measurement_tstamp, '%H') %in% 
                           c('16','17'))  # 4-6 p.m.

# Clean-up Data
rm(lsum.17)

# Create subset of data from weekday afternoon "rush hour" in "spring" 15
pm.wek.fal.17 <- subset(fal.17, 
                        format(measurement_tstamp, '%H') %in% 
                          c('16','17'))  # 4-6 p.m.

# Clean-up Data
rm(fal.17)

# Create subset of data from weekday afternoon "rush hour" in "spring" 15
pm.wek.hol.17 <- subset(hol.17, 
                        format(measurement_tstamp, '%H') %in% 
                          c('16','17'))  # 4-6 p.m.

# Clean-up Data
rm(hol.17)

################################################################################
################################################################################
# 2018 Subsets
################################################################################
################################################################################

# Convert data type of date data (String to POSIXct).
base.18$measurement_tstamp <- as.POSIXct(base.18$measurement_tstamp,
                                         format="%Y-%m-%d %H:%M:%S")

# Create subset of data from the "winter" season of 15
win.18 <- subset(base.18, 
                 format(measurement_tstamp, '%m') %in%
                   c('01','02'))

# Create subset of data from the "spring" season of 15
spr.18 <- subset(base.18, 
                 format(measurement_tstamp, '%m') %in% 
                   c('03','04'))

# Create subset of data from the "early summer" season of 15
esum.18 <- subset(base.18, 
                  format(measurement_tstamp, '%m') %in% 
                    c('05','06'))

# Create subset of data from the "late summer" season of 15
lsum.18 <- subset(base.18, 
                  format(measurement_tstamp, '%m') %in% 
                    c('07','08'))

# Create subset of data from the "fall" season of 15
fal.18 <- subset(base.18, 
                 format(measurement_tstamp, '%m') %in% 
                   c('09','10'))

# Create subset of data from the "holiday" season of 15
hol.18 <- subset(base.18, 
                 format(measurement_tstamp, '%m') %in% 
                   c('11','12'))

# Clean-up Data
rm(base.18)

################################################################################

# Create subset of data from weekday morning "rush hour" in "winter" 15
am.wek.win.18 <- subset(win.18, 
                        format(measurement_tstamp, '%H') %in% 
                          c('07','08'))  # 7-9 a.m.

# Create subset of data from weekday morning "rush hour" in "spring" 15
am.wek.spr.18 <- subset(spr.18, 
                        format(measurement_tstamp, '%H') %in% 
                          c('07','08'))  # 7-9 a.m.

# Create subset of data from weekday morning "rush hour" in "early summer" 15
am.wek.esum.18 <- subset(esum.18, 
                         format(measurement_tstamp, '%H') %in% 
                           c('07','08'))  # 7-9 a.m.

# Create subset of data from weekday morning "rush hour" in "late summer" 15
am.wek.lsum.18 <- subset(lsum.18, 
                         format(measurement_tstamp, '%H') %in% 
                           c('07','08'))  # 7-9 a.m.

# Create subset of data from weekday morning "rush hour" in "fall" 15
am.wek.fal.18 <- subset(fal.18, 
                        format(measurement_tstamp, '%H') %in% 
                          c('07','08'))  # 7-9 a.m.

# Create subset of data from weekday morning "rush hour" in "fall" 15
am.wek.hol.18 <- subset(hol.18, 
                        format(measurement_tstamp, '%H') %in% 
                          c('07','08'))  # 7-9 a.m.

################################################################################

# Create subset of data from weekday afternoon "rush hour" in "winter" 15
pm.wek.win.18 <- subset(win.18, 
                        format(measurement_tstamp, '%H') %in% 
                          c('16','17'))  # 4-6 p.m.

# Clean-up Data
rm(win.18)

# Create subset of data from weekday afternoon "rush hour" in "spring" 15
pm.wek.spr.18 <- subset(spr.18, 
                        format(measurement_tstamp, '%H') %in% 
                          c('16','17'))  # 4-6 p.m.

# Clean-up Data
rm(spr.18)

# Create subset of data from weekday afternoon "rush hour" "early summmer" 15
pm.wek.esum.18 <- subset(esum.18, 
                         format(measurement_tstamp, '%H') %in% 
                           c('16','17'))  # 4-6 p.m.

# Clean-up Data
rm(esum.18)

# Create subset of data from weekday afternoon "rush hour" "late summer" 15
pm.wek.lsum.18 <- subset(lsum.18, 
                         format(measurement_tstamp, '%H') %in% 
                           c('16','17'))  # 4-6 p.m.

# Clean-up Data
rm(lsum.18)

# Create subset of data from weekday afternoon "rush hour" in "spring" 15
pm.wek.fal.18 <- subset(fal.18, 
                        format(measurement_tstamp, '%H') %in% 
                          c('16','17'))  # 4-6 p.m.

# Clean-up Data
rm(fal.18)

# Create subset of data from weekday afternoon "rush hour" in "spring" 15
pm.wek.hol.18 <- subset(hol.18, 
                        format(measurement_tstamp, '%H') %in% 
                          c('16','17'))  # 4-6 p.m.

# Clean-up Data
rm(hol.18)

################################################################################
################################################################################
################################################################################
# Calculations Prep
################################################################################
################################################################################
################################################################################

# Upload TMC identification file with segment descriptions
tmc.id.segm <- read.csv(
  "S://CMP//2019//Data//Input//tmc_id_corr_segments.csv", 
  header = TRUE)

# Upload TMC identification file
tmc.id <- read.csv(
  "S://CMP//2019//Data//Input//TMC_Identification.csv", 
  header = TRUE)

# Assign Code TMC Active Periods
   #active_start_date	        #active_end_date
#1 #2018-12-03 12:00:00-05:00	#NA
#2 #2018-04-25 00:00:00-04:00	#2018-12-03 12:00:00-05:00
#3 #2017-05-15 00:00:00-04:00	#2018-04-25 00:00:00-04:00
#4 #2016-08-22 20:00:00-04:00	#2017-05-15 00:00:00-04:00
#5 #2016-03-29 00:00:00-04:00	#2016-08-22 00:00:00-04:00
#6 #2015-10-29 00:00:00-04:00	#2016-03-29 00:00:00-04:00
#7 #2015-04-28 00:00:00-04:00	#2015-10-29 00:00:00-04:00
#8 #2014-03-01 00:00:00-05:00	#2014-10-20 20:00:00-04:00
#9 #2011-03-30 00:00:00-04:00	#2014-03-01 00:00:00-05:00
tmc.id$active <- 
  ifelse(tmc.id$active_start_date == 
           "2018-12-03 12:00:00-05:00", 1, 
         ifelse(tmc.id$active_start_date == 
                  "2018-04-25 00:00:00-04:00", 2, 
                ifelse(tmc.id$active_start_date == 
                   "2017-05-15 00:00:00-04:00", 3, 
                 ifelse(tmc.id$active_start_date == 
                    "2016-08-22 20:00:00-04:00", 4, 
                  ifelse(tmc.id$active_start_date == 
                           "2016-03-29 00:00:00-04:00", 5, 
                         ifelse(tmc.id$active_start_date == 
                            "2015-10-29 00:00:00-04:00", 6, 
                          ifelse(tmc.id$active_start_date == 
                             "2015-04-28 00:00:00-04:00", 7,  
                           ifelse(tmc.id$active_start_date == 
                                    "2014-03-01 00:00:00-05:00", 8, 
                                  ifelse(tmc.id$active_start_date == 
                                     "2011-03-30 00:00:00-04:00", 9, 0)
                                  ))))))))

# Form a unique ID for processing
tmc.id$unique <- paste(tmc.id$tmc, tmc.id$active)

# Convert data type of date data (String to POSIXct).
tmc.id$active_start_date <- as.POSIXct(tmc.id$active_start_date, 
                                       format="%Y-%m-%d %H:%M:%S")

# Convert data type of date data (String to POSIXct).
tmc.id$active_end_date <- as.POSIXct(tmc.id$active_end_date, 
                                     format="%Y-%m-%d %H:%M:%S")

# Change name of the first column from "tmc" to "tmc_code" (match inrix field)
name.tmc_code <- colnames(tmc.id)
name.tmc_code[1] <- "tmc_code"
colnames(tmc.id) <- name.tmc_code

# Remove Fields regained later during post-processing TMC GIS Polyline Join
# (miles retained for calculations in R)
tmc.id$road <- NULL
tmc.id$direction <- NULL
tmc.id$intersection <- NULL
tmc.id$zip <- NULL
tmc.id$start_latitude <- NULL
tmc.id$start_longitude <- NULL
tmc.id$end_latitude <- NULL
tmc.id$end_longitude <- NULL
tmc.id$road_order <- NULL
tmc.id$type <- NULL

################################################################################
################################################################################
################################################################################
# Define Segment Analysis Function
################################################################################
################################################################################
################################################################################

segmentanalysis <- function(perf.segm, iampm) {
  # Aggregating Metrics Across Multiple TMCs by Segment
  colsel <- c(9,19,20,28,31,32,33,34,35,36)
  perf.segm.wavg <- aggregate(perf.segm[, colsel],
                              list(perf.segm$segment_name),
                              FUN = function(x) list(x))
  # Initialize The Following While Loop
  i <- 1 # row index
  n <- nrow(perf.segm.wavg) # outer loop end condition
  N <- ncol(perf.segm.wavg) # inner loop end condition
  # Weighted average based on the length of the individual TMC codes
  while (i <= n) {
    I = 3 # column index
    while (I <= N) {
      values <- unlist(perf.segm.wavg[i, I])
      weights <- unlist(perf.segm.wavg[i, 2])
      perf.segm.wavg[i, I] <- weighted.mean(values, weights, na.rm = TRUE)
      I = I+1
    }
    i = i+1
  }
  
  # Clean-up Data
  perf.segm.wavg$miles <- NULL
  perf.segm.wavg$spd_hmean <- unlist(perf.segm.wavg$spd_hmean)
  perf.segm.wavg$histavgspd_hmean <- unlist(perf.segm.wavg$histavgspd_hmean)
  perf.segm.wavg$bufindex <- unlist(perf.segm.wavg$bufindex)
  perf.segm.wavg$planindex <- unlist(perf.segm.wavg$planindex)
  perf.segm.wavg$tvltimeindex <- unlist(perf.segm.wavg$tvltimeindex)
  perf.segm.wavg$congspdpct <- unlist(perf.segm.wavg$congspdpct)
  perf.segm.wavg$comparspdpct <- unlist(perf.segm.wavg$comparspdpct)
  perf.segm.wavg$histavgcongpct <- unlist(perf.segm.wavg$histavgcongpct)
  perf.segm.wavg$tti_pti_ratio <- unlist(perf.segm.wavg$tti_pti_ratio)
  
  # Aggregating Metrics Across Multiple TMCs by Segment
  colsel <- c(9,22,27,29)
  perf.segm.sum <- aggregate(perf.segm[, colsel],
                             list(perf.segm$segment_name),
                             FUN = function(x) sum(x))
  
  # merge weighted averages and Sums by Sement into one dataframe
  perf.segm.merge <- merge(perf.segm.sum, perf.segm.wavg, by = 'Group.1')
  
  # Clean-up Data
  rm(perf.segm.wavg)
  rm(perf.segm.sum)
  
  # List of each Segment's TMCS as string of characters a.k.a. text
  colsel <- c(1)
  perf.segm.tmcs <- aggregate(perf.segm[, colsel],
                              list(perf.segm$segment_name),
                              FUN = function(x) toString(x))
  
  # Clean-up Data
  rm(perf.segm)
  
  # Count of each Segment's TMCS
  perf.segm.tmcs$tmc_count <- count.fields(textConnection(perf.segm.tmcs$x), 
                                           sep = ",")
  
  # Change name of the first column from "Group.1" to "segment_name"
  name.tmc_code <- colnames(perf.segm.tmcs)
  name.tmc_code[2] <- "segment_tmcs"
  colnames(perf.segm.tmcs) <- name.tmc_code
  
  # Append Segment TMC info
  perf.segm.merge <- merge(perf.segm.merge, perf.segm.tmcs, by = 'Group.1')
  
  # Clean-up Data
  rm(perf.segm.tmcs)
  
  # Change name of the first column from "Group.1" to "segment_name"
  name.tmc_code <- colnames(perf.segm.merge)
  name.tmc_code[1] <- "segment_name"
  colnames(perf.segm.merge) <- name.tmc_code
  
  ################################################################################
  
  # Re-Assign Fields
  perf.segm.merge$peakhour <- reference[1, 10]
  perf.segm.merge$season <- reference[1, 8]
  perf.segm.merge$seasoncode <- reference[1, 9]
  perf.segm.merge$year <- reference[1, 7]
  
  # Create a csv.
  outputpath <- "S://CMP//2019//Data//OutputSegm//"
  output <- paste(outputpath, outputname, ".segm", outputext, sep = "")
  write.csv(perf.segm.merge, output)
  
  # Clean-up Data
  rm(perf.segm.merge)
  
  # Index++
  fampm <- iampm + 1
  print(as.name(paste(ampm[iampm], season[iseason], xx[ixx], "!", sep = "")))
  return(fampm)
}

################################################################################
################################################################################
################################################################################
# Calculations
################################################################################
################################################################################
################################################################################

# initialize while loop
xx <- c('15', '16', '17', '18')
ixx <- 1
nxx <- length(xx)
season <- c('win.', 'spr.', 'esum.', 'lsum.', 'fal.', 'hol.')
iseason <- 3 # No Winter or Spring Data in 2015
nseason <- length(season)
ampm <- c('am.wek.', 'pm.wek.')
iampm <- 1
nampm <- length(ampm)

# Calculate TMC and Segment Performance Measures
while (ixx <= nxx) {
  while(iseason <= nseason) {
    while (iampm <= nampm) {
      # Create Copy of Applicable Subset
      reference <- eval(as.name(paste(ampm[iampm], season[iseason], xx[ixx],
                                      sep = "")))
      
      # Clean-up Data
      outputname <- paste(ampm[iampm], season[iseason], xx[ixx], 
                          sep = "")
      #rm(list = ls()[grepl(outputname, ls())])
      
      ##########################################################################
      # Assign Best Fit TMC Length
      ##########################################################################
      
      # Estimate TMC length in Miles based on speed and travel time data
      reference$estmiles = 
        (reference$speed / 60) * (reference$travel_time_minutes)
      
      # Average TMC length estimates by TMC
      estmiles <- aggregate(reference$estmiles, 
                            list(reference$tmc_code), 
                            FUN = function(x) mean(x))
      
      # Format Data for Processing
      estmiles$tmcindex = 0
      estmiles$tmcindex = 1:nrow(estmiles)
      name.tmc_code <- colnames(estmiles)
      name.tmc_code[1] <- "tmc_code"
      name.tmc_code[2] <- "estmiles"
      colnames(estmiles) <- name.tmc_code
      
      # Merge and Sort TMC length Estimates to TMC Identification Matrix
      tmc.id.merge <- merge(tmc.id, estmiles, by = 'tmc_code')
      tmc.id.sort <- tmc.id.merge[order(tmc.id.merge$unique),]
      
      # Form list of Inrix Assigned Miles for each TMC for each Active Period
      tmcseries <- aggregate(tmc.id.sort$miles, 
                             list(tmc.id.sort$tmcindex),
                             FUN = function(x) list(x))
      
      # Rename Fields  
      name.tmc_code <- colnames(tmcseries)
      name.tmc_code[1] <- "tmcindex"
      name.tmc_code[2] <- "givenmiles"
      colnames(tmcseries) <- name.tmc_code
      
      # Initialize The Following While Loop
      tmc.id.sort$tmcselector = 0
      n <- nrow(estmiles)
      N <- nrow(tmcseries)
      i <- 1
      
      # Test for Errors
      #ifelse(n == N, print("Go!"), print("Mayday!"))
      
      # Calculate Which Active Period TMC Length has Best Match with... 
      # Estimated TMC Length from Data
      while (i <= n) {
        calcmiles <- estmiles$estmiles[i]
        givenmiles <- unlist(tmcseries$givenmiles[i], use.names=FALSE)
        tmcselector <- which.min(abs(givenmiles - calcmiles))
        tmc.id.sort$tmcselector <- ifelse(tmc.id.sort$tmcindex == i, 
                                          tmcselector, 
                                          tmc.id.sort$tmcselector)
        i = i+1
      }
      
      # Create subset of data from weekday afternoon "rush hour" in "spring" 18
      tmc.id.mix <- subset(tmc.id.sort, tmcselector == active)
      
      # Clean-up Data
      rm(estmiles)
      rm(tmc.id.merge)
      rm(tmcseries)
      rm(tmc.id.sort)
      
      ################################################################################
      # Aggregate Performance by TMC
      ################################################################################
      
      # Harmonic mean is used when taking the average over a across a given DISTANCE.
      reference.hmean <- aggregate(reference[, 3:5], 
                                   list(reference$tmc_code), 
                                   FUN = function(x) harmonic.mean(x))
      
      # Arithmetic mean is used when taking the average over a given period of TIME.
      reference.mean <- aggregate(reference[, 6], 
                                  list(reference$tmc_code), 
                                  FUN = function(x) mean(x))
      
      # Merge Performance Averages
      reference.means <- merge(reference.hmean, reference.mean, 
                               by = 'Group.1') # Group.1: i.e. tmc_code
      
      # Clean-up Data
      rm(reference.hmean)
      rm(reference.mean)
      
      # Calculate 95th Percentile Performance
      reference.95pctl <- aggregate(reference[, 3:6], 
                                    list(reference$tmc_code), 
                                    FUN = function(x) 
                                      quantile(x, probs = 0.95, na.rm = TRUE))
      
      # merge harmonic speed and 95% travel time by tmc in one data frame
      # This may intentionally overwrite previous merge data frame
      perf <- merge(reference.means, reference.95pctl, # .x & .y
                    by = 'Group.1')  # Group.1: i.e. tmc_code
      
      # Clean-up Data
      rm(reference.means)
      rm(reference.95pctl)
      
      # Change name of the first column from "Group.1" to "tmc_code"
      # This intentionally overwrites previous values set
      name.tmc_code <- colnames(perf)
      name.tmc_code[1] <- "tmc_code"
      name.tmc_code[2] <- "spd_hmean"
      name.tmc_code[3] <- "histavgspd_hmean"
      name.tmc_code[4] <- "refspd_hmean"
      name.tmc_code[5] <- "tvltime_mean"
      name.tmc_code[6] <- "spd_95pctl"
      name.tmc_code[7] <- "histavgspd_95pctl"
      name.tmc_code[8] <- "refspd_95pctl"
      name.tmc_code[9] <- "tvltime_95pctl"
      colnames(perf) <- name.tmc_code
      
      ################################################################################
      # Calculate Performance Measures
      ################################################################################
      
      # Create Performance Measure Dataframe
      perf.meas <- merge(tmc.id.mix, perf, by = 'tmc_code')
      
      # Clean-up Data
      rm(perf)
      rm(tmc.id.mix)
      
      # 1. Buffer time (minutes)
      perf.meas$buftime = (perf.meas$tvltime_95pctl - perf.meas$tvltime_mean)
      
      # 2. Buffer Index
      perf.meas$bufindex = perf.meas$buftime/perf.meas$tvltime_mean
      
      # 1. Buffer time (minutes)
      perf.meas$buftime = (perf.meas$tvltime_95pctl - perf.meas$tvltime_mean)
      
      # 2. Buffer Index
      perf.meas$bufindex = perf.meas$buftime/perf.meas$tvltime_mean
      
      
      # 3. Planning time (minutes)
      perf.meas$plantime = perf.meas$tvltime_95pctl
      
      # 4. Planning time index - planningtime/free-flow travel time.
      #    For free flow speed, RITIS uses historic 85% travel speed
      perf.meas$freeflowtvltime = ((perf.meas$miles)/(perf.meas$refspd_hmean)) * 60
      perf.meas$planindex = (perf.meas$tvltime_95pctl/(perf.meas$freeflowtvltime))
      
      # 5. Travel time index - Travel time represented as a percentage of the...
      #    ideal travel time (Travel Time / Free-flow Travel Time) 
      perf.meas$tvltimeindex = (perf.meas$tvltime_mean/(perf.meas$freeflowtvltime))
      
      # 6. Congestion - Measured speed as a percentage of the free flow speed.
      perf.meas$congspdpct = perf.meas$spd_hmean/perf.meas$refspd_hmean
      
      # 7. Comparative speed - Measured speed as a percentage of the...
      #    historic average speed for this time of day and day of week.
      perf.meas$comparspdpct = perf.meas$spd_hmean/perf.meas$histavgspd_hmean
      
      # 8. Historic Average Congestion - Historic average speed as a percentage of...
      #    the free flow speed for this time of day and day of week.
      perf.meas$histavgcongpct = (perf.meas$histavgspd_hmean/perf.meas$refspd_hmean)
      
      # 9. TTI to PTI: shows the degree of possible unexpected delay (non-recurring)
      perf.meas$tti_pti_ratio = (perf.meas$tvltimeindex/perf.meas$planindex)
      
      # Re-Assign Fields
      perf.meas$peakhour <- reference[1, 10]
      perf.meas$season <- reference[1, 8]
      perf.meas$seasoncode <- reference[1, 9]
      perf.meas$year <- reference[1, 7]
      
      # Save as CSV file
      outputname <- paste(ampm[iampm], season[iseason], xx[ixx], sep = "")
      outputpath <- "S://CMP//2019//Data//Output//"
      outputext <- ".csv"
      output <- paste(outputpath, outputname, outputext, sep = "")
      write.csv(perf.meas, output)
      
      ################################################################################
      # Segment Analysis
      ################################################################################
      
      # Merge Segment ID Information
      perf.segm <- merge(tmc.id.segm, perf.meas,  by = 'tmc_code')
      
      # Clean-up Data
      rm(perf.meas)
      
      print(paste(ampm[iampm], season[iseason], xx[ixx], "?", sep = ""))
      
      # Error Handling
      errorhandling <- nrow(perf.segm)
      iampm <- ifelse(errorhandling == 0, iampm + 1, 
                      segmentanalysis(perf.segm, iampm))
      
      # Clean-up Data
      rm(reference)
      rm(perf.segm)
    }
    iampm = 1
    iseason = iseason + 1
  }
  iseason = 1
  ixx = ixx + 1
}