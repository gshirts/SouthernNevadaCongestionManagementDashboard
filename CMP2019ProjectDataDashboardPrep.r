# Google's R Style Guide states that maximum line length is eighty characters.80
################################################################################

# 2019 Regional Transportation Commission of Southern Nevada, MPO
# Grant C. Shirts, Transportation Planner, Modeling & Analysis Division

# Used to manipulate and analyze transportation data collected by INRIX, Inc.
# Outputs used for the Congestion Management Process

################################################################################

# Set the working directory (wd) a.k.a. file folder. Use "//" (double slash)
setwd("S://CMP//2019//Data//ProjectLists")

# Check that the working directory was set successfully
cat("Your current working directory is", getwd())

# Set file path where packages are stored and check if successful. Use "//"
cat("Your current library trees are:", 
		.libPaths("S://CMP//Methodologies//R//Packages"))

################################################################################

# Install packages (this updates them if they are already installed)
#install.packages("plyr", dependencies = TRUE)
#install.packages("psych", dependencies = TRUE)
install.packages("tidyr", dependencies = TRUE)
install.packages("dplyr", dependencies = TRUE)
install.packages("zoo", dependencies = TRUE)
#install.packages("data.table", dependencies = TRUE)

# Call packages. Don't worry about warnings as long as packages are called to R
#library(plyr)
#library(psych)
library(tidyr)
library(dplyr)
library(zoo)
#library(data.table)


################################################################################
################################################################################
################################################################################
#                           Load and Prepare Data		         			   #
################################################################################
################################################################################
################################################################################

# initialize while loops
fileext <- ".csv"
pathoutput <- "S://CMP//2019//Data//ProjectLists//Output//"
pathinput <- "S://CMP//2019//Data//ProjectLists//Input//"
nameoutput <- "GisProjectsThrough2018"
nameinput <- c("RTC.GISSDE.MPO_CMWG_L_Through2018WithDates", 
               "RTC.GISSDE.MPO_CMWG_X_Through2018WithDates")
nnameinput <- length(nameinput)
inameinput <- 1
datatype <- c("Lines", "Points")
idatatype <- 1
colclass <- c("colline", "colpoint")
colline <- c(15,17,19,22,23,25,27,28,29,30,31,32)
colpoint <- c(13,15,17,21,23,25,26,27,28,29,30)
col <- NULL

while (inameinput <= nnameinput){
  inputid <- paste(pathinput, nameinput[inameinput], fileext, sep = "")
  
  # Import data downloaded from Incidents
  imported <- read.csv(inputid, stringsAsFactors = FALSE, header = TRUE)
  
  # Specific Data Error
  imported$ACTUAL_CONSTR_END <- gsub("2007", "2017", imported$ACTUAL_CONSTR_END)

  # Adjust Field Types
  imported$PROJECT_TYPE <- as.factor(imported$PROJECT_TYPE)
  imported$PROJECT_STATUS <- as.factor(imported$PROJECT_STATUS)
  imported$CONSTR_TYPE <- as.factor(imported$CONSTR_TYPE)
  ifelse(inameinput == 1, assign("col", colline), assign("col", colpoint))
  ncol <- length(col)
  icol <- 1
  while (icol <= ncol) {
  imported[,col[icol]] <- as.Date(imported[,col[icol]], "%m/%d/%Y")
  icol = icol + 1
  }
  
################################################################################
  
  # Create Construction Date Range Fields
  imported$start1 <- imported$ACTUAL_CONSTR_START
  imported$start <- if_else(is.na(imported$start1), imported$EST_CONSTR_START, 
                            imported$start1)
  imported$start1 <- NULL
  imported$end1 <- imported$ACTUAL_CONSTR_END
  imported$end <- if_else(is.na(imported$end1), imported$EST_CONSTR_END, 
                          imported$end1)
  imported$end1 <- NULL
  
  # Fix Data Error of Negative Days
  imported$date_diff <- imported$end - imported$start
  imported$startfix <- if_else(imported$date_diff < 0, imported$end, imported$startfix)
  imported$endfix <- if_else(imported$date_diff < 0, imported$start, imported$endfix)
  imported$start <- if_else(imported$date_diff < 0, imported$startfix, imported$start)
  imported$startfix <- NULL
  imported$end <- if_else(imported$date_diff < 0, imported$endfix, imported$end)
  imported$endfix <- NULL
  imported$date_diff <- imported$end - imported$start
  
  # Remove Projects Without Dates
  withdates <- subset(imported, !is.na(imported$date_diff)) # Not NULL
  rm(imported)
  
  # Number of months different
  withdates$Mdiff <- 
    (as.yearmon(strptime(withdates$end, format = "%Y-%m-%d")) - 
     as.yearmon(strptime(withdates$start, format = "%Y-%m-%d")))*12
  
  # Initialize Loop
  withdates$startmonth <- as.integer(format(withdates$start, format = "%m"))
  withdates$endmonth <- as.integer(format(withdates$end, format = "%m"))
  withdates$startyear <- as.integer(format(withdates$start, format = "%Y"))
  withdates$endyear <- as.integer(format(withdates$end, format = "%Y"))
  withdates$mlist <- NA
  withdates$ylist <- NA
  nrow <- nrow(withdates)
  irow <- 1

  # Loop Through Rows
  while(irow <= nrow){
    nmdiff <- withdates$Mdiff[irow]
    imdiff <- 0
    im <- withdates$startmonth[irow]
    ny <- withdates$endyear[irow] 
    iy <- withdates$startyear[irow]
    while(imdiff <= nmdiff){
      while(iy <= ny){
        nm <- if_else(iy == ny, withdates$endmonth[irow], as.integer(12))
        while(im <= nm){
          append <- im
          assign <- toString(c(withdates$mlist[irow], append))
          withdates$mlist[irow] <- assign
          append <- iy
          assign <- toString(c(withdates$ylist[irow], append))
          withdates$ylist[irow] <- assign
          im = im + 1
          imdiff = imdiff + 1
        }
        iy = iy + 1
        im <- 1
      }
    }
    assign <- unlist(strsplit(unlist(withdates$mlist[irow]), split=", "))
    withdates$mlist[irow] <- list(assign[-1])
    assign <- unlist(strsplit(unlist(withdates$ylist[irow]), split=", "))
    withdates$ylist[irow] <- list(assign[-1])
    irow = irow + 1
  }
  
  # Initialize Loop
  withdates$mylist <- NA
  nrow <- nrow(withdates)
  irow <- 1
  
  # create list of Unique Month Year Combinations Loop
  while(irow <= nrow){
    im <- 1
    nm <- length(unlist(withdates$mlist[irow]))
    months <- unlist(withdates$mlist[irow])
    years <- unlist(withdates$ylist[irow])
    while(im <= nm) {
      season <- # the following ifelse
      ifelse(months[im] %in% c('1','2'), 
             "Winter", 
      ifelse(months[im] %in% c('3','4'), 
             "Spring", 
      ifelse(months[im] %in% c('5','6'), 
             "Early Summer", 
      ifelse(months[im] %in% c('7','8'), 
             "Late Summer", 
      ifelse(months[im] %in% c('9','10'), 
             "Fall", 
      ifelse(months[im] %in% c('11','12'), 
             "Holiday", 
             NA))))))
      append <- gsub(", ", "|", toString(c(season, years[im])))
      assign <- toString(c(withdates$mylist[irow], append))
      withdates$mylist[irow] <- assign
      im = im + 1
    }
    assign <- unique(unlist(strsplit(unlist(withdates$mylist[irow]), split=", ")))
    withdates$mylist[irow] <- list(assign[-1])
    irow = irow + 1
  }
  
  # Turn Lists into Correctly Formatted Observations
  colnames <- colnames(withdates)
  withdates <- separate_rows(withdates, tail(colnames, n=1), sep = ", ", convert = FALSE)
  withdates$mlist <- NULL
  withdates$ylist <- NULL
  withdates$myliststr1 <- gsub("c\\(\"", "", withdates$mylist)
  withdates$myliststr2 <- gsub("\"\\)", "", withdates$myliststr1)
  withdates$myliststr3 <- gsub("\"", "", withdates$myliststr2)
  withdates$myliststr <- withdates$myliststr3
  withdates$myliststr1 <- NULL
  withdates$myliststr2 <- NULL
  withdates$myliststr3 <- NULL
  withdates <- eval(separate(data = withdates, 
                             col = myliststr, 
                             into = c("season", "yeartxt"), sep = "\\|"))
  withdates$mylist <- NULL
  withdates$seasoncode <- 
  ifelse(withdates$season == "Winter", 1, 
         ifelse(withdates$season == "Spring", 2, 
                ifelse(withdates$season == "Early Summer", 3, 
                       ifelse(withdates$season == "Late Summer", 4, 
                              ifelse(withdates$season == "Fall", 5, 
                                     ifelse(withdates$season == "Holiday", 6, 
                                            NA))))))
  withdates$year <- as.numeric(withdates$yeartxt)
  
  # Create Dataframe
  assign(paste(datatype[idatatype], nameoutput, sep = ""), withdates)
  rm(withdates)
  
  idatatype = idatatype + 1
  inameinput = inameinput + 1
}

LinesGisProjectsThrough2018$DataType <- "Line"
PointsGisProjectsThrough2018$DataType <- "Point"

write.csv(LinesGisProjectsThrough2018, paste(pathoutput, 
                                             "LinesGisProjectsThrough2018", 
                                             fileext, sep = ""),
          na = "")

write.csv(PointsGisProjectsThrough2018, paste(pathoutput, 
                                              "PointsGisProjectsThrough2018", 
                                              fileext, sep = ""), 
          na = "")

rm(LinesGisProjectsThrough2018)

################################################################################
################################################################################
################################################################################
#		                            Subset Point Data			              					 #
################################################################################
################################################################################
################################################################################

# Create subset of data from the year 2015 (15)
base.15 <- subset(PointsGisProjectsThrough2018, year == 2015)

# Create subset of data from the year 2016 (16)
base.16 <- subset(PointsGisProjectsThrough2018, year == 2016)

# Create subset of data from the year 2017 (17)
base.17 <- subset(PointsGisProjectsThrough2018, year == 2017)

# Create subset of data from the year 2018 (18)
base.18 <- subset(PointsGisProjectsThrough2018, year == 2018)

# Clean-up data
rm(PointsGisProjectsThrough2018)

################################################################################
################################################################################
# 2015 Subsets
################################################################################
################################################################################

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
                  seasoncode == 3)

# Create subset of data from the "late summer" season of 15
lsum.15 <- subset(base.15, 
                  seasoncode == 4)

# Create subset of data from the "fall" season of 15
fal.15 <- subset(base.15, 
                 seasoncode == 5)

# Create subset of data from the "holiday" season of 15
hol.15 <- subset(base.15, 
                 seasoncode == 6)

# Clean-up Data
rm(base.15)

################################################################################
################################################################################
# 2016 Subsets
################################################################################
################################################################################

# Create subset of data from the "winter" season of 15
win.16 <- subset(base.16, 
                 seasoncode == 1)

# Create subset of data from the "spring" season of 15
spr.16 <- subset(base.16, 
                 seasoncode == 2)

# Create subset of data from the "early summer" season of 15
esum.16 <- subset(base.16, 
                  seasoncode == 3)

# Create subset of data from the "late summer" season of 15
lsum.16 <- subset(base.16, 
                  seasoncode == 4)

# Create subset of data from the "fall" season of 15
fal.16 <- subset(base.16, 
                 seasoncode == 5)

# Create subset of data from the "holiday" season of 15
hol.16 <- subset(base.16, 
                 seasoncode == 6)

# Clean-up Data
rm(base.16)

################################################################################
################################################################################
# 2017 Subsets
################################################################################
################################################################################

# Create subset of data from the "winter" season of 15
win.17 <- subset(base.17, 
                 seasoncode == 1)

# Create subset of data from the "spring" season of 15
spr.17 <- subset(base.17, 
                 seasoncode == 2)

# Create subset of data from the "early summer" season of 15
esum.17 <- subset(base.17, 
                  seasoncode == 3)

# Create subset of data from the "late summer" season of 15
lsum.17 <- subset(base.17, 
                  seasoncode == 4)

# Create subset of data from the "fall" season of 15
fal.17 <- subset(base.17, 
                 seasoncode == 5)

# Create subset of data from the "holiday" season of 15
hol.17 <- subset(base.17, 
                 seasoncode == 6)

# Clean-up Data
rm(base.17)

################################################################################
################################################################################
# 2018 Subsets
################################################################################
################################################################################

# Create subset of data from the "winter" season of 15
win.18 <- subset(base.18, 
                 seasoncode == 1)

# Create subset of data from the "spring" season of 15
spr.18 <- subset(base.18, 
                 seasoncode == 2)

# Create subset of data from the "early summer" season of 15
esum.18 <- subset(base.18, 
                  seasoncode == 3)

# Create subset of data from the "late summer" season of 15
lsum.18 <- subset(base.18, 
                  seasoncode == 4)

# Create subset of data from the "fall" season of 15
fal.18 <- subset(base.18, 
                 seasoncode == 5)

# Create subset of data from the "holiday" season of 15
hol.18 <- subset(base.18, 
                 seasoncode == 6)

# Clean-up Data
rm(base.18)

################################################################################
################################################################################
################################################################################
#		                            Write Points To CSV    			        					 #
################################################################################
################################################################################
################################################################################

# Export To CSV
alldf <- names(which(unlist(eapply(.GlobalEnv,is.data.frame))))
nalldf <- length(alldf)
ialldf <- 1
while(ialldf <= nalldf){
  exportdf <- eval(as.name(alldf[ialldf]))
  write.csv(exportdf, paste(pathoutput, "Points//", alldf[ialldf], fileext, 
                            sep = ""), 
            na = "")
  rm(exportdf)
ialldf = ialldf + 1
}

rm(list=ls())

################################################################################
################################################################################
################################################################################
#		                            Subset Line Data			              					 #
################################################################################
################################################################################
################################################################################

# Import data downloaded from Incidents
LinesGisProjectsThrough2018 <- read.csv("S://CMP//2019//Data//ProjectLists//Output//LinesGisProjectsThrough2018.csv", 
                                         header = TRUE)

# Create subset of data from the year 2015 (15)
base.15 <- subset(LinesGisProjectsThrough2018, year == 2015)

# Create subset of data from the year 2016 (16)
base.16 <- subset(LinesGisProjectsThrough2018, year == 2016)

# Create subset of data from the year 2017 (17)
base.17 <- subset(LinesGisProjectsThrough2018, year == 2017)

# Create subset of data from the year 2018 (18)
base.18 <- subset(LinesGisProjectsThrough2018, year == 2018)

# Clean-up data
rm(LinesGisProjectsThrough2018)

################################################################################
################################################################################
# 2015 Subsets
################################################################################
################################################################################

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
                  seasoncode == 3)

# Create subset of data from the "late summer" season of 15
lsum.15 <- subset(base.15, 
                  seasoncode == 4)

# Create subset of data from the "fall" season of 15
fal.15 <- subset(base.15, 
                 seasoncode == 5)

# Create subset of data from the "holiday" season of 15
hol.15 <- subset(base.15, 
                 seasoncode == 6)

# Clean-up Data
rm(base.15)

################################################################################
################################################################################
# 2016 Subsets
################################################################################
################################################################################

# Create subset of data from the "winter" season of 15
win.16 <- subset(base.16, 
                 seasoncode == 1)

# Create subset of data from the "spring" season of 15
spr.16 <- subset(base.16, 
                 seasoncode == 2)

# Create subset of data from the "early summer" season of 15
esum.16 <- subset(base.16, 
                  seasoncode == 3)

# Create subset of data from the "late summer" season of 15
lsum.16 <- subset(base.16, 
                  seasoncode == 4)

# Create subset of data from the "fall" season of 15
fal.16 <- subset(base.16, 
                 seasoncode == 5)

# Create subset of data from the "holiday" season of 15
hol.16 <- subset(base.16, 
                 seasoncode == 6)

# Clean-up Data
rm(base.16)

################################################################################
################################################################################
# 2017 Subsets
################################################################################
################################################################################

# Create subset of data from the "winter" season of 15
win.17 <- subset(base.17, 
                 seasoncode == 1)

# Create subset of data from the "spring" season of 15
spr.17 <- subset(base.17, 
                 seasoncode == 2)

# Create subset of data from the "early summer" season of 15
esum.17 <- subset(base.17, 
                  seasoncode == 3)

# Create subset of data from the "late summer" season of 15
lsum.17 <- subset(base.17, 
                  seasoncode == 4)

# Create subset of data from the "fall" season of 15
fal.17 <- subset(base.17, 
                 seasoncode == 5)

# Create subset of data from the "holiday" season of 15
hol.17 <- subset(base.17, 
                 seasoncode == 6)

# Clean-up Data
rm(base.17)

################################################################################
################################################################################
# 2018 Subsets
################################################################################
################################################################################

# Create subset of data from the "winter" season of 15
win.18 <- subset(base.18, 
                 seasoncode == 1)

# Create subset of data from the "spring" season of 15
spr.18 <- subset(base.18, 
                 seasoncode == 2)

# Create subset of data from the "early summer" season of 15
esum.18 <- subset(base.18, 
                  seasoncode == 3)

# Create subset of data from the "late summer" season of 15
lsum.18 <- subset(base.18, 
                  seasoncode == 4)

# Create subset of data from the "fall" season of 15
fal.18 <- subset(base.18, 
                 seasoncode == 5)

# Create subset of data from the "holiday" season of 15
hol.18 <- subset(base.18, 
                 seasoncode == 6)

# Clean-up Data
rm(base.18)

################################################################################
################################################################################
################################################################################
#		                            Write Lines To CSV    			        					 #
################################################################################
################################################################################
################################################################################

# Export To CSV
alldf <- names(which(unlist(eapply(.GlobalEnv,is.data.frame))))
nalldf <- length(alldf)
ialldf <- 1
while(ialldf <= nalldf){
  exportdf <- eval(as.name(alldf[ialldf]))
  write.csv(exportdf, 
            paste("S://CMP//2019//Data//ProjectLists//Output//Lines//", 
                  alldf[ialldf],
                  ".csv",
                  sep = ""), 
            na = "")
  rm(exportdf)
  ialldf = ialldf + 1
}
