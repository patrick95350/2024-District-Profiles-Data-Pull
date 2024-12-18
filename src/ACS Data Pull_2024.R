### ACS Data Pull for District Profile Re-Do
#     Pull State, County, District and Tract-level Data from ACS
#
#     By Patrick Rogers, California Research Bureau
#       Oct 2018, Updated Dec 2025
#     Uses the following Packages
#       stringr
#
#     Uses the following data
#
#		Variables - Updated 2019-01-09_CorrectedS0101_2.csv
#       
#		(Old Version)
#       higher_geo.csv
#       county_fips.csv
#       tract_geo.csv
#       timeseries_geo.csv

# ToDo 
#   The get data from ACS step takes 48 hours to run. Optimize to reduce calls to API
#   county FIPs is coming from saving csv file, update to get from ACS via API

# Clear Workspace
rm(list=ls(all=TRUE))

# Load Packages
library(stringr)
library(here)
library(tictoc)

# User Inits
most.recent.year <- 2023

# Inits
setwd(here::here())
source(file.path(here::here(),  "private", "private_acs_api_key.R"))
variable_list <- file.path(here::here(), "data", "Variables - 2022.csv")

if(!file.exists(file.path(here::here(), "out"))){
  dir.create(file.path(here::here(), "out"))
}

if(!file.exists(file.path(here::here(), "temp"))){
  dir.create(file.path(here::here(), "temp"))
}

# Load Sourcefiles

# Custom Functions
check.contents <- function(pattern, text){
  val <- FALSE
  if(length(grep(pattern, text)) > 0){
    val <- TRUE
  }
  return(val)
}

get.acs.subject.var <- function(endyear, span, geography, variable, key){
  # Set up connection to ACS API
  my.connection <- url(paste0("https://api.census.gov/data/",
                              endyear,
                              "/acs/acs", span,
                              "/subject?",
                              "get=", variable,
                              "&for=", geography,
                              "&key=", key))
  
  # Read to CSV
  my.val <- tryCatch(
    {
      suppressWarnings(read.csv(file=my.connection,
                                na.strings = c("-", "**", "***", "(X)", "N", "null"),
                                stringsAsFactors = FALSE))
    },
    error=function(cond) {
      temp <- data.frame(x1 = NA, x2 = 0, x3 = 0, x4 = 0, x5 = 0)
      colnames(temp)[1] <- "error"
      return(temp)
    }
  )
  
  # Drop last column
  my.val <- my.val[, -length(my.val)]
  
  # Clean formatting
  my.val[,1] <- gsub("[", "", my.val[,1], fixed = T)
  my.val[,ncol(my.val)] <- gsub("]", "", my.val[,ncol(my.val)], fixed = T)
  
  # Determine Geography
  my.geo <- "California"
  my.level <- "State"
  
  if(check.contents("county", colnames(my.val))){
    my.geo <- as.numeric(my.val[,3])
    my.level <- "County"
  }
  
  if(check.contents("upper", colnames(my.val))){
    my.geo <- as.numeric(my.val[,3])
    my.level <- "Senate"
  }
  
  if(check.contents("lower", colnames(my.val))){
    my.geo <- as.numeric(my.val[,3])
    my.level <- "Assembly"
  }
  
  if(check.contents("tract", colnames(my.val))){
    my.geo <- paste0(formatC(my.val[,2], width = 2, flag = "0"),
                     formatC(my.val[,3], width = 3, flag = "0"),
                     formatC(my.val[,4], width = 6, flag = "0"))
    my.level <- "Tract"
  }
  
  if(check.contents("error", colnames(my.val))){
    my.geo <- NA
    my.level <- "Error"
  }
  
  
  my.val <- data.frame(Startyear = endyear-span+1,
                       Endyear = endyear,
                       level = my.level,
                       Geo = my.geo, 
                       Name = variable,
                       Estimate = my.val[,1],
                       stringsAsFactors = FALSE)
  
  return(my.val)
}

##########################
### Variable Breakouts ###
##########################

# Required Format Changed from the long, short and time series list.
# That format had each column a different geography for each ACS table
# New format requires a different file for each geography, with each column being a sub-table within a larger ACS table


# Pseudo Code
# Inits
#  Get list of every geography required
#  Make list of filenames from geographies
#  Read in list of variables
#   Keep only those with 1 in "include" column

# Iterate over every geography:
# {
#   Iterate over every row:
#   {
#     Iterate over every column:
#     {
#       Check if there is a table code entered
#       If yes, do ACS pull for that data table and the relevant geography
#       Should be a single data value, put that into the same cell position as the table was pulled from
#     }
#   }
# Save to separate file for every geography

#  Set starting time
tictoc::tic()

# Get list of geographies
my.geographies <- c("state:06",
                    paste0("county:", str_pad(seq(from=1, to=116, by=2), 3, "left", "0"), "&in=state:06"),
                    paste0("state%20legislative%20district%20(upper%20chamber):", str_pad(1:40, 3, "left", "0"), "&in=state:06"),
                    paste0("state%20legislative%20district%20(lower%20chamber):", str_pad(1:80, 3, "left", "0"), "&in=state:06"))
# Make into Filenames
my.filenames <- gsub(":", "_", my.geographies)
my.filenames <- gsub("&in=state_06", "", my.filenames)
my.filenames <- gsub("state%20legislative%20district%20\\(upper%20chamber\\)", "SD", my.filenames)
my.filenames <- gsub("state%20legislative%20district%20\\(lower%20chamber\\)", "AD", my.filenames)

# Read in list of variables
my.vars <- read.csv(file = variable_list,
                    stringsAsFactors = FALSE)
my.vars <- my.vars[which(my.vars$Include.==1),]

# Get data from ACS
for(k in 1:length(my.geographies)){
  current.geo <- my.geographies[k]
  my.data <- my.vars
  count <- 0
  for(i in 1:nrow(my.vars)){
    for(j in 12:ncol(my.vars)){
      if(my.vars[i,j]!=""){
        count <- count+1
        print(paste0(current.geo, ": ", count, ": ", i, ", ", j))
        my.data[i,j] <- get.acs.subject.var(
          endyear = most.recent.year,
          span = 5,
          geography = current.geo,
          variable = my.vars[i,j],
          key = my.key)$Estimate
      }
    }
  }
  write.csv(my.data, file = paste0("temp/", my.filenames[k], ".csv"), row.names = FALSE)
}

# Merge Individual Files into Combined
my.data <- NULL

for(my.file in list.files(path="temp")){
  temp <- cbind(Geo=my.file,
                read.csv(file = file.path("temp", my.file),
                         header = TRUE,
                         stringsAsFactors = FALSE))
  my.data <- rbind(my.data, temp)
}

# Clean up Geo Names
my.data$Geo <- gsub("_S2503.csv", "", my.data$Geo)
my.data$Geo <- gsub(".csv", "", my.data$Geo)
my.data$Geo <- gsub("y_00", "", my.data$Geo)
my.data$Geo <- gsub("y_0", "", my.data$Geo)
my.data$Geo <- gsub("y_", "", my.data$Geo)
my.data$Geo <- gsub("count", "", my.data$Geo)
my.data$Geo <- gsub("D_00", "D ", my.data$Geo)
my.data$Geo <- gsub("D_0", "D ", my.data$Geo)
my.data$Geo <- gsub("state_06", "California", my.data$Geo)

# Sub County Names in
counties <- read.csv(file = file.path(here::here(), "data", "county_fips.csv"),
                     header = FALSE,
                     stringsAsFactors = FALSE)

for(i in 1:nrow(counties)){
  k <- which(my.data$Geo == counties[i,2])
  my.data$Geo[k] <- counties[i,1]
}

# Sort correctly
my.data <- cbind(Level = 2, my.data)

my.data$Level[which(my.data$Geo=="California")] <- 1
my.data$Level[grep("SD ", my.data$Geo)] <- 3
my.data$Level[grep("AD ", my.data$Geo)] <- 4

my.data <- my.data[order(my.data$Name, my.data$Level),-1]


# Export final data
write.csv(my.data,
          file = file.path("out", paste0(most.recent.year, " ACS Data for District Profiles ", Sys.Date(), ".csv")),
          row.names = FALSE)



# Check ending time
tictoc::toc()

# ###########
# ### EOF ###
# ###########
