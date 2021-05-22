# importing libraries
library(data.table, quietly = TRUE)

# Clean the environment
rm(list = ls(all = TRUE))

# Script to sanitize the input data



# Get deals
dealsTable = setDT(read.csv("./data/Deals Mémoire.csv"))

usaDeals = dealsTable[Acquiror.Mid.Industry == "REITs"][Acquiror.Nation.of.Primary.Stock.Exchange == "United States"][as.Date(Date.Announced) > as.Date("2003-01-01")]

# Creating a dir to save the data fully sanitized
dir.create('./data_sanitized')



# sanitizing acquirers
files = dir("./data/Acquirers/", full.names = TRUE)
acquirers = list()

sheet = NULL
newSheet = NULL
columns = NULL

function()

for (file in files)
{
  sheet = setDT(read.csv(file, stringsAsFactors = FALSE))
  dealNumber = sheet[7,X]
  
  # Get the index of the column names for the price table
  lineIndex = grep("Exchange Date",sheet[[1]])
  # Extract the actual price value by dates values from the sheet
  newSheet = sheet[lineIndex+1:nrow(sheet),]
  # Put the empty values in unused columns to 
  newSheet[newSheet == ""] <--NA
  # Filter the data where its NA
  newSheet<- Filter(function(x)!all(is.na(x)), newSheet)
  #Gettinge the new colum names
  columns = as.vector(as.matrix(sheet[lineIndex,]))
  columns <- Filter(function(x)x!="", columns)
  setnames(newSheet, names(newSheet), columns)
  acquirers[[dealNumber]] = newSheet
}