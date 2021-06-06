# importing libraries
library(data.table, quietly = TRUE)
#plotting
library(ggplot2)
#conversion to percent for printing
library(scales)
#library for latex tables
library(xtable)

readFiles = function(directory)
{
  newSheet = NULL
  sheet = NULL
  columns = NULL
  table = list()
  files = dir(directory, full.names = TRUE)
  for (file in files)
  {
    sheet = setDT(read.csv(file, stringsAsFactors = FALSE))
    lineIndex = grep("[Dd]eal *[Nn]umber",sheet[[1]])
    dealNumber = sheet[c(lineIndex),X]
    # Get the index of the column names for the price table
    lineIndex = grep("Exchange Date",sheet[[1]])
    # Extract the actual price value by dates values from the sheet
    newSheet = sheet[lineIndex+2:nrow(sheet) - 1,] # TO DO is this really propre en fait ?
    # Put the empty values in unused columns to 
    # print(colnames(newSheet))
    # newSheet[newSheet == ""] <--NA
    # Filter the data where its NA
    newSheet<- na.omit(Filter(function(x)!all(is.na(x)), newSheet))
  
    #Getting the new colum names
    columns = as.vector(as.matrix(sheet[lineIndex,]))
    columns <- Filter(function(x)x!="", columns)
    newSheet = newSheet[,1:8]
    newNames = 
    setnames(newSheet, names(newSheet), columns[1:8])
    table[[dealNumber]] = newSheet
  }
  return (table)
}

readFtunus = function(path)
{
  # Reading the FTUNUS index
  ftunusfile = setDT(read.csv(path))
  ftunus = ftunusfile[grep("Exchange Date", ftunusfile[[1]]) + 2:nrow(ftunusfile), ]
  setnames(ftunus, colnames(ftunus), as.vector(as.matrix(ftunusfile[17])))
  setnames(ftunus, '%Chg', 'ftunus_chg')
  ftunus[, 'Exchange Date' := as.Date(ftunus$`Exchange Date`)]
  ftunusChange = ftunus[, c('Exchange Date', 'ftunus_chg')]
  #ftunusChange = Filter(function(x)x!="", ftunusChange)
  ftunusChange = na.omit(Filter(function(x)!all(is.na(x)), ftunusChange))
  return (ftunusChange)
}
