# importing libraries
library(data.table, quietly = TRUE)

#plotting
library(ggplot2)

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
  newSheet[newSheet == ""] <--NA
  # Filter the data where its NA
  newSheet<- na.omit(Filter(function(x)!all(is.na(x)), newSheet))

  #Getting the new colum names
  columns = as.vector(as.matrix(sheet[lineIndex,]))
  columns <- Filter(function(x)x!="", columns)
  setnames(newSheet, names(newSheet), columns)
  acquirers[[dealNumber]] = newSheet
}



# Reading the FTUNUS index
ftunusfile = setDT(read.csv('./data/Price History_FTUNUS.csv'))
ftunus = ftunusfile[grep("Exchange Date", ftunusfile[[1]]) + 2:nrow(ftunusfile), ]
setnames(ftunus, colnames(ftunus), as.vector(as.matrix(ftunusfile[17])))
setnames(ftunus, '%Chg', 'ftunus_chg')
ftunus[, 'Exchange Date' := as.Date(ftunus$`Exchange Date`)]

ftunusChange = ftunus[, c('Exchange Date', 'ftunus_chg')]
#ftunusChange = Filter(function(x)x!="", ftunusChange)
ftunusChange = na.omit(Filter(function(x)!all(is.na(x)), ftunusChange))

companies = list()
for (deal in names(acquirers))
{
  announce = as.Date(usaDeals[M.A.SDC.Deal.Number == deal, Date.Announced])
  company = data.table(acquirers[[deal]])
  company[, day := ( as.integer(difftime(as.Date(company$`Exchange Date`), announce, unit = "days")) )]
  company[, "Exchange Date" := as.Date(company$`Exchange Date`)]
  setnames(company, '%Chg', "chg")

  print(deal)
  test = company 
  company = merge(ftunusChange, company, by="Exchange Date")
  company[, chg := as.numeric(gsub(",", ".", chg))]
  company[, ftunus_chg := as.numeric(gsub(",", ".", ftunus_chg))]
  company[,AR := chg - ftunus_chg]
  
  reg = lm(chg ~ ftunus_chg, company[between(day, -110, -10)])
  a = reg$coef[("Intercept")]
  b = reg$coef["Rm"]
  company[, CAPM_AR := chg - b * ftunus_chg]
  companies[[deal]] =  company
}

getDayAAR = function(d, companies)
{
  sum = 0
  for (company in companies)
  {
    tmp = company[day == d, "AR"]
    if (dim(tmp)[1] > 0 && !is.na(tmp) && length(tmp) != 0)
      sum = sum + tmp
  }
  return(sum / length(companies))
}

getAverageTable = function(companies)
{
  t = data.table(day = -10:10)
  t[, AAR := sapply(day, getDayAAR, companies)]
  t[, CAAR := cumsum(AAR)]
  return (t)
}

AARTable = getAverageTable(companies)

gg = ggplot(data=AARTable, aes(x = day, y = AAR)) +
 geom_vline(xintercept=0, colour="firebrick", size=1) +
 geom_point(color="blue", size=2.5) +
 geom_path(color="blue", size=1) +
 xlim(-5, 5) +
 ylim(-0.4, 0.4)

print(gg)
