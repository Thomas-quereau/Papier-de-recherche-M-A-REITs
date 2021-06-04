# importing libraries
library(data.table, quietly = TRUE)
#plotting
library(ggplot2)
#conversion to percent for printing
library(scales)
# library for latex tables
library(xtable)

# Clean the environment
rm(list = ls(all = TRUE))

# Script to sanitize the input data



# Get deals
dealsTable = setDT(read.csv("./data/Deals Mémoire.csv"))

usaDeals = dealsTable[Acquiror.Mid.Industry == "REITs"][Acquiror.Nation.of.Primary.Stock.Exchange == "United States"][as.Date(Date.Announced) > as.Date("2003-01-01")]

# Reading the FTUNUS index
ftunusfile = setDT(read.csv('./data/Price History_FTUNUS.csv'))
ftunus = ftunusfile[grep("Exchange Date", ftunusfile[[1]]) + 2:nrow(ftunusfile), ]
setnames(ftunus, colnames(ftunus), as.vector(as.matrix(ftunusfile[17])))
setnames(ftunus, '%Chg', 'ftunus_chg')
ftunus[, 'Exchange Date' := as.Date(ftunus$`Exchange Date`)]
ftunusChange = ftunus[, c('Exchange Date', 'ftunus_chg')]
#ftunusChange = Filter(function(x)x!="", ftunusChange)
ftunusChange = na.omit(Filter(function(x)!all(is.na(x)), ftunusChange))

# mkdir('./output/')
# printAnalysis = function(deals, outputFolder)
# {
#   mkdir(outputFolder)
# }


# sanitizing acquirers
files = dir("./data/Targets/", full.names = TRUE)
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




companies = list()
for (deal in names(acquirers))
{
  announce = as.Date(usaDeals[M.A.SDC.Deal.Number == deal, Date.Announced])
  company = data.table(acquirers[[deal]])
  company[, day := ( as.integer(difftime(as.Date(company$`Exchange Date`), announce, unit = "days")) )]
  company[, "Exchange Date" := as.Date(company$`Exchange Date`)]
  setnames(company, '%Chg', "chg")

  test = company 
  company = merge(ftunusChange, company, by="Exchange Date")
  company[, chg := as.numeric(gsub(",", ".", chg))]
  company[, ftunus_chg := as.numeric(gsub(",", ".", ftunus_chg))]
  company[, AR := chg - ftunus_chg]
  
  reg = lm(chg ~ ftunus_chg, company[between(day, -110, -10)])
  a = reg$coef[("Intercept")]
  b = reg$coef["ftunus_chg"]
  # print(reg)
  company[, CAPM_AR := chg - b * ftunus_chg]
  companies[[deal]] =  company
}

getDayAAR = function(d, companies, field)
{
  sum = 0
  for (company in companies)
  {
    tmp = company[day == d][[field]]
    if (!is.na(tmp) && length(tmp) != 0)
      sum = sum + tmp
  }
  return(sum / length(companies))
}

getAverageTable = function(companies)
{
  t = data.table(day = -10:10)
  t[, ZEROR := sapply(day, getDayAAR, companies, "chg")]
  t[, C_ZEROR := cumsum(ZEROR)]
  t[, AAR := sapply(day, getDayAAR, companies, "AR")]
  t[, CAAR := cumsum(AAR)]
  t[, CAPM_AAR := sapply(day, getDayAAR, companies, "CAPM_AR")]
  t[, CAPM_CAAR := cumsum(CAPM_AAR)]
  return (t)
}

getSum = function(b, a, table, field)
{
  return(sum(table[between(day, a, b)][[field]]))
}

getCAARTables = function(table, fields)
{
  r = list()
  for (name in fields)
  {
    t = (data.table(
      "a = -5" = sapply(0:5, getSum, a = -5, table, name),
      "a = -4" = sapply(0:5, getSum, a = -4, table, name),
      "a = -3" = sapply(0:5, getSum, a = -3, table, name),
      "a = -2" = sapply(0:5, getSum, a = -2, table, name),
      "a = -1" = sapply(0:5, getSum, a = -1, table, name),
      "a = -0" = sapply(0:5, getSum, a = +0, table, name)))
    rownames(t) = paste0("b = ", 0:5)
    r[[name]] = t
  }
  return (r)
}


getTestTable = function(table)
{
  test_table = table
  
  test_table[,AAR := round(AAR * 100, 2)]
  test_table[,CAAR := round(CAAR * 100, 2)]
  # tAAR
  test_table[,tAAR := round(AAR/sd(AAR), 4)]
  test_table[,mask := (1 - pt(tAAR, length(AAR))) < 0.05]
  stars = factor(test_table$mask, labels=c("", "*"))
  test_table[,tAAR := paste0(as.character(tAAR), as.character(stars))]
  
  # tCAAR
  test_table[,tCAAR := round(CAAR/sd(CAAR), 4)]
  test_table[,mask := (1 - pt(tCAAR, length(AAR))) < 0.05]
  stars = factor(test_table$mask, labels=c("","*"))
  test_table[,tCAAR := paste0(as.character(tCAAR), as.character(stars))]
  
  test_table[,mask := NULL] 
  
  test_table[,AAR := paste0(AAR, "%")]
  test_table[,CAAR := paste0(CAAR, "%")]
  return(test_table)
}

AARTable = getAverageTable(companies)
IntervalTables = getCAARTables(AARTable, c("ZEROR","AAR", "CAPM_AAR"))
test_table = getTestTable(AARTable)

gg = ggplot(data=AARTable, aes(x = day, y = AAR)) +
 geom_vline(xintercept=0, colour="firebrick", size=1) +
 geom_point(color="blue", size=2.5) +
 geom_path(color="blue", size=1) +
 xlim(-5, 5) +
 ylim(-0.4, 0.4)

print(gg)
