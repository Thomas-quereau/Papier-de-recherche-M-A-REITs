library(dplyr)
# Clean the environment
rm(list = ls(all = TRUE))

source('sanitize.r')
source('analysis.r')
source('printing.r')

# Get deals
dealsTable = setDT(read.csv('./data/Deals Mémoire.csv'))
dealsTable[, valuation := as.numeric(gsub(",", ".",Rank.Value.inc..Net.Debt.of.Target..USD..Millions.))]
dealsTable[, Rank.Value.inc..Net.Debt.of.Target..USD..Millions. := NULL]
# Filtering the deals
usaDeals = dealsTable[Acquiror.Mid.Industry == "REITs"][Acquiror.Nation.of.Primary.Stock.Exchange == "United States"][as.Date(Date.Announced) > as.Date("2003-01-01")]

# Reading files
ftunusData = readFtunus('./data/Price History_FTUNUS.csv')
targets = readFiles("./data/Targets/")
acquirers = readFiles("./data/Acquirers/")

# Generating the analysis
#By 
usaDealsAnalysis = getAnalysis(usaDeals, ftunusData, acquirers, targets)
#usaDealsAnalysis = getAnalysis(usaDeals[1:3,], ftunusData, acquirers, targets)
usaTargetsDeals = usaDeals[M.A.SDC.Deal.Number %in% names(targets)]
usaTargetsDeals$quartile <- ntile(usaTargetsDeals$valuation, 4)
Q1 = getAnalysis(usaTargetsDeals[quartile == 1,], ftunusData, acquirers, targets)
Q2 = getAnalysis(usaTargetsDeals[quartile == 2,], ftunusData, acquirers, targets)
Q3 = getAnalysis(usaTargetsDeals[quartile == 3,], ftunusData, acquirers, targets)
Q4 = getAnalysis(usaTargetsDeals[quartile == 4,], ftunusData, acquirers, targets)

quartileAnalysis = data.table(Quartile=c("25%","50%", "75%", "100%"),
                              "Rank value (millions)"= quantile(usaTargetsDeals$valuation)[2:5],
                              "CAPM CAAR" = c(
                                sum(Q1[["targets"]][["average_table"]][day >= -5 & day <= 5, CAPM_AAR]),
                                sum(Q2[["targets"]][["average_table"]][day >= -5 & day <= 5, CAPM_AAR]),
                                sum(Q3[["targets"]][["average_table"]][day >= -5 & day <= 5, CAPM_AAR]),
                                sum(Q4[["targets"]][["average_table"]][day >= -5 & day <= 5, CAPM_AAR])
                            ))
quartileAnalysis[,("Adjusted") := quartileAnalysis[["CAPM CAAR"]] - sum(usaDealsAnalysis[["targets"]][["average_table"]][day >= -5 & day <= 5, CAPM_AAR])]
quartileAnalysis[,"CAPM CAAR" := paste0(round(quartileAnalysis[["CAPM CAAR"]] * 100,4), "%")]
quartileAnalysis[,"Adjusted" := paste0(round(quartileAnalysis[["Adjusted"]] * 100,4), "%")]
print(quartileAnalysis)

printAnalysis(usaDealsAnalysis, "./output/usa_deals")
printAnalysis(Q1, './output/Q1')
printAnalysis(Q2, './output/Q2')
printAnalysis(Q3, './output/Q3')
printAnalysis(Q4, './output/Q4')
print(xtable(
  quartileAnalysis,
  caption = "CAPM CAAR for the 4 tragets's quartile by valuation",
  type = "latex"),
  table.placement = "H",
  caption.placement = "top",
  include.rownames = FALSE,
  floating = TRUE, latex.environments = "center",
  file = "output/target_quartiles.tex")

targetsData = usaDealsAnalysis[["targets"]][["average_table"]][day <= 10 & day >= -50, c("day", "CAPM_AAR")]
targetsData[, "CAPM CAAR" := cumsum(CAPM_AAR)]
runups = ggplot(data = targetsData, aes(x=day, y=`CAPM CAAR`)) + geom_col(size=1.5) 
print(runups)

targetsData = usaDealsAnalysis[["acquirers"]][["average_table"]][day <= 10 & day >= -50, c("day", "CAPM_AAR")]
targetsData[, "CAPM CAAR" := cumsum(CAPM_AAR)]
runups = ggplot(data = targetsData, aes(x=day, y=`CAPM CAAR`)) + geom_col(size=1.5) 
print(runups)
