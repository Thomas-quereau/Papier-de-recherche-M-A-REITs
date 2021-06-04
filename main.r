# Clean the environment
rm(list = ls(all = TRUE))

source('sanitize.r')
source('functions.r')

# Get deals
dealsTable = setDT(read.csv("./data/Deals Mémoire.csv"))
usaDeals = dealsTable[Acquiror.Mid.Industry == "REITs"][Acquiror.Nation.of.Primary.Stock.Exchange == "United States"][as.Date(Date.Announced) > as.Date("2003-01-01")]





# Reading files
ftunusData = readFtunus('./data/Price History_FTUNUS.csv')
targets = readFiles("./data/Targets/")
# acquirers = readFiles("./data/Acquirers/")

# plzwork = printAnalysis(usaDeals, targets, './output/reits_targets')
awer = printAnalysis(usaDeals, targets, './output/reits_targets')


dir.create('./output/')