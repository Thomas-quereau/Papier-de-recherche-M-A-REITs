source('utils.r')

getAverageTable = function(companies)
{
  t = data.table(Day = -100:10)
  t[, "Z_return" := sapply(Day, getDayAAR, companies, "chg")]
  t[, "CZ_return" := cumsum(Z_return)]
  t[, "SMR_AAR" := sapply(Day, getDayAAR, companies, "SMR_AR")]
  t[, "SMR_CAAR" := cumsum(SMR_AAR)]
  t[, "CAPM_AAR" := sapply(Day, getDayAAR, companies, "CAPM_AR")]
  t[, "CAPM_CAAR" := cumsum(CAPM_AAR)]
  return (t)
}

getCAARTables = function(table, fields)
{
  r = list()
  for (name in fields)
  {
    t = (data.table(
      "a = -0" = sapply(0:5, getSum, a = +0, table, name),
      "a = -1" = sapply(0:5, getSum, a = -1, table, name),
      "a = -2" = sapply(0:5, getSum, a = -2, table, name),
      "a = -3" = sapply(0:5, getSum, a = -3, table, name),
      "a = -4" = sapply(0:5, getSum, a = -4, table, name),
      "a = -5" = sapply(0:5, getSum, a = -5, table, name)))
    rownames(t) = paste0("b = ", 0:5)
    r[[name]] = t
  }
  return (r)
}

getSingleCAARTestTable = function(data, field, cfield)
{
  test_table = data.table()
  testField = paste0("t-", field)
  ctestField = paste0("t-", cfield)
  test_table[, Day := data[, Day]]
  test_table[, (field) := data[[field]]]
  test_table[, (cfield) := data[[cfield]]]
  
  computeField = function(name, tname)
  {
    test_table[,(name) := paste0(round(test_table[[name]] * 100, 2), "%")]
    test_table[,(tname) := round(abs(data[[name]]/sd(data[[name]])), 2)]
    test_table[,mask := (1 - pt(test_table[[tname]], length(test_table[[tname]]))) < 0.05]
    stars = factor(test_table$mask, labels=c("", "*"))
    test_table[,(tname) := paste0(as.character(test_table[[tname]]), as.character(stars))]
    test_table[, mask := NULL]
  }
  
  computeField(field, testField)
  computeField(cfield, ctestField)
  test_table = test_table[Day <= 10 & Day >= -10,]
  return(test_table)
}

getCAARTestTables = function(data, fields)
{
  tables = list()
  tables[["ZERO"]] = getSingleCAARTestTable(data, "Z_return", "CZ_return")
  tables[["SMR"]] = getSingleCAARTestTable(data, "SMR_AAR", "SMR_CAAR")
  tables[["CAPM"]] = getSingleCAARTestTable(data, "CAPM_AAR", "CAPM_CAAR")
  return (tables)
}

test = NULL

getSingleAnalysis = function(deals, ftunusChange, data)
{
  companies = list()
  #for each company, calculate the AR in all counterfactual
  for (deal in deals[, M.A.SDC.Deal.Number])
  {
    deal = as.character(deal)
    if(deal %in% names(data) == FALSE) next
    announce = as.Date(deals[M.A.SDC.Deal.Number == deal, Date.Announced])
    company = data.table(data[[deal]])
    company[, Day := ( as.integer(difftime(as.Date(company$`Exchange Date`), announce, unit = "days")) )]
    company[, "Exchange Date" := as.Date(company$`Exchange Date`)]
    setnames(company, '%Chg', "chg")
    
    company = merge(ftunusChange, company, by="Exchange Date")
    company[, chg := as.numeric(gsub(",", ".", chg))]
    company[, ftunus_chg := as.numeric(gsub(",", ".", ftunus_chg))]
    company[, SMR_AR := chg - ftunus_chg]
    
    reg = lm(chg ~ ftunus_chg, company[between(Day, -130, -30)])
    a = reg$coef[("Intercept")]
    b = reg$coef["ftunus_chg"]
    company[, CAPM_AR := chg - b * ftunus_chg]
    companies[[deal]] =  company
  }
  # From  this, generate the different analysis tables
  AARTable = getAverageTable(companies)
  # volume_table = getVolumeAnalysis(companies)
  
  test = companies
  analysis = list()
  analysis[["average_table"]] = AARTable
  analysis[["caar_tables"]] = getCAARTables(AARTable, c("Z_return","SMR_AAR", "CAPM_AAR"))
  analysis[["test_tables"]] = getCAARTestTables(AARTable) 
  return(analysis)
}

getAnalysis = function(deals, indexData, acquirers, targets)
{
  analysis = list()
  analysis[["acquirers"]] = getSingleAnalysis(deals, indexData, acquirers)
  analysis[["targets"]] = getSingleAnalysis(deals, indexData, targets)
  return (analysis)
}
