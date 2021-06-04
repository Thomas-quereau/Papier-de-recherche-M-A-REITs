source('utils.r')

printAnalysis = function(deals, data, outputFolder)
{
  dir.create(outputFolder)
  companies = list()
  for (deal in names(data))
  {
    announce = as.Date(deals[M.A.SDC.Deal.Number == deal, Date.Announced])
    company = data.table(data[[deal]])
    company[, day := ( as.integer(difftime(as.Date(company$`Exchange Date`), announce, unit = "days")) )]
    company[, "Exchange Date" := as.Date(company$`Exchange Date`)]
    setnames(company, '%Chg', "chg")
    
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
  return(test_table)
}