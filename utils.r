
getSum = function(b, a, table, field)
{
  return(paste0(round(sum(table[between(day, a, b)][[field]]) * 100, 4), "%"))
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