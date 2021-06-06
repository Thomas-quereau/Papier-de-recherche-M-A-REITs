
getSum = function(b, a, table, field)
{
  return(sum(table[between(day, a, b)][[field]]))
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