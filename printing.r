printAnalysis = function(analysis, path)
{
  dir.create(path, showWarnings = FALSE)
  for (anal in names(analysis))
  {
    print(anal)
    subpath = sprintf("%s/%s", path, anal)
    dir.create(subpath, showWarnings = FALSE)
    
    caar = analysis[[anal]][["caar_tables"]]
    subsubpath = sprintf("%s/caar", subpath)
    dir.create(subsubpath, showWarnings = FALSE)
    captions = c(CAPM_AAR="CAPM",SMR_AAR="SMR",Z_return="Zero-return")
    for (name in names(caar))
    {
      filename = sprintf("%s/%s.tex", subsubpath, name)
      
      print(xtable(
        caar[[name]],
        caption = sprintf("%s CAAR for %s between -5 and 5 days around event", captions[[name]], anal),
        type = "latex"),
        table.placement = "H",
        caption.placement = "top",
        include.rownames = TRUE,
        floating = TRUE, latex.environments = "center",
        file = filename)
    }    
    
    test = analysis[[anal]][["test_tables"]]
    subsubpath = sprintf("%s/t-test", subpath)
    dir.create(subsubpath, showWarnings = FALSE)
    captions = c(CAPM="CAPM",SMR="SMR",ZERO="Zero-return")
    for (name in names(test))
    {
      filename = sprintf("%s/%s.tex", subsubpath, name)
      colnames(test[[name]]) = gsub("_", " ", colnames(test[[name]]))
      print(xtable(
        test[[name]],
        caption = sprintf("t-test for %s AAR and CAAR for %s between -10 and 10 days around event", captions[[name]], anal),
        type = "latex"),
        table.placement = "H",
        caption.placement = "top",
        include.rownames = FALSE,
        floating = TRUE, latex.environments = "center",
        file = filename)
    }
  }
}
