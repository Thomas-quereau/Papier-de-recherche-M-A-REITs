printAnalysis = function(analysis, path)
{
  dir.create('./output/', showWarnings = FALSE)
  dir.create(path, showWarnings = FALSE)
  
  
  # print(xtable(
  #   test_table,
  #   caption = "t-values for CAAR and AAR over a 21 plz",
  #   type = "latex"),
  #   table.placement = "H",
  #   caption.placement = "top",
  #   include.rownames = FALSE,
  #   floating = TRUE, latex.environments = "center",
  #   file = "output/test_table.tex")
  # print(xtable(
  #   IntervalTables[[1]],
  #   caption = "t-values for CAAR and AAR over a 21 plz",
  #   type = "latex"),
  #   table.placement = "H",
  #   caption.placement = "top",
  #   include.rownames = FALSE,
  #   floating = TRUE, latex.environments = "center",
  #   file = "output/capm_table.tex")
}