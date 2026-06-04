gVars <- shiny::reactiveValues(
  phTable = NULL,             # setting pheno data matrix
  sampleColID = FALSE,      # setting sample column number
  doseColID = NULL,         # setting dose column number
  TPColID = NULL,             # setting TP column number
  inputGx = NULL,             # setting gene expression matrix
  KEGG_MAT = NULL,
  lev1_h = NULL,
  lev2_h = NULL,
  lev3_h = NULL,
  n_groups = NULL,
  hierarchy = NULL,
  GList = NULL,
  pheno = NULL, #original pheno, used as backup
  exp_ann = NULL,
  reduced_kegg_hierarchy = NULL,
  toPlot = NULL,
  toPlotMap = NULL,
  clust_mat = NULL,
  samplesID = NULL,
  nSamples = NULL,
  nPath = NULL,
  DATA = NULL,
  rgList = NULL,
  celDir = NULL,
  totalSamples = NULL,
  filteredSamples = NULL,
  removedSamples = NULL,
  norm.data = NULL,
  pcChoices = NULL,
  comb.data = NULL,
  agg.data = NULL,
  comps = list(),
  loadedRaw = FALSE,
  QC_passed = FALSE,
  filtered = FALSE,
  normalized = FALSE,
  corrected = FALSE,
  gxTable = NULL, 
  dgxTable = NULL,
  clustered = 1,
  
  filtered_bmd = NULL, #used to store the latest data table to use in BMD analysis
  unfilteredPval = NULL, #saved for plotting PIE charts in filtering
  selectedPvalTh = NULL, #this is needed in the pie chart plotting in case anova/ trend are run multiple times
  bmdlatestTable = NULL, #keeps latest bmd result
  BMDMQ_latest = NULL, #keeps latest result
  in_glp_mode = FALSE #states if run in glp mode or not - i.e. is extensive logging performed or not
)

gVars$sepChoices <- c("TAB", ",", ";", "SPACE", "OTHER")
gVars$quoteChoices <- c(NA, "SINGLE", "DOUBLE")

gVars$pvAdjChoices <- c("Holm" = "holm", 
                        "Hochberg" = "hochberg", 
                        "Hommel" = "hommel", 
                        "Bonferroni" = "bonferroni", 
                        "Benjamini & Hochberg" = "BH",
                        "Benjamini & Yekutieli" = "BY",
                        "False Detection Rate" = "fdr", 
                        "None" = "none")

gVars$normChoices <- c("Between Arrays" = "BA", 
                       "Quantile" = "quantile", 
                       "Variance Stabilizing" = "vsn", 
                       "Cyclic Loess" = "cl")

gVars$baChoices <- c("None" = "none", 
                     "Scale" = "scale", 
                     "Quantile" = "quantile", 
                     "Cyclic Loess" = "cyclicloess")

gVars$is_pheno_uploaded = FALSE
gVars$is_gene_expression_uploaded = FALSE

gVars$was_anova_performed = FALSE
gVars$was_trend_test_performed = FALSE
gVars$was_logFC_filtering_performed = FALSE
gVars$was_filtered_skipped = FALSE
gVars$final_filtering_strategy = NULL
gVars$is_average_computed = FALSE
gVars$is_optimal_model_selected = FALSE
gVars$is_bmd_filtering_performed = FALSE

gVars$last_step_performed = NULL

logVars <- shiny::reactiveValues( #vars to make up logging and report - latest version used for report
  phenoDescription = NULL,
  expDescription = NULL,
  skipFilterReason = NULL,
  anovaFilterReason = NULL,
  trendFilterReason = NULL,
  anovaFilterData = NULL,
  projectDescription = NULL,
  computeBMDReason = NULL,
  file = NULL,
  BMDFilterJustification = NULL,
  BMDAverageJustification = NULL,
  BMDResetJustification = NULL,
  BMDRowPlotJustification = NULL,
  BMDValuesJustification = NULL,
  BMDLOOFJustification = NULL,
  BMDBMD_BMDLJustification = NULL,
  BMDFMJustification = NULL,
  BMDGTPJustification = NULL
)