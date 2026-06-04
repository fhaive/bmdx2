glpModeCallback <- function(value) { return(value) }

create_directory_for_log = function(mode, gVars,logVars){
  if (mode == "TRUE") {
    print("running in glp mode")
    gVars$in_glp_mode <- TRUE
    
    #create log folder
    ts = Sys.time()
    tsn = as.numeric(ts)
    #TODO change to user input if wanted
    
    if (!dir.exists(paths = "GLP_logs/")) {
      dir.create(path = "GLP_logs")
    }
    
    logVars$location = paste(c("GLP_logs/",toString(tsn)), collapse = "")
    
    dir.create(file.path(logVars$location))
    dir.create(file.path(paste(logVars$location, "/tables", sep = "")))
    dir.create(file.path(paste(logVars$location, "/plots", sep = "")))
    
    #create log file
    fileConn <- file(paste(logVars$location,"/glp_log.log", sep = ""))
    writeLines(c("BMDX2 GLP Mode Action Log", paste("Start Time:", ts, sep = " ")), fileConn)
    close(fileConn)
    
    logVars$file = paste(logVars$location,"/glp_log.log", sep = "")
    
  } else{
    print("running in normal mode")
    gVars$in_glp_mode <- FALSE
  } 
  
  return(list(gVars, logVars))
  
}


glp_alerts = function(logVars) {
  if (is.null(logVars$file)) {return(NULL) }
  
  shinyalert(
    title = "You selected GLP Mode",
    text = paste("Your logs will be saved in ", logVars$location, "of the installation/ project directiory", sep = ""),
    size = "s", 
    closeOnEsc = TRUE,
    closeOnClickOutside = FALSE,
    html = FALSE,
    type = "info",
    showConfirmButton = TRUE,
    showCancelButton = FALSE,
    confirmButtonText = "OK",
    confirmButtonCol = "#AEDEF4",
    timer = 0,
    imageUrl = "",
    animation = TRUE
  )
  
  #GLP MODE
  shinyalert(
    title = "GLP Mode Justification",
    text = "Please provide a project description, reason for running this analysis & expected outcome.",
    size = "s", 
    closeOnEsc = TRUE,
    closeOnClickOutside = FALSE,
    html = FALSE,
    type = "input",
    inputType = "text",
    inputValue = "",
    inputPlaceholder = "",
    showConfirmButton = TRUE,
    showCancelButton = FALSE,
    confirmButtonText = "Submit",
    confirmButtonCol = "#AEDEF4",
    timer = 0,
    imageUrl = "",
    animation = TRUE,
    callbackR = function(x) {logVars$projectDescription = x}
    
  )
  
  return(logVars)
}

log_project_description = function(logVars){
  if (is.null(logVars$projectDescription)) {
    return(NULL)
  }
  print("writing log file")
  line = "-----------------------------------------------------"
  write(line,file = logVars$file,append = TRUE)
  
  line = "PROJECT DESCRIPTION"
  write(line,file = logVars$file,append = TRUE)
  
  line = paste("", logVars$projectDescription, sep = " ")
  write(line,file = logVars$file,append = TRUE)
}

log_load_pheno = function(logVars){
  if (is.null(logVars$phenoDescription)) {
    return(NULL)
  }
  print("writing log file")
  line = "-----------------------------------------------------"
  write(line,file = logVars$file,append = TRUE)
  
  line = "LOAD PHENOTYPE DATA"
  write(line,file = logVars$file,append = TRUE)
  
  ts  =  Sys.time()
  line = paste("Timestamp:", logVars$phenoTimestamp, sep = " ")
  write(line,file = logVars$file,append = TRUE)
  
  line = paste("File:", logVars$phenoFile, sep = " ")
  write(line,file = logVars$file,append = TRUE)
  
  line = paste("Description of Pheno File:", logVars$phenoDescription, sep = " ")
  write(line,file = logVars$file,append = TRUE)
  
  line = paste("Columns of Interest", "", sep = " ")
  write(line,file = logVars$file,append = TRUE)
  
  line = paste("Sample ID Variable:", logVars$phenoSampleVar, sep = " ")
  write(line,file = logVars$file,append = TRUE)
  
  line = paste("Dose Variable:",  logVars$phenoDoseVar, sep = " ")
  write(line,file = logVars$file,append = TRUE)
  
  line = paste("Time Point Variable:", logVars$phenoTPVar, sep = " ")
  write(line,file = logVars$file,append = TRUE)
  
  line = paste("Optional Variables of Interest:", logVars$phenoOptVar, sep = " ")
  write(line,file = logVars$file,append = TRUE)
}

log_expression_description = function(logVars){
  if (is.null(logVars$expDescription)) {
    return(NULL)
  }
  print("writing log file")
  
  line = paste("Timestamp:", logVars$expTimestamp, sep = " ")
  write(line,file = logVars$file,append = TRUE)
  
  line = paste("File:", logVars$expFile, sep = " ")
  write(line,file = logVars$file,append = TRUE)
  
  line = paste("Description of Expression File:", logVars$expDescription, sep = " ")
  write(line,file = logVars$file,append = TRUE)
  
  line = paste("X Variable Name:", logVars$expX, sep = " ")
  write(line,file = logVars$file,append = TRUE)
  
  line = paste("Y Variable Name:", logVars$expY, sep = " ")
  write(line,file = logVars$file,append = TRUE)
}

log_skip_filtering = function(logVars){
  if (is.null(logVars$skipFilterReason)) {
    return(NULL)
  }
  print("writing log file")
  line = "-----------------------------------------------------"
  write(line,file = logVars$file,append = TRUE)
  
  line = "GENE FILTERING"
  write(line,file = logVars$file,append = TRUE)
  
  line = "SKIP FILTERING"
  write(line,file = logVars$file,append = TRUE)
  
  ts  =  Sys.time()
  line = paste("Timestamp:", logVars$skipFilterTimestamp, sep = " ")
  write(line,file = logVars$file,append = TRUE)
  
  line = paste("Justification for this Step & Parameters:", logVars$skipFilterReason, sep = " ")
  write(line,file = logVars$file,append = TRUE)
  
}

log_trend_filter = function(logVars){
  if (is.null(logVars$trendFilterReason)) {
    return(NULL)
  }
  print("writing log file")
  
  line = paste("Timestamp:", logVars$trendFilterTimestamp, sep = " ")
  write(line,file = logVars$file,append = TRUE)
  
  line = paste("Justification for Step and Parameters:", logVars$trendFilterReason, sep = " ")
  write(line,file = logVars$file,append = TRUE)
  
  line = paste("Parameters", "", sep = " ")
  write(line,file = logVars$file,append = TRUE)
  
  line = paste("Pvalue Adjustment Method:", logVars$trendFilterPvalAdj, sep = " ")
  write(line,file = logVars$file,append = TRUE)
  
  line = paste("Trend PValue Threshold:",  logVars$trendFilterPvalTh, sep = " ")
  write(line,file = logVars$file,append = TRUE)
  
  line = paste("Number of cores:", logVars$trendFilterCores, sep = " ")
  write(line,file = logVars$file,append = TRUE)
}

log_anova_filtering = function(logVars){
  if (is.null(logVars$anovaFilterReason)) {
    return(NULL)
  }
  print("writing log file")
  
  line = paste("Timestamp:", logVars$anovaFilterTimestamp, sep = " ")
  write(line,file = logVars$file,append = TRUE)
  
  line = paste("Justification for Step and Parameters:", logVars$anovaFilterReason, sep = " ")
  write(line,file = logVars$file,append = TRUE)
  
  line = paste("Parameters", "", sep = " ")
  write(line,file = logVars$file,append = TRUE)
  
  line = paste("Pvalue Adjustment Method:", logVars$anovaFilterPvalAdj, sep = " ")
  write(line,file = logVars$file,append = TRUE)
  
  line = paste("Anova PValue Threshold:",  logVars$anovaFilterPvalTh, sep = " ")
  write(line,file = logVars$file,append = TRUE)
  
  line = paste("Number of cores:", logVars$anovaFilterCores, sep = " ")
  write(line,file = logVars$file,append = TRUE)
}

log_fc_filtering = function(logVars){
  if (is.null(logVars$fcFilterReason)) {
    return(NULL)
  }
  print("writing log file")
  
  line = paste("Timestamp:", logVars$fcFilterTimestamp, sep = " ")
  write(line,file = logVars$file,append = TRUE)
  
  line = paste("Justification for Step and Parameters:", logVars$fcFilterReason, sep = " ")
  write(line,file = logVars$file,append = TRUE)
  
  line = paste("Parameters", "", sep = " ")
  write(line,file = logVars$file,append = TRUE)
  
  line = paste("Pvalue Adjustment Method:", logVars$fcFilterPvalAdj, sep = " ")
  write(line,file = logVars$file,append = TRUE)
  
  line = paste("Limma PValue Threshold:",  logVars$fcFilterPvalTh, sep = " ")
  write(line,file = logVars$file,append = TRUE)
  
  line = paste("Foldchange Threshold:",  logVars$fcFilteredFCth, sep = " ")
  write(line,file = logVars$file,append = TRUE)
  
  line = paste("Number of cores:", logVars$fcFilterCores, sep = " ")
  write(line,file = logVars$file,append = TRUE)
}


log_bmd = function(logVars){
  if (is.null(logVars$computeBMDReason)) {
    return(NULL)
  }
  print("writing log file")
  
  ts  =  Sys.time()
  line = paste("Timestamp:", logVars$computeBMDTimestamp, sep = " ")
  write(line,file = logVars$file,append = TRUE)
  
  line = paste("Justification for this Step & Parameters:", logVars$computeBMDReason, sep = " ")
  write(line,file = logVars$file,append = TRUE)
  
  line = paste("Parameters", "", sep = " ")
  write(line,file = logVars$file,append = TRUE)
  
  line = paste("Number of Cores:", logVars$computeBMDCores, sep = " ")
  write(line,file = logVars$file,append = TRUE)
  
  line = paste("Number of Maximum Iterations:", logVars$computeBMDmaxIter, sep = " ")
  write(line,file = logVars$file,append = TRUE)
  
  line = paste("Variance Type:", logVars$variance_type, sep = " ")
  write(line,file = logVars$file,append = TRUE)
  
  # line = paste("Assumption of Constant Variance:", logVars$computeBMDconstVar, sep = " ")
  # write(line,file = logVars$file,append = TRUE)
  # 
  # line = paste("Assumption of Model Variance:",  logVars$computeBMDmodelVar, sep = " ")
  # write(line,file = logVars$file,append = TRUE)
  
  line = paste("Lack-of-fit PValue Threshold:", logVars$computeBMDLOOF, sep = " ")
  write(line,file = logVars$file,append = TRUE)
  
  line = paste("Response Level/ Risk Factor:", logVars$computeBMDRiskFactor, sep = " ")
  write(line,file = logVars$file,append = TRUE)
  
  line = paste("Significance Threshold:", logVars$computeBMDSigTh, sep = " ")
  write(line,file = logVars$file,append = TRUE)
  
  line = paste("Confidence Interval:", logVars$computeBMDConfInt, sep = " ")
  write(line,file = logVars$file,append = TRUE)
  
  line = paste("BMD Analysis Setting:", logVars$computeBMDAnSetting, sep = " ")
  write(line,file = logVars$file,append = TRUE)
  
  line = paste("Data Type:", logVars$computeBMDdataType , sep = " ")
  write(line,file = logVars$file,append = TRUE)
  
  line = paste("Deviation Type:", logVars$computeBMDdevType, sep = " ")
  write(line,file = logVars$file,append = TRUE)
  
  line = paste("Selected Models:", logVars$computeBMDmodels, sep = " ")
  write(line,file = logVars$file,append = TRUE)
  
}

log_filter_bmd = function(logVars){
  if (is.null(logVars$BMDFilterJustification)) {
    return(NULL)
  }
  
  ts  =  Sys.time()
  line = paste("Timestamp:", logVars$BMDFilterTimestamp, sep = " ")
  write(line,file = logVars$file,append = TRUE)
  
  line = paste("Justification for this Step & Parameters:", logVars$BMDFilterJustification, sep = " ")
  write(line,file = logVars$file,append = TRUE)
  
  line = paste("Filtering Paramters", "", sep = " ")
  write(line,file = logVars$file,append = TRUE)
  
  line = paste("BMD/BMDL ratio:", logVars$BMDFilterBMDBMDL, sep = " ")
  write(line,file = logVars$file,append = TRUE)
  
  line = paste("BMDU/BMD ratio:", logVars$BMDFilterBMDUBMD, sep = " ")
  write(line,file = logVars$file,append = TRUE)
  
  line = paste("BMDU/BMDL ratio:", logVars$BMDFilterBMDUBMDL, sep = " ")
  write(line,file = logVars$file,append = TRUE)
  
  line = paste("Threshold for the Lack of Fit Pvalue:", logVars$BMDFilterLOOFth, sep = " ")
  write(line,file = logVars$file,append = TRUE)
  
  line = paste("Lowest dose filter:",  logVars$BMDFilterlowDF, sep = " ")
  write(line,file = logVars$file,append = TRUE)
  
  line = paste("Highest dose filter:", logVars$BMDFilterhighDF, sep = " ")
  write(line,file = logVars$file,append = TRUE)
  
  line = paste("Apply ratio filters:", logVars$BMDFilterratioF, sep = " ")
  write(line,file = logVars$file,append = TRUE)
  
  line = paste("Filter by Lower Boundary:", logVars$BMDFilterlowBF, sep = " ")
  write(line,file = logVars$file,append = TRUE)
  
  line = paste("Filter by Upper Boundary:", logVars$BMDFilterhighBF, sep = " ")
  write(line,file = logVars$file,append = TRUE)
  
  line = paste("Filter by Lack of Fit:", logVars$BMDFilterLOOFF, sep = " ")
  write(line,file = logVars$file,append = TRUE)
  
  line = paste("Filter BMD NAs:", logVars$BMDFilterNABMD, sep = " ")
  write(line,file = logVars$file,append = TRUE)
  
  line = paste("Filter BMDL NAs:", logVars$BMDFilterNABMDL, sep = " ")
  write(line,file = logVars$file,append = TRUE)
  
  line = paste("Filter BMDU NAs:", logVars$BMDFilterNABMDU, sep = " ")
  write(line,file = logVars$file,append = TRUE)
  
  line = paste("Filter IC50 NAs:", logVars$BMDFilterNAIC50, sep = " ")
  write(line,file = logVars$file,append = TRUE)
  
  line = paste("Number of cores:", logVars$BMDFilterNcores, sep = " ")
  write(line,file = logVars$file,append = TRUE)
}

log_model_average = function(logVars){
  if (is.null(logVars$BMDAverageJustification)) {
    return(NULL)
  }
  
  ts = Sys.time()
  line = paste("Timestamp:", logVars$BMDAverageTimestamp, sep = " ")
  write(line,file = logVars$file,append = TRUE)
  
  line = paste("Justification for this Step & Parameters:", logVars$BMDAverageJustification, sep = " ")
  write(line,file = logVars$file,append = TRUE)
  
}


log_optimal_model_selection = function(logVars){
  if (is.null(logVars$OptModSelJustification)) {
    return(NULL)
  }
  
  ts  =  Sys.time()
  line = paste("Timestamp:", logVars$OptModSelFilterTimestamp, sep = " ")
  write(line,file = logVars$file,append = TRUE)
  
  line = paste("Method used:", logVars$OptModSelMethod, sep = " ")
  write(line,file = logVars$file,append = TRUE)
  
  line = paste("Justification for this Step & Parameters:", logVars$OptModSelJustification, sep = " ")
  write(line,file = logVars$file,append = TRUE)
  
  
}


log_reset_model_filter_average = function(logVars){
  if (is.null(logVars$BMDResetJustification)) {
    return(NULL)
  }
  
  ts  =  Sys.time()
  line = paste("Timestamp:", logVars$BMDResetTimestamp, sep = " ")
  write(line,file = logVars$file,append = TRUE)
  
  line = paste("Justification for this Step & Parameters:", logVars$BMDResetJustification, sep = " ")
  write(line,file = logVars$file,append = TRUE)
}

log_bmd_plot_model = function(logVars){
  if (is.null(logVars$BMDRowPlotJustification)) {
    return(NULL)
  }
  ts  =  Sys.time()
  line = paste("Timestamp:", logVars$BMDRowTimestamp, sep = " ")
  write(line,file = logVars$file,append = TRUE)
  
  line = paste("Experiment:", logVars$BMDRowExperiment, sep = " ")
  write(line,file = logVars$file,append = TRUE)
  
  line = paste("Other Columns:", logVars$BMDRowOtherCol , sep = " ")
  write(line,file = logVars$file,append = TRUE)
  
  line = paste("Feature:", logVars$BMDRowFeature, sep = " ")
  write(line,file = logVars$file,append = TRUE)
  
  line = paste("Model:", logVars$BMDRowModel, sep = " ")
  write(line,file = logVars$file,append = TRUE)
}

log_bmd_value_plot_justification = function(logVars){
  if (is.null(logVars$BMDValuesJustification)) {
    return(NULL)
  }
  
  ts  =  Sys.time()
  line = paste("Timestamp:", logVars$BMDValuesTimestamp, sep = " ")
  write(line,file = logVars$file,append = TRUE)
  
  line = paste("Justification:", logVars$BMDValuesJustification, sep = " ")
  write(line,file = logVars$file,append = TRUE)
  
  line = paste("Color By:", logVars$BMDValuesColorby, sep = " ")
  write(line,file = logVars$file,append = TRUE)
  
  line = paste("Group By X:", logVars$BMDValuesGroupbyX , sep = " ")
  write(line,file = logVars$file,append = TRUE)
  
  line = paste("Group By Y:", logVars$BMDValuesGroupByY, sep = " ")
  write(line,file = logVars$file,append = TRUE)
  
  line = paste("Filter By Column:", logVars$BMDValuesFilterbyCol, sep = " ")
  write(line,file = logVars$file,append = TRUE)
  
  line = paste("Filter By Values:", logVars$BMDValuesFilterbyVals, sep = " ")
  write(line,file = logVars$file,append = TRUE)
  
}


log_fitting_statistics_plot  =  function(logVars){
  if (is.null(logVars$BMDLOOFJustification)) {
    return(NULL)
  }
  
  ts  =  Sys.time()
  line = paste("Timestamp:", logVars$BMDLOOFTimestamp, sep = " ")
  write(line,file = logVars$file,append = TRUE)
  
  line = paste("Justification:", logVars$BMDLOOFJustification, sep = " ")
  write(line,file = logVars$file,append = TRUE)
  
  line = paste("Color By:", logVars$BMDLOOFColorby, sep = " ")
  write(line,file = logVars$file,append = TRUE)
  
  line = paste("Group By X:", logVars$BMDLOOFGroupbyX , sep = " ")
  write(line,file = logVars$file,append = TRUE)
  
  line = paste("Group By Y:", logVars$BMDLOOFGroupByY, sep = " ")
  write(line,file = logVars$file,append = TRUE)
  
  line = paste("Filter By Column:", logVars$BMDLOOFFilterbyCol, sep = " ")
  write(line,file = logVars$file,append = TRUE)
  
  line = paste("Filter By Values:", logVars$BMDLOOFFilterbyVals, sep = " ")
  write(line,file = logVars$file,append = TRUE)
}

# log_R2_plot  =  function(logVars){
#   if (is.null(logVars$BMDR2Justification)) {
#     return(NULL)
#   }
#   
#   ts  =  Sys.time()
#   line = paste("Timestamp:", logVars$BMDR2Timestamp, sep = " ")
#   write(line,file = logVars$file,append = TRUE)
#   
#   line = paste("Justification:", logVars$BMDR2Justification, sep = " ")
#   write(line,file = logVars$file,append = TRUE)
#   
#   line = paste("Color By:", logVars$BMDR2Colorby, sep = " ")
#   write(line,file = logVars$file,append = TRUE)
#   
#   line = paste("Group By X:", logVars$BMDR2GroupbyX , sep = " ")
#   write(line,file = logVars$file,append = TRUE)
#   
#   line = paste("Group By Y:", logVars$BMDR2GroupByY, sep = " ")
#   write(line,file = logVars$file,append = TRUE)
#   
#   line = paste("Filter By Column:", logVars$BMDR2FilterbyCol, sep = " ")
#   write(line,file = logVars$file,append = TRUE)
#   
#   line = paste("Filter By Values:", logVars$BMDR2FilterbyVals, sep = " ")
#   write(line,file = logVars$file,append = TRUE)
# }


log_plot_nr_genes_by_time_point  =  function(logVars){
  if (is.null(logVars$BMDGTPJustification)) {
    return(NULL)
  }
  
  ts  =  Sys.time()
  line = paste("Timestamp:", logVars$BMDGTPTimestamp, sep = " ")
  write(line,file = logVars$file,append = TRUE)
  
  line = paste("Justification:", logVars$BMDGTPJustification, sep = " ")
  write(line,file = logVars$file,append = TRUE)
  
  line = paste("Group By:", logVars$BMDGTPGroupbyX , sep = " ")
  write(line,file = logVars$file,append = TRUE)
  
  line = paste("Filter By Column:", logVars$BMDGTPFilterbyCol, sep = " ")
  write(line,file = logVars$file,append = TRUE)
  
  line = paste("Filter By Values:", logVars$BMDGTPFilterbyVals, sep = " ")
  write(line,file = logVars$file,append = TRUE)
  
}

log_bmd_bml_plot  =  function(logVars){
  if (is.null(logVars$BMDBMD_BMDLJustification)) {
    return(NULL)
  }
  
  ts  =  Sys.time()
  line = paste("Timestamp:", logVars$BMDBMD_BMDLTimestamp, sep = " ")
  write(line,file = logVars$file,append = TRUE)
  
  line = paste("Justification:", logVars$BMDBMD_BMDLJustification, sep = " ")
  write(line,file = logVars$file,append = TRUE)
  
  line = paste("Color By:", logVars$BMDBMD_BMDLColorby, sep = " ")
  write(line,file = logVars$file,append = TRUE)
  
  line = paste("Group By X:", logVars$BMDBMD_BMDLGroupbyX , sep = " ")
  write(line,file = logVars$file,append = TRUE)
  
  line = paste("Group By Y:", logVars$BMDBMD_BMDLGroupByY, sep = " ")
  write(line,file = logVars$file,append = TRUE)
  
  line = paste("Filter By Column:", logVars$BMDBMD_BMDLFilterbyCol, sep = " ")
  write(line,file = logVars$file,append = TRUE)
  
  line = paste("Filter By Values:", logVars$BMDBMD_BMDLFilterbyVals, sep = " ")
  write(line,file = logVars$file,append = TRUE)
  
}

log_bmd_bmdl_plot  =  function(logVars){
  if (is.null(logVars$BMDFMJustification)) {
    return(NULL)
  }
  
  ts  =  Sys.time()
  line = paste("Timestamp:", logVars$BMDFMTimestamp, sep = " ")
  write(line,file = logVars$file,append = TRUE)
  
  line = paste("Justification:", logVars$BMDFMJustification, sep = " ")
  write(line,file = logVars$file,append = TRUE)
  
  line = paste("Group By X:", logVars$BMDValuesGroupbyX , sep = " ")
  write(line,file = logVars$file,append = TRUE)
  
  line = paste("Group By Y:", logVars$BMDFMGroupByY, sep = " ")
  write(line,file = logVars$file,append = TRUE)
  
  line = paste("Filter By Column:", logVars$BMDFMFilterbyCol, sep = " ")
  write(line,file = logVars$file,append = TRUE)
  
  line = paste("Filter By Values:", logVars$BMDFMFilterbyVals, sep = " ")
  write(line,file = logVars$file,append = TRUE)
}

log_build_funmmappone_heatmap  =  function(logVars){
  if (is.null(logVars$enrichmentReason)) {
    return(NULL)
  }
  
  print("writing log file obj")
  line = "-----------------------------------------------------"
  write(line,file = logVars$file,append = TRUE)
  
  line = "BUILD HEATMAP"
  write(line,file = logVars$file,append = TRUE)
  
  line = "PARAMETERS"
  write(line,file = logVars$file,append = TRUE)
  
  line = paste("Timestamp:", logVars$buildFunmmapponeHeatmapTimestamp, sep = " ")
  write(line,file = logVars$file,append = TRUE)
  
  line = paste("Level1:", logVars$buildFunmmapponeHeatmapLev1, sep = " ")
  write(line,file = logVars$file,append = TRUE)
  
  line = paste("Level2:", logVars$buildFunmmapponeHeatmapLev2, sep = " ")
  write(line,file = logVars$file,append = TRUE)
  
  line = paste("Level3:", logVars$buildFunmmapponeHeatmapLev3, sep = " ")
  write(line,file = logVars$file,append = TRUE)
  
  line = paste("Samples ID:", logVars$buildFunmmapponeHeatmapSamplesID, sep = " ")
  write(line,file = logVars$file,append = TRUE)
  
  line = paste("Collapse level:", logVars$buildFunmmapponeHeatmapCollapseLevel, sep = " ")
  write(line,file = logVars$file,append = TRUE)
  
  line = paste("doGrouping:", logVars$buildFunmmapponeHeatmapDoGrouping, sep = " ")
  write(line,file = logVars$file,append = TRUE)

  line = paste("Continous:", logVars$buildFunmmapponeHeatmapCountinous, sep = " ")
  write(line,file = logVars$file,append = TRUE)
}

log_enrichment_params  =  function(logVars){
  if (is.null(logVars$enrichmentReason)) {
    return(NULL)
  }
  
  print("writing log file obj")
  line = "-----------------------------------------------------"
  write(line,file = logVars$file,append = TRUE)
  
  line = "ENRICHMENT"
  write(line,file = logVars$file,append = TRUE)
  
  line = "PARAMETERS"
  write(line,file = logVars$file,append = TRUE)
  
  ts  =  Sys.time()
  line = paste("Timestamp:", logVars$enrichmentTimestamp, sep = " ")
  write(line,file = logVars$file,append = TRUE)
  
  line = paste("Justification:", logVars$enrichmentReason, sep = " ")
  write(line,file = logVars$file,append = TRUE)
  
  line = paste("Organisms:", logVars$enrichmentOrganisms , sep = " ")
  write(line,file = logVars$file,append = TRUE)
  
  line = paste("GeneID:", logVars$enrichmentGeneID, sep = " ")
  write(line,file = logVars$file,append = TRUE)
  
  line = paste("Select Functional Annotation:",  logVars$enrichmentFunctionalAnnotation, sep = " ")
  write(line,file = logVars$file,append = TRUE)
  
  line = paste("Select GO:",  logVars$enrichmentGO, sep = " ")
  write(line,file = logVars$file,append = TRUE)
  
  line = paste("Correction Method:", logVars$enrichmentcorrectionM , sep = " ")
  write(line,file = logVars$file,append = TRUE)
  
  line = paste("P-value threshold:", logVars$enrichmentPvalTh, sep = " ")
  write(line,file = logVars$file,append = TRUE)
  
  line = paste("Significant Results only:", logVars$enrichmentSigRes, sep = " ")
  write(line,file = logVars$file,append = TRUE)
  
  line = paste("Annotated genes only:",  logVars$enrichmentAnnGenes, sep = " ")
  write(line,file = logVars$file,append = TRUE)
  
  line = paste("Minimum number of genes in the intersection:", logVars$enrichmentMinNGenes, sep = " ")
  write(line,file = logVars$file,append = TRUE)
  
  line = paste("Aggregation Function:", logVars$enrichmenAggFunction, sep = " ")
  write(line,file = logVars$file,append = TRUE)
  
  line = paste("Plot modification:", logVars$enrichmentPlotM , sep = " ")
  write(line,file = logVars$file,append = TRUE)
  
  line = paste("Choose Values Type:", logVars$enrichmentValT, sep = " ")
  write(line,file = logVars$file,append = TRUE)
}



log_bmdcompexp_paras  =  function(logVars){
  if (is.null(logVars$bmdcompexpReason)) {
    return(NULL)
  }
  
  print("writing log file obj")
  line = "-----------------------------------------------------"
  write(line,file = logVars$file,append = TRUE)
  
  line = "BMD"
  write(line,file = logVars$file,append = TRUE)
  
  line = "COMPARE EXPERIMENTS PLOTS"
  write(line,file = logVars$file,append = TRUE)
  
  ts  =  Sys.time()
  line = paste("Timestamp:", logVars$BMDCompExpTimestamp, sep = " ")
  write(line,file = logVars$file,append = TRUE)
  
  line = paste("Justification:", logVars$bmdcompexpReason, sep = " ")
  write(line,file = logVars$file,append = TRUE)
  
  line = paste("Rel Var:", logVars$BMDCompExpRelVar , sep = " ")
  write(line,file = logVars$file,append = TRUE)
  
  line = paste("Group By:", logVars$BMDCompExpGroupBy, sep = " ")
  write(line,file = logVars$file,append = TRUE)
  
  line = paste("Other Vars:",  logVars$BMDCompExpOtherVar, sep = " ")
  write(line,file = logVars$file,append = TRUE)
  
  line = paste("Number Clusters:",  logVars$BMDCompExpnCl, sep = " ")
  write(line,file = logVars$file,append = TRUE)
  
  line = paste("Number Bins:", logVars$BMDCompExpnBins , sep = " ")
  write(line,file = logVars$file,append = TRUE)
  
  line = paste("max Intersection:", logVars$BMDCompExpmInt, sep = " ")
  write(line,file = logVars$file,append = TRUE)
  
  line = paste("Group By 2:", logVars$BMDCompExpgb, sep = " ")
  write(line,file = logVars$file,append = TRUE)
  
  line = paste("Order By:", logVars$BMDCompExpob, sep = " ")
  write(line,file = logVars$file,append = TRUE)
  
  
}


log_cluster_gene_pairs  =  function(logVars){
  
  if (is.null(logVars$genepariscomparisonclusteringReasons)){
    return(NULL)
  }
  print("writing log file")
  
  ts  =  Sys.time()
  line = paste("Timestamp:", logVars$genepariscomparisonclusteringTimestamp, sep = " ")
  write(line,file = logVars$file,append = TRUE)
  
  line = paste("Number of clusters:", logVars$genepariscomparisonclusteringNclust, sep = " ")
  write(line,file = logVars$file,append = TRUE)
  
  line = paste("Used similarity/distance for clustering:", logVars$clustering_similarity, sep = " ")
  write(line,file = logVars$file,append = TRUE)
  
  line = paste("Organism", logVars$genepariscomparisonclusteringOrganism, sep = " ")
  write(line,file = logVars$file,append = TRUE)
  
  line = paste("Correction method:", logVars$genepariscomparisonclusteringCorrectionMethod, sep = " ")
  write(line,file = logVars$file,append = TRUE)
}

log_compare_gene_pairs  =  function(logVars){
  if (is.null(logVars$genepariscomparisonReasons)) {
    return(NULL)
  }
  print("writing log file")
  
  ts  =  Sys.time()
  line = paste("Timestamp:", logVars$genePairComparisonTimestamp, sep = " ")
  write(line,file = logVars$file,append = TRUE)
  
  line = paste("Justification for this Step & Parameters:", logVars$genepariscomparisonReasons, sep = " ")
  write(line,file = logVars$file,append = TRUE)
  
  line = paste("Parameters", "", sep = " ")
  write(line,file = logVars$file,append = TRUE)
  
  line = paste("Experiment:", logVars$genePairComparisonExperiment, sep = " ")
  write(line,file = logVars$file,append = TRUE)
  
  line = paste("Time point:", logVars$genePairComparisonTimepoint, sep = " ")
  write(line,file = logVars$file,append = TRUE)
  
  line = paste("Number of cores:", logVars$genePairComparisonCores, sep = " ")
  write(line,file = logVars$file,append = TRUE)
  
  line = paste("Length predicted data:",  logVars$genePairComparisonNewDataLength, sep = " ")
  write(line,file = logVars$file,append = TRUE)
  
}

log_compare_gene_pairs_PPI  =  function(logVars){
  if (is.null(logVars$genepariscomparisonReasonsPPI)) {
    return(NULL)
  }
  
  print("writing log file")
  
  ts  =  Sys.time()
  line = paste("Timestamp:", logVars$genePairComparisonPPITimestamp, sep = " ")
  write(line,file = logVars$file,append = TRUE)
  
  line = paste("Justification for this Step & Parameters:", logVars$genepariscomparisonReasonsPPI, sep = " ")
  write(line,file = logVars$file,append = TRUE)
  
  line = paste("Parameters", "", sep = " ")
  write(line,file = logVars$file,append = TRUE)
  
  line = paste("Organism:", logVars$genePairComparisonPPIorganism, sep = " ")
  write(line,file = logVars$file,append = TRUE)
  
  line = paste("Enrichment correction method (if NULL no enrichment is computed):", logVars$genePairComparisonPPIGostCorrectionMethod, sep = " ")
  write(line,file = logVars$file,append = TRUE)
  
  line = paste("Query enrichment (if NULL no enrichment is computed):", logVars$genePairComparisonPPIGostQuery, sep = " ")
  write(line,file = logVars$file,append = TRUE)
  
}

log_enrich_aop = function(logVars) {
  if (is.null(logVars$enrichAOPTimestamp)) {
    return(NULL)
  }
  
  print("writing log file")
  
  # Write the log file contents
  ts = Sys.time()
  line = paste("Timestamp:", logVars$enrichAOPTimestamp, sep = " ")
  write(line, file = logVars$file, append = TRUE)
  
  line = paste("Justification for this Step & Parameters:", logVars$enreachAOPReasons, sep = " ")
  write(line,file = logVars$file,append = TRUE)
  
  line = paste("Key Events (KE) Significant P-Value Threshold:", logVars$ke_sig_pval, sep = " ")
  write(line, file = logVars$file, append = TRUE)
  
  line = paste("Key Events (KE) Correction Method:", logVars$ke_correction_method, sep = " ")
  write(line, file = logVars$file, append = TRUE)
  
  line = paste("Key Events (KE) POD aggregation function:", logVars$aggregation_function, sep = " ")
  write(line, file = logVars$file, append = TRUE)
  
  line = paste("Key Events (KE) Only Significant Results:", logVars$ke_only_significant, sep = " ")
  write(line, file = logVars$file, append = TRUE)
  
  line = paste("Adverse Outcome Pathway (AOP) Significant P-Value Threshold:", logVars$aop_sig_pval, sep = " ")
  write(line, file = logVars$file, append = TRUE)
  
  line = paste("Adverse Outcome Pathway (AOP) Correction Method:", logVars$aop_correction_method, sep = " ")
  write(line, file = logVars$file, append = TRUE)
  
  line = paste("User provided background:", logVars$user_provided_background, sep = " ")
  write(line, file = logVars$file, append = TRUE)
  
  line = paste("Background file uploaded from the user:", logVars$user_provided_background_file, sep = " ")
  write(line, file = logVars$file, append = TRUE) 
  
  line = paste("Adverse Outcome Pathway (AOP) Only Significant Results:", logVars$aop_only_significant, sep = " ")
  write(line, file = logVars$file, append = TRUE)
  
  line = paste("Minimum AOP Length for Fingerprints:", logVars$min_aop_length_fingerprint, sep = " ")
  write(line, file = logVars$file, append = TRUE)
  
  line = paste("Percentage Enriched Key Events:", logVars$percentage_enriched_ke, sep = " ")
  write(line, file = logVars$file, append = TRUE)
  
}

log_create_ker_network = function(logVars) {
  if (is.null(logVars$Create_KEr_networkTimestamp)) {
    return(NULL)
  }
  
  print("writing log file")
  
  # Write the log file contents
  ts = Sys.time()
  line = paste("Timestamp:", logVars$Create_KEr_networkTimestamp, sep = " ")
  write(line, file = logVars$file, append = TRUE)
  
  line = paste("Justification for this Step & Parameters:", logVars$KEKENetworkReason, sep = " ")
  write(line,file = logVars$file,append = TRUE)
  
  line = paste("Filter Key Events (KEs) by AOP Fingerprint:", logVars$Create_KEr_network_KEr_filter_ke_by_aop_fingerprint, sep = " ")
  write(line, file = logVars$file, append = TRUE)
  
  line = paste("Enlarge Key Event (KE) Selection:", logVars$Create_KEr_network_KEr_KEr_enlarge_ke_selection, sep = " ")
  write(line, file = logVars$file, append = TRUE)
  
  line = paste("Group KE by SSbD categories:", logVars$Create_KEr_network_KEr_KEr_group_by_ssbd, sep = " ")
  write(line, file = logVars$file, append = TRUE)
  
}
