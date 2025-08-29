skip_filtering = function(gVars, logVars, input){
  
  gVars$EXP_FIL = gVars$inputGx # TODO: remove this and replace with gVars$filtered_bmd
  gVars$PValMat = NULL
  gVars$unfilteredPval = NULL #avoids plotting
  gVars$selectedPvalTh = NULL
  
  gVars$filtered_bmd = gVars$inputGx
  gVars$was_anova_performed = FALSE
  gVars$was_trend_test_performed = FALSE
  gVars$was_logFC_filtering_performed = FALSE
  
  if (gVars$in_glp_mode) {
    
    #GLP MODE
    shinyalert(
      title = "GLP Mode Justification",
      text = "Please provide a reason for choosing Skip Filtering.",
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
      callbackR = function(x) {logVars$skipFilterReason = x}
    )
    
    #save for report
    ts = Sys.time()
    logVars$skipFilterTimestamp = ts
  }
  gVars$was_filtered_skipped = TRUE
  return(list(gVars, logVars))
}


run_fc_analysis = function(gVars, logVars, input){
  time_col_id = gVars$TPColID
  dose_col_id = gVars$doseColID
  sample_col_id = gVars$sampleColID
  
  colnames_pheno = colnames(gVars$inputPh()[[1]])
  fc_res = NULL
  
   tryCatch({
     fc_res = bmdx::perform_differential_expression_analysis_filtering(data_dictionary = gVars$inputGx, 
                                                                      experimental_data = gVars$original_experimental_data,
                                                                      phTable = gVars$inputPh(),
                                                                      time_point_variable = colnames_pheno[time_col_id],
                                                                      dose_variable = colnames_pheno[dose_col_id],
                                                                      samples_variable = colnames_pheno[sample_col_id],
                                                                      fcAdjustment = input$fcPvalAdj,
                                                                      fcPval.th = as.numeric(input$fcPvalTh), 
                                                                      fc.th = as.numeric(input$fcTh), 
                                                                      nCores = as.numeric(input$fcNCores),
                                                                      x = "dose",
                                                                      y = "expr",
                                                                      other_variables_id_col = gVars$optVarsGroup)

  },
  error = function(e) {
    message('Error: ', e)
    showModal(
      modalDialog(
        div(paste("The following error occured. ",e,"\\n Please check that when you import the dose-repated data, they are numerical!", sep = ""))
        ,,easyClose = TRUE)
    )
    gVars$was_logFC_filtering_performed  =  F
    return(list(gVars, logVars))
  })
  
  if(!is.null(fc_res)){
    gVars$EXP_FIL = fc_res$filtered_data_dictionary # TODO: remove this and replace with gVars$filtered_bmd
    gVars$PValMat = fc_res$fc_res_dataframe 
    gVars$unfilteredPval = fc_res$fc_res_unfiltered_dataframe
    gVars$selectedPvalTh = as.numeric(input$anovaPvalTh)
    gVars$filtered_bmd = fc_res$filtered_data_dictionary
    logVars$fcFilterData = fc_res$filtered_data_dictionary
    gVars$was_logFC_filtering_performed = TRUE
    gVars$final_filtering_strategy = "Differential analysis"
    
    if (gVars$in_glp_mode) {
      
      #GLP MODE
      shinyalert(
        title = "GLP Mode Justification",
        text = "Please provide a justification for selecting Foldchange Filtering, 
      the Pvalue adjustment method and the Pvalue Threshold.",
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
      callbackR = function(x) {logVars$fcFilterReason = x}
      )
      
      #save for report
      ts = Sys.time()
      logVars$fcFilterTimestamp = ts
      logVars$fcFilterPvalAdj = input$fcPvalAdj
      logVars$fcFilterPvalTh = input$fcPvalTh
      logVars$fcFilteredFCth = input$fcTh
      logVars$fcFilterCores = input$fcNCores
      
    }
    
    if (gVars$in_glp_mode) {
      print("writing log file anova obj")
      line = "-----------------------------------------------------"
      write(line,file = logVars$file,append = TRUE)
      
      line = "GENE FILTERING"
      write(line,file = logVars$file,append = TRUE)
      
      line = "FOLD-CHANGE FILTERING"
      write(line,file = logVars$file,append = TRUE)
      
      ts  =  Sys.time()
      tsn  =  as.numeric(ts)
      # fname  =  paste(logVars$location, "/tables/", toString(tsn), "_FC_Filtered.RData", sep = "")
      # save(fc_res, file  =  fname)
      # line = paste("The Current R Data Object is saved:", fname, sep = " ")
      # write(line,file = logVars$file,append = TRUE)
      # line = paste("You can access the filtered data table with $filtered_data_dictionary:", "", sep = " ")
      # write(line,file = logVars$file,append = TRUE)
    }
    gVars$was_logFC_filtering_performed  =  TRUE
    
  }
 
  return(list(gVars, logVars))
}

run_anova_analysis = function(gVars, logVars, input){
  
  print(input$anovaPvalAdj)
  anova_res = bmdx::perform_anova(gVars$inputGx, anovaPval.th = as.numeric(input$anovaPvalTh), 
                            anovaAdjustment = input$anovaPvalAdj, 
                            anovaCores = as.numeric(input$anovaNCores), 
                            x = input$x_ID, y = input$y_ID,  
                            other_variables_id_col = gVars$optVarsGroup)
  
  print(head(anova_res$anova_res_dataframe))
  
  gVars$EXP_FIL = anova_res$filtered_data_dictionary # TODO: remove this and replace with gVars$filtered_bmd
  gVars$PValMat = anova_res$anova_res_dataframe 
  gVars$unfilteredPval = anova_res$anova_res_unfiltered_dataframe
  gVars$anova_plot_list = anova_res$anova_plot_list
  gVars$selectedPvalTh = input$anovaPvalTh #this is used in the pie chart plotting
  #TODO check if correct object
  gVars$filtered_bmd = anova_res$filtered_data_dictionary
  
  gVars$was_anova_performed = TRUE
  gVars$final_filtering_strategy = "ANOVA"
  
  logVars$anovaFilterData = anova_res$filtered_data_dictionary
  if (gVars$in_glp_mode) {
    
    #GLP MODE
    shinyalert(
      title = "GLP Mode Justification",
      text = "Please provide a justification for selecting Anova Filtering, the Pvalue adjustment method and the Pvalue Threshold.",
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
      callbackR = function(x) {logVars$anovaFilterReason = x}
      
    )
    
    #save for report
    ts = Sys.time()
    logVars$anovaFilterTimestamp = ts
    logVars$anovaFilterPvalAdj = input$anovaPvalAdj
    logVars$anovaFilterPvalTh = input$anovaPvalTh
    logVars$anovaFilterCores = input$anovaNCores
    
  }
  
  if (gVars$in_glp_mode) {
    print("writing log file anova obj")
    line = "-----------------------------------------------------"
    write(line,file = logVars$file,append = TRUE)
    
    line = "GENE FILTERING"
    write(line,file = logVars$file,append = TRUE)
    
    line = "ANOVA FILTERING"
    write(line,file = logVars$file,append = TRUE)
    
    ts  =  Sys.time()
    tsn  =  as.numeric(ts)
    fname  =  paste(logVars$location, "/tables/", toString(tsn), "_anovaFiltered.RData", sep = "")
    save(anova_res, file  =  fname)
    line = paste("The Current R Data Object is saved:", fname, sep = " ")
    write(line,file = logVars$file,append = TRUE)
    line = paste("You can access the filtered data table with $filtered_data_dictionary:", "", sep = " ")
    write(line,file = logVars$file,append = TRUE)
  }
  gVars$was_anova_performed = TRUE
  return(list(gVars, logVars))
  
}

run_trend_filtering = function(gVars, logVars, input){
  trend_res = bmdx::perform_trend_test(gVars$inputGx, 
                                       trendPval.th = as.numeric(input$trendPvalTh), 
                                       trendAdjustment = input$TrendPvalAdj, 
                                       trendCores = input$trendNCores,  
                                       x = input$x_ID, 
                                       y = input$y_ID,  
                                       other_variables_id_col = gVars$optVarsGroup)
  
  gVars$EXP_FIL = trend_res$filtered_data_dictionary #TODO: remove and substitute with gVars$filtered_bmd
  gVars$PValMat = trend_res$trend_res_dataframe 
  gVars$unfilteredPval = trend_res$trend_res_unfiltered_dataframe
  gVars$selectedPvalTh = input$trendPvalTh #this is used in the pie chart plotting
  
  #TODO check if correct object
  gVars$filtered_bmd = trend_res$filtered_data_dictionary
  gVars$was_trend_test_performed = TRUE
  gVars$final_filtering_strategy = "Trend test"
  
  if (gVars$in_glp_mode) {
    
    #GLP MODE
    shinyalert(
      title = "GLP Mode Justification",
      text = "Please provide a justification for selecting Trend Filtering, the Pvalue adjustment method and the Pvalue Threshold.",
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
      callbackR = function(x) {logVars$trendFilterReason = x}
      
    )
    
    #save for report
    ts = Sys.time()
    logVars$trendFilterTimestamp = ts
    logVars$trendFilterPvalAdj = input$TrendPvalAdj
    logVars$trendFilterPvalTh = input$trendPvalTh
    logVars$trendFilterCores = input$trendNCores
    
  }
  
  if (gVars$in_glp_mode) {
    print("writing log file trend obj")
    line = "-----------------------------------------------------"
    write(line,file = logVars$file,append = TRUE)
    
    line = "GENE FILTERING"
    write(line,file = logVars$file,append = TRUE)
    
    line = "TREND FILTERING"
    write(line,file = logVars$file,append = TRUE)
    
    ts  =  Sys.time()
    tsn  =  as.numeric(ts)
    fname  =  paste(logVars$location, "/tables/", toString(tsn), "_trendFiltered.RData", sep = "")
    save(trend_res, file  =  fname)
    line = paste("The Current R Data Object is saved:", fname, sep = " ")
    write(line,file = logVars$file,append = TRUE)
    line = paste("You can access the filtered data table with $filtered_data_dictionary:", "", sep = " ")
    write(line,file = logVars$file,append = TRUE)
  }
  gVars$was_trend_test_performed = TRUE
  return(list(gVars, logVars))
}

render_time_selection_anova_interface = function(gVars, input){
  if (is.null(gVars$filtered_bmd) || is.null(input$experiment_anova)) {
    return(NULL)
  }
  
  expList = gVars$filtered_bmd[[input$experiment_anova]]
  times = unique(unlist(lapply(strsplit(x = names(gVars$filtered_bmd), split = "_"),FUN = function(elem)elem[[2]])))
  selectInput("time_point_id_visual", "Time Points", choices = c("All",times), selected = "All")
}

render_experiment_selection_anova_interface = function(gVars){
  if (is.null(gVars$filtered_bmd)) {
    return(NULL)
  }
  experiment_names = unique(unlist(lapply(strsplit(x = names(gVars$filtered_bmd), split = "_"),FUN = function(elem)elem[[1]])))
  selectInput("experiment_anova", "Experiment", choices = c("All",experiment_names), selected = "All")
}


compute_anova_tab = function(gVars, input){
 
  if (is.null(gVars$PValMat))
    return(NULL)
  if (is.null(input$time_point_id_visual))
    return(NULL)
  if (is.null(input$experiment_anova))
    return(NULL)
  if (is.null(gVars$unfilteredPval))
    return(NULL) #wont plot if skip was last performed action
  if (input$experiment_anova != "All" & input$time_point_id_visual != "All") {
    Anova_tab <- gVars$PValMat[gVars$PValMat$Exp == input$experiment_anova & gVars$PValMat$Time == input$time_point_id_visual,]
  } else if (input$experiment_anova == "All" & input$time_point_id_visual != "All") {
    Anova_tab <- gVars$PValMat[gVars$PValMat$Time == input$time_point_id_visual,]
  } else if (input$experiment_anova != "All" & input$time_point_id_visual == "All") {
    Anova_tab <- gVars$PValMat[gVars$PValMat$Exp == input$experiment_anova ,]
  } else {
    Anova_tab <- gVars$PValMat
  }
  
  if (nrow(Anova_tab) == 0) {
    print("data not available")
    Anova_tab = data.frame(matrix(ncol = ncol(gVars$PValMat), nrow = 0))
    colnames(Anova_tab) = colnames(gVars$PValMat)
    drop <- c("usedFilteringPval")
    Anova_tab = Anova_tab[,!(names(Anova_tab) %in% drop)]
    print(Anova_tab)
  }
  gVars$Anova_tab = Anova_tab
  drop <- c("usedFilteringPval")
  Anova_tab = Anova_tab[,!(names(Anova_tab) %in% drop)]

  return(gVars)
}

render_anova_tab_DT_datatable = function(gVars){
  DT::datatable(gVars$Anova_tab, filter = "top",
                options = list(
                  search = list(regex = TRUE, caseInsensitive = FALSE), 
                  scrollX = TRUE,
                  ordering = T,
                  rowCallback = JS('
                        function(nRow, aData, iDisplayIndex, iDisplayIndexFull) {
                        // Bold and green cells for conditions
                        if (parseFloat(aData[2]) <=0.05)
                        $("td:eq(3)", nRow).css("font-weight", "bold");
                        }')
                )
  )
}


pie_chart_anova = function(gVars, input){
  if (is.null(gVars$unfilteredPval))
    return(NULL)
  
  if (is.null(input$experiment_anova)) return(NULL)
  if (is.null(input$time_point_id_visual)) return(NULL)
  if (is.null(gVars$Anova_tab)) return(NULL)
  if (is.null(gVars$final_filtering_strategy)) return(NULL)
  
  if (length(gVars$unfilteredPval) == 0)
    return(NULL)
  
  if (input$experiment_anova != "All") {
    exp_selection = gVars$unfilteredPval$Exp == input$experiment_anova
  }else{
    exp_selection = rep(TRUE, nrow(gVars$unfilteredPval))
  }
  
  if (input$time_point_id_visual != "All") {
    time_selection = gVars$unfilteredPval$Time == input$time_point_id_visual
  } else{
    time_selection = rep(TRUE, nrow(gVars$unfilteredPval))
  }
  
  Anova_tab <- gVars$unfilteredPval[exp_selection & time_selection,]

  if (nrow(Anova_tab) != 0) {
      
    p = bmdx::plot_filtering_pie_chart(Anova_tab,gVars$selectedPvalTh, title =gVars$final_filtering_strategy)
    
  }else{
    p = make_empty_graph()
  }
  return(p)
}
