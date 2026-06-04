#TODO: current selection is done by numbers, we need to update it with the list of available modesl
# represented as strings #DONE!
render_model_selection_for_bmd_analysis = function(gVars, input){
  shiny::validate(need(is.null(gVars$BMDSettings), "No BMD Settings!"))
  
  if (input$BMDSettings == "All") {
    selected = c("linear","hill","power",
                 "poly2","poly3","poly4","poly5",
                 "exp2","exp3","exp4","exp5",
                 "llog2","llog3","llog4","llog5",
                 "mm2","weibul12","weibul13","weibul14",
                 "weibul22","weibul23","weibul24")
  }
  if (input$BMDSettings == "Regulatory") {
    selected = c("linear","hill","power",
      "poly2","poly3","poly4","poly5",
      "exp2","exp3","exp4","exp5")
  }
  if (input$BMDSettings == "Custom") {
    selected = c("linear","hill","power",
                 "poly2",
                 "exp2")
  }
  if (input$BMDSettings == "Degree of Freedom") {
    print("degree of freedom")
    if (is.null(gVars$phTable)) {
      nDose = 0
      selected = c()
    }else{
      nDoses = c()
      for (i in 1:length(gVars$phTable)) {
        nDoses = c(nDoses,length(unique(gVars$phTable[[i]][,gVars$doseColID])))
      }
      
      if (length(nDoses) > 0) {
        nDose = min(nDoses) - 1
        DF =  nDose - 1
        
        selected = c("linear","hill","power",
                     "poly2","poly3","poly4","poly5",
                     "exp2","exp3","exp4","exp5",
                     "llog2","llog3","llog4","llog5",
                     "mm2","weibul12","weibul13","weibul14",
                     "weibul22","weibul23","weibul24")
        
        modDF = c(1,1,1, 
                  2,3,4,5,
                  2,3,4,5,
                  2,3,4,5,
                  2,
                  2,3,4,
                  2,3,4)
        selected = selected[which(modDF <= DF)]
      }else{
        selected = c()
      }
    }
    
  }
  
  tags$div(align = 'left',class = 'multicol', 
    style = "height: auto; padding: 5px;",
           checkboxGroupInput("ModGroup", 
                              label = "Models available", 
                              choices = list(
                                  "Linear" = "linear",
                                  "Polynomial 2" = "poly2",
                                  "Polynomial 3" = "poly3",
                                  "Polynomial 4" = "poly4",
                                  "Polynomial 5" = "poly5",
                                  "Hill" = "hill",
                                  "Power" = "power",
                                  "Exponential 2" = "exp2",
                                  "Exponential 3" = "exp3",
                                  "Exponential 4" = "exp4",
                                  "Exponential 5" = "exp5",
                                  "Log-logistic 2" = "llog2",
                                  "Log-logistic 3" = "llog3",
                                  "Log-logistic 4" = "llog4",
                                  "Log-logistic 5" = "llog5",
                                  "Michaelis-Menten 2" = "mm2",
                                  "Weibull 1.2" = "weibul12",
                                  "Weibull 1.3" = "weibul13",
                                  "Weibull 1.4" = "weibul14",
                                  "Weibull 2.2" = "weibul22",
                                  "Weibull 2.3" = "weibul23",
                                  "Weibull 2.4" = "weibul24"),
                              selected = selected) 
  )
}

run_bmd_analysis = function(gVars, logVars, input){
  model_names = input$ModGroup
  print(model_names)
  x = input$x_ID
  y = input$y_ID
  
  model_list = bmdx::build_models(model_names = model_names, 
                            max_iter = input$BMDMaxIter, 
                            data_type = input$BMDdataType, 
                            x = x, 
                            y = y)

  #in point_of_departure.R
  response_level = bmr_factor(as.numeric(input$RespLev))
  
  print("fittin list")

  all_fitted_models = bmdx::fitting_list(data_dictionary = gVars$filtered_bmd, 
                                   model_list,
                                   deviation_type = input$BMDdeviationType,
                                   rl = response_level,
                                   confidence_interval =  as.numeric(input$conf_interval),
                                   variance_type = input$variance_type,
                                   significance_level = as.numeric(input$BMDsignificance),
                                   nCores = as.numeric(input$BMDNCores),is_parallel = as.numeric(input$BMDNCores)>1)
  
  other_variables_id_col = colnames(gVars$inputPh()[[1]])[as.numeric(gVars$TPColID)]
  
  BMD_tab = bmdx::compute_model_statistics(all_fitted_models,
                                          other_variables_id_col = c(other_variables_id_col, gVars$optVarsGroup),
                                          nCores = input$BMDNCores,is_parallel = as.numeric(input$BMDNCores)>1)
  
  # Save inputs into gVars 
  gVars$computeBMDCores = input$BMDNCores
  gVars$computeBMDmaxIter = input$BMDMaxIter
  gVars$variance_type = input$variance_type
  gVars$computeBMDLOOF = input$LOOF
  gVars$computeBMDRiskFactor = response_level
  gVars$computeBMDSigTh = input$BMDsignificance
  gVars$computeBMDConfInt = input$conf_interval
  gVars$computeBMDAnSetting = input$BMDSettings
  gVars$computeBMDdataType = input$BMDdataType
  gVars$computeBMDdevType = input$BMDdeviationType
  gVars$computeBMDmodels = model_names
  
  #TODO figure out where to go or if
  gVars$MQ_BMD_filtered  = NULL
  gVars$bmdlatestTable = BMD_tab #latest to be used
  gVars$MQ_BMD = all_fitted_models #MQ_BMDList original one
  gVars$BMDMQ_latest = all_fitted_models #latest to be used
  gVars$all_models_statistics = BMD_tab #original table
  
  gVars$MQ_BMDListFilteredValues = NULL 
  gVars$MQ_BMD_filtered2 = NULL
  
  if (gVars$in_glp_mode) {
    
    #GLP MODE
    shinyalert(
      title = "GLP Mode Justification",
      text = "Please provide a justification for all your selected parameters, especially P-Values, risk factor and confidence Interval, as well as the models selected.",
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
      callbackR = function(x) {logVars$computeBMDReason = x}
      
    )
    
    #save for report
    ts = Sys.time()
    logVars$computeBMDTimestamp = ts
    logVars$computeBMDCores = input$BMDNCores
    logVars$computeBMDmaxIter = input$BMDMaxIter
    logVars$variance_type = input$variance_type
    logVars$computeBMDLOOF = input$LOOF
    logVars$computeBMDRiskFactor = response_level
    logVars$computeBMDSigTh = input$BMDsignificance
    logVars$computeBMDConfInt = input$conf_interval
    logVars$computeBMDAnSetting = input$BMDSettings
    logVars$computeBMDdataType = input$BMDdataType
    logVars$computeBMDdevType = input$BMDdeviationType
    logVars$computeBMDmodels = model_names
  }
  
  
  if (gVars$in_glp_mode) {
    print("writing log file obj")
    line = "-----------------------------------------------------"
    write(line,file = logVars$file,append = TRUE)
    
    line = "COMPUTE BMD"
    write(line, file = logVars$file,append = TRUE)
    
    line = "COMPUTE BMD VALUE"
    write(line, file = logVars$file,append = TRUE)
    
    ts = Sys.time()
    tsn = as.numeric(ts)
    # fname = paste(logVars$location, "/tables/", toString(tsn), "_BMD.RData", sep = "")
    # save(BMD_tab, file = fname)
    # line = paste("The Current R Data Object is saved:", fname, sep = " ")
    # write(line,file = logVars$file,append = TRUE)
    
  }
  
  return(list(gVars, logVars))
  
}

render_main_bmd_table_results = function(gVars, input){
  if (is.null(gVars$BMDMQ_latest) || is.null(input$experiment_bmd) || is.null(input$time_point_id_visual2)) { 
    print("Null BMD")
    return(NULL)
  }
  
  if (input$experiment_bmd != "All" & input$time_point_id_visual2 != "All") {
    
    BMD_tab <- gVars$bmdlatestTable[gVars$bmdlatestTable$Experiment == input$experiment_bmd & gVars$bmdlatestTable[colnames(gVars$inputPh()[[1]])[as.numeric(gVars$TPColID)]] == input$time_point_id_visual2,]
    
  } else if (input$experiment_bmd == "All" & input$time_point_id_visual2 != "All") {
    BMD_tab <- gVars$bmdlatestTable[gVars$bmdlatestTable[colnames(gVars$inputPh()[[1]])[as.numeric(gVars$TPColID)]] == input$time_point_id_visual2,]
    
  } else if (input$experiment_bmd != "All" & input$time_point_id_visual2 == "All") {
    BMD_tab <- gVars$bmdlatestTable[gVars$bmdlatestTable$Experiment == input$experiment_bmd ,]
    
  } else {
    BMD_tab <- gVars$bmdlatestTable
  }
  gVars$BMD_tab = BMD_tab
  return(gVars)
  
}



filter_bmd = function(gVars, logVars, input){
  filtered_models = bmdx::model_filtering(fitted_models = gVars$BMDMQ_latest,
                                         loofth = as.numeric(input$bmdloofth),
                                         lower_bound_th = as.numeric(input$min_dose_perc2),
                                         upper_bound_th = as.numeric(input$max_dose_perc2),
                                         bmd_bmdl_th = as.numeric(input$bmd_bmdl_th2),
                                         bmdu_bmd_th = as.numeric(input$bmdu_bmd_th2),
                                         bmdu_bmdl_th = as.numeric(input$bmdu_bmdl_th2),
                                         filter_lower_bound = input$filter_bounds_bmdl_bmdu2_low,
                                         filter_upper_bound = input$filter_bounds_bmdl_bmdu2_high,
                                         filter_by_lack_of_fit = input$filter_bmdl_loof,
                                         ratio_filter = input$ratio_filter2,
                                         bmd_na_filter = input$filter_bmd_na,
                                         bmdl_na_filter = input$filter_bmdl_na,
                                         bmdu_na_filter = input$filter_bmdu_na,
                                         ic50_na_filter = input$filter_ic50_na,
                                         r2_filter = input$filter_by_R2,
                                         r2_th = as.numeric(input$BMD_filter_r2),
                                         filter_by_monotonicity = input$filter_by_monotonicity,
                                         filter_by_negative_values = input$filter_by_negative_values,
                                         filter_by_unordered_values = input$filter_by_unordered_values)
  
  other_variables_id_col = colnames(gVars$inputPh()[[1]])[as.numeric(gVars$TPColID)]
  filtered_models_statistics = bmdx::compute_model_statistics(filtered_models,other_variables_id_col = c(other_variables_id_col, gVars$optVarsGroup),is_parallel = TRUE,nCores = input$BMDN_filter_Cores)
  
  gVars$MQ_BMD_filtered = filtered_models 
  gVars$filtered_models_statistics = filtered_models_statistics
  
  gVars$BMDMQ_latest = filtered_models 
  gVars$bmdlatestTable = filtered_models_statistics #always to be used
  
  gVars$is_bmd_filtering_performed = TRUE
  
  if (gVars$in_glp_mode) {
    
    #GLP MODE
    shinyalert(
      title = "GLP Mode Justification",
      text = "Please provide a justification for filtering and the selected parameters.",
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
      callbackR = function(x) {logVars$BMDFilterJustification = x}
      
    )
    
    #save for report
    ts = Sys.time()
    logVars$BMDFilterTimestamp = ts
    logVars$BMDFilterBMDBMDL = input$bmd_bmdl_th2
    logVars$BMDFilterBMDUBMD = input$bmdu_bmd_th2
    logVars$BMDFilterBMDUBMDL = input$bmdu_bmdl_th2
    logVars$BMDFilterLOOFth = input$bmdloofth
    logVars$BMDFilterlowDF = input$min_dose_perc2
    logVars$BMDFilterhighDF = input$max_dose_perc2
    logVars$BMDFilterratioF = input$ratio_filter2
    logVars$BMDFilterlowBF = input$filter_bounds_bmdl_bmdu2_low
    logVars$BMDFilterhighBF = input$filter_bounds_bmdl_bmdu2_high
    logVars$BMDFilterLOOFF = input$filter_bmdl_loof
    logVars$BMDFilterNABMD = input$filter_bmd_na
    logVars$BMDFilterNABMDL = input$filter_bmdl_na
    logVars$BMDFilterNABMDU = input$filter_bmdu_na
    logVars$BMDFilterNAIC50 = input$filter_ic50_na
    logVars$BMDFilterNcores = input$BMDN_filter_Cores
    
  }
  
  if (gVars$in_glp_mode) {
    print("writing log file obj")
    line = "-----------------------------------------------------"
    write(line,file = logVars$file,append = TRUE)
    
    line  =  "COMPUTE BMD"
    write(line,file = logVars$file,append = TRUE)
    
    line = "FILTERING"
    write(line,file = logVars$file,append = TRUE)
    
    ts  =  Sys.time()
    tsn  =  as.numeric(ts)
    # fname  =  paste(logVars$location, "/tables/", toString(tsn), "_BMDFiltered.RData", sep = "")
    # save(filtered_models_statistics, file  =  fname)
    # line = paste("The Current R Data Object is saved:", fname, sep = " ")
    # write(line,file = logVars$file,append = TRUE)
    # 
  }
  
  return(list(gVars, logVars))
}

select_optimal_model = function(gVars, logVars, input){
  method = input$select_best_model_options
  current_models = gVars$BMDMQ_latest #always to be used

  res = bmdx::select_optimal_models(current_models, 
                                    method = method, 
                                    time_col_id = colnames(gVars$inputPh()[[1]])[as.numeric(gVars$TPColID)], 
                                    optional_col_ids =  gVars$optVarsGroup, 
                                    nCores = input$BMDNCores)
  optimal_models = res$optimal_models  
  BMD_tab_optimal = res$BMD_tab_optimal  
  
  gVars$BMDMQ_latest = optimal_models
  gVars$bmdlatestTable = BMD_tab_optimal
  gVars$is_optimal_model_selected = TRUE
  
  if (gVars$in_glp_mode) {
    
    #GLP MODE
    shinyalert(
      title = "GLP Mode Justification",
      text = "Please provide a justification to why the select of the optimal model is performed and a justification of the selected parameters.",
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
      callbackR = function(x) {logVars$OptModSelJustification = x}
    )
    
    #save for report
    ts = Sys.time()
    logVars$OptModSelFilterTimestamp = ts
    logVars$OptModSelMethod = input$select_best_model_options
    
  }
  
  if (gVars$in_glp_mode) {
    print("writing log file obj")
    line = "-----------------------------------------------------"
    write(line,file = logVars$file,append = TRUE)
    
    line = "COMPUTE BMD"
    write(line,file = logVars$file,append = TRUE)
    
    line = "OPTIMAL MODEL SELECTION"
    write(line,file = logVars$file,append = TRUE)
    
    ts  =  Sys.time()
    tsn  =  as.numeric(ts)
    # fname  =  paste(logVars$location, "/tables/", toString(tsn), "_OptModSel_stats.RData", sep = "")
    # save(BMD_tab_optimal, file  =  fname)
    # line = paste("The Current R Data Object is saved:", fname, sep = " ")
    # write(line,file = logVars$file,append = TRUE)
    # 
  }
  
  return(list(gVars, logVars))
}

compute_model_average = function(gVars, logVars, input){
  models_with_avg = bmdx::add_average_models(gVars$BMDMQ_latest)
  
  other_variables_id_col = colnames(gVars$inputPh()[[1]])[as.numeric(gVars$TPColID)]
  model_statistics_with_avg = bmdx::compute_model_statistics(models_with_avg,nCores = input$ModelAverageNCores,is_parallel = as.numeric(input$ModelAverageNCores)>1,
                                                             other_variables_id_col = c(other_variables_id_col, gVars$optVarsGroup))
  
  gVars$BMDMQ_latest = models_with_avg #always to be used
  gVars$bmdlatestTable = model_statistics_with_avg #always to be used
  gVars$is_average_computed = TRUE
  
  if (gVars$in_glp_mode) {
    
    #GLP MODE
    shinyalert(
      title = "GLP Mode Justification",
      text = "Please provide a justification for computing the Average.",
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
      callbackR = function(x) {logVars$BMDAverageJustification = x}
    )
    
    #save for report
    ts = Sys.time()
    logVars$BMDAverageTimestamp = ts
  }
  
  if (gVars$in_glp_mode) {
    print("writing log file trend obj")
    line = "-----------------------------------------------------"
    write(line,file = logVars$file,append = TRUE)
    
    line = "COMPUTE BMD"
    write(line,file = logVars$file,append = TRUE)
    
    line = "BMD AVERAGE"
    write(line,file = logVars$file,append = TRUE)
    
    ts  =  Sys.time()
    tsn  =  as.numeric(ts)
    fname  =  paste(logVars$location, "/tables/", toString(tsn), "_BMDAverage.RData", sep = "")
    save(model_statistics_with_avg, file  =  fname)
    line = paste("The Current R Data Object is saved:", fname, sep = " ")
    write(line,file = logVars$file,append = TRUE)
    
  }
  return(list(gVars, logVars))
}

reset_model_filter_average = function(gVars, logVars, input){
  print("model reset")
  gVars$BMDMQ_latest = gVars$MQ_BMD #always to be used
  gVars$bmdlatestTable =  gVars$all_models_statistics #always to be used
  BMD_tab = gVars$all_models_statistics #cannot save sub-variables
  gVars$is_average_computed = FALSE
  gVars$is_bmd_filtering_performed = FALSE
  gVars$is_optimal_model_selected = FALSE
  
  if (gVars$in_glp_mode) {
    
    #GLP MODE
    shinyalert(
      title = "GLP Mode Justification",
      text = "Please provide a justification for Reseting the Filtering & Average Step.",
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
      callbackR = function(x) {logVars$BMDResetJustification = x}
    )
    #save for report
    ts = Sys.time()
    logVars$BMDResetTimestamp = ts
  }
  
  if (gVars$in_glp_mode) {
    print("writing log file trend obj")
    line = "-----------------------------------------------------"
    write(line,file = logVars$file,append = TRUE)
    
    line = "COMPUTE BMD"
    write(line,file = logVars$file,append = TRUE)
    
    line = "BMD RESET"
    write(line,file = logVars$file,append = TRUE)
    
    ts  =  Sys.time()
    tsn  =  as.numeric(ts)
    fname  =  paste(logVars$location, "/tables/", toString(tsn), "_BMDReset.RData", sep = "")
    save(BMD_tab, file  =  fname)
    line = paste("The Current R Data Object is saved:", fname, sep = " ")
    write(line,file = logVars$file,append = TRUE)
  }

  return(list(gVars,logVars))
}

bmd_print_model_formula = function(gVars, logVars, input){
  if (length(input$BMD_table_rows_selected) == 0) {
    return(NULL)
  }
  
  selectedrowindex = input$BMD_table_rows_selected[length(input$BMD_table_rows_selected)]
  selectedrowindex = as.numeric(selectedrowindex)
  
  other_variables_id_col = colnames(gVars$inputPh()[[1]])[as.numeric(gVars$TPColID)]
  optVarsGroup = gVars$optVarsGroup
  
  # last updated table
  BMDVF = gVars$bmdlatestTable
  #select model indices
  experiment = BMDVF[selectedrowindex,"Experiment"]
  if(is.null(experiment)){ 
    return(NULL)
    print("null experiment in bmd_model_plot ")
  }
  others = BMDVF[selectedrowindex,c(other_variables_id_col, optVarsGroup)]
  feature = BMDVF[selectedrowindex,"Feature"]
  model = as.character(BMDVF[selectedrowindex,"Model"])
  if (model == "model_average") model = "average"
  to_plot_model = gVars$BMDMQ_latest[[paste(experiment, paste(others, collapse = "_"), feature,sep = "_")]][[model]]
  
  if (model %in% c("linear", "poly2","poly3","poly4","poly5","exp2","exp3","exp4","exp5")) {
    model_parameters = paste(names(to_plot_model$fitted$coefficients), to_plot_model$fitted$coefficients, collapse = "\n")
  }else{
    model_parameters = paste(names(to_plot_model$params), to_plot_model$params, collapse = "\n")
  }
  
  mod_formula = paste("Model Formula: ", to_plot_model$formula,model_parameters,sep = "\n")
  gVars$current_mod_formula = mod_formula
 
  return(list(gVars, logVars))
}

bmd_model_plot = function(gVars, logVars, input){

  if(length(input$BMD_table_rows_selected) == 0){
    return(NULL)
  }
  

  selectedrowindex = input$BMD_table_rows_selected[length(input$BMD_table_rows_selected)]
  selectedrowindex = as.numeric(selectedrowindex)

  other_variables_id_col = colnames(gVars$inputPh()[[1]])[as.numeric(gVars$TPColID)]
  optVarsGroup = gVars$optVarsGroup
  
  # last updated table
  BMDVF = gVars$bmdlatestTable
  #select model indices
  experiment = BMDVF[selectedrowindex,"Experiment"]
  
  if(is.null(experiment)){ 
    return(NULL)
    print("null experiment in bmd_model_plot ")
  }
  others = BMDVF[selectedrowindex,c(other_variables_id_col, optVarsGroup)]
  feature = BMDVF[selectedrowindex,"Feature"]
  model = as.character(BMDVF[selectedrowindex,"Model"])
  if (model == "model_average") model = "average"

  to_plot_model = gVars$BMDMQ_latest[[paste(experiment, paste(others, collapse = "_"), feature,sep = "_")]][[model]]
  title_string = paste("Gene: ", feature, "; Model: ",model, "; Timepoint: " ,BMDVF[selectedrowindex,other_variables_id_col], "; Experiment: ",experiment, sep = "")
  
  
  ggp = bmdx::plot_bmdx(model = to_plot_model,cex= 16,xlim_u = as.numeric(input$xlim_u),
                        plot_ic50 = input$plot_ic50,
                        title_label = title_string)
  # ggp = plot(to_plot_model ,title_label = model)
  gVars$plotted_gene = ggp

  # if(gVars$in_glp_mode){
  #   
  #   #GLP MODE
  #   shinyalert(
  #     title = "GLP Mode Justification",
  #     text = "Why have you selected this row?",
  #     size = "s", 
  #     closeOnEsc = TRUE,
  #     closeOnClickOutside = FALSE,
  #     html = FALSE,
  #     type = "input",
  #     inputType = "text",
  #     inputValue = "",
  #     inputPlaceholder = "",
  #     showConfirmButton = TRUE,
  #     showCancelButton = FALSE,
  #     confirmButtonText = "Submit",
  #     confirmButtonCol = "#AEDEF4",
  #     timer = 0,
  #     imageUrl = "",
  #     animation = TRUE,
  #     callbackR =function(x) {logVars$BMDRowPlotJustification = x}
  #     
  #   )
  #   
  #   #save for report
  #   ts = Sys.time()
  #   logVars$BMDRowTimestamp = ts
  #   logVars$BMDRowExperiment = experiment
  #   logVars$BMDRowOtherCol = others
  #   logVars$BMDRowFeature = feature
  #   logVars$BMDRowModel = model
  #   
  # }
  # 
  # if(gVars$in_glp_mode){
  #   print("writing log file obj")
  #   line="-----------------------------------------------------"
  #   write(line,file=logVars$file,append=TRUE)
  #   
  #   line="COMPUTE BMD"
  #   write(line,file=logVars$file,append=TRUE)
  #   
  #   line="PLOT ROW"
  #   write(line,file=logVars$file,append=TRUE)
  #   
  #   ts = Sys.time()
  #   tsn = as.numeric(ts)
  #   fname = paste(logVars$location, "/plots/", toString(tsn), "_BMDRowPlot.RData", sep="")
  #   save(ggp, file = fname)
  #   line=paste("The Current R Data Object is saved:", fname, sep=" ")
  #   write(line,file=logVars$file,append=TRUE)
  #   
  # }
  
  #return(ggp)
  return(list(gVars, logVars))
  
}    