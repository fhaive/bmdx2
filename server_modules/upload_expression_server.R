upload_gene_expression = function(gVars, logVars, input){
  experimental_data_file <- input$gx
  metadata = gVars$inputPh()
  x = input$x_ID
  y = input$y_ID
  
  # if no path to gene expression data is provided or if no metadata are loaded return NULL
  if (is.null(experimental_data_file) | is.null(metadata) | is.null(x) | is.null(y))
    return(NULL)
  
  # read excel file
  experimental_data = bmdx::read_excel_allsheets(filename =  experimental_data_file$datapath, 
                                                 first_col_as_rownames = input$first_col_as_rownames_exp,
                                                 is_rnaseq_raw_count = input$is_rnaseq,
                                                 check_numeric = input$check_numeric_exp)
  
  if(!is.logical(all.equal(names(experimental_data), names(metadata)))){
    gVars$error_input_data = TRUE
    gVars$inputGx <- NULL
    gVars$original_experimental_data = NULL
    gVars$is_gene_expression_uploaded = FALSE
    
    ### SET TO NULL PHENODATA UPLOADs
    
    gVars$sampleColID = NULL
    gVars$doseColID = NULL
    gVars$TPColID = NULL
    gVars$phTable <- NULL
    gVars$phFactor <- NULL
    gVars$factorCols <- NULL
    gVars$doseColID <- NULL
    gVars$TPColID <- NULL
    gVars$sampleColID <- NULL
    gVars$doseColName <- NULL
    gVars$TPColName <- NULL
    gVars$sampleColName <- NULL
    gVars$totalSamples <- NULL
    gVars$filteredSamples <- NULL
    gVars$removedSamples <- NULL
    gVars$removedSamplesInfo <- NULL
    gVars$is_pheno_uploaded = NULL
    gVars$optVarsGroup = NULL
    logVars$phenoDescription = NULL
    logVars$phenoTimestamp = NULL
    logVars$phenoFile = NULL
    logVars$phenoDoseVar = NULL
    logVars$phenoTPVar = NULL
    logVars$phenoSampleVar = NULL
    logVars$phenoOptVar = NULL
    gVars$phLoaded <- NULL

  }else{
    gVars$error_input_data = FALSE
    #TODO: REMOVE NEXT LINE, USED ONLY FOR TESTING
    # for (i in 1:length(experimental_data)) experimental_data[[i]] = experimental_data[[i]][sample(x = 1:nrow(experimental_data[[i]]),size = 100),]
    # for(i in 1:length(experimental_data)) experimental_data[[i]] = experimental_data[[i]][1:1000,]
    # 
    # create data structure
    sample_id_col = colnames(metadata[[1]])[as.numeric(input$sampleIDCol)]
    dose_id_col = colnames(metadata[[1]])[as.numeric(input$doseCol)]
    
    other_variables_id_col = colnames(metadata[[1]])[as.numeric(gVars$TPColID)]
    
    print(c(other_variables_id_col, gVars$optVarsGroup))
    data_dictionary = bmdx::create_data_structure(experimental_data, 
                                                  metadata, 
                                                  sample_id_col = sample_id_col, 
                                                  dose_id_col = dose_id_col, 
                                                  other_variables_id_col = c(other_variables_id_col, gVars$optVarsGroup),
                                                  x = x,
                                                  y = y)
    
    # TODO: this is the data_dictionary of experimental data that need to be used
    # by the BMD analysis. Before BMD analysis user can filter the data list by ANOVA and TREND test
    # or they can skip the filtering.
    gVars$inputGx <- data_dictionary #experimental_data
    gVars$original_experimental_data = experimental_data
    gVars$is_gene_expression_uploaded = TRUE
    
  }
  
  if (gVars$in_glp_mode) {
    
    #GLP MODE
    shinyalert(
      title = "GLP Mode Justification",
      text = "Please provide a description of your expression file.",
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
      callbackR =function(x) {logVars$expDescription = x}
      
    )
    
    #save for report
    ts = Sys.time()
    logVars$expTimestamp = ts
    logVars$expFile = input$gx$name
    logVars$expX = input$x_ID
    logVars$expY = input$y_ID
  }
  
  #save object in glp mode
  if (gVars$in_glp_mode) {
    line = "-----------------------------------------------------"
    write(line,file = logVars$file,append = TRUE)
    
    line = "LOAD EXPRESSION DATA"
    write(line,file = logVars$file,append = TRUE)
    
    ts = Sys.time()
    tsn = as.numeric(ts)
    # fname = paste(logVars$location, "/tables/", toString(tsn), "_combined_pheno_exp_data.RData", sep = "")
    # save(data_dictionary, file = fname)
    # line = paste("The Current R Data Object is saved:", fname, sep = " ")
    # write(line,file = logVars$file,append = TRUE)
  }
  
  
  return(list(gVars, logVars))
}

gene_expression_col_choices = function(gVars){
  if (is.null(gVars$dgxLoaded))
    return(c("NA"))
  
  choicesVec <- seq(1,ncol(gVars$inputDgx()))
  choicesNames <- paste0("Column ", choicesVec)
  names(choicesVec) <- choicesNames
  return(choicesVec)
}



render_experiment_list = function(gVars){
  if (is.null(gVars$original_experimental_data))
    return(NULL)
  
  selectInput(inputId = "experiment_for_data", label = "Select experiment",
              choices = names(gVars$original_experimental_data),
              selected = names(gVars$original_experimental_data)[1])
}

render_gene_expression_DT_datatable = function(gVars, input) {
  
  if (is.null(gVars$original_experimental_data)) return(NULL)
  
  inputGx = gVars$original_experimental_data[[input$experiment_for_data]]
  
  DT::datatable(inputGx, filter = "none",
                options = list(
                  search = list(regex = TRUE, caseInsensitive = FALSE),
                  scrollX = TRUE,
                  ordering = F
                )
  )
}

render_gene_expression_mds = function(gVars, input){
  if (is.null(gVars$original_experimental_data))
    return(NULL)
  
  inputGx = gVars$original_experimental_data[[input$experiment_for_data]]
  
  combined_phTable = gVars$inputPh()
  combined_phTable = combined_phTable[[input$experiment_for_data]]
  time_col = colnames(combined_phTable)[gVars$TPColID]
  dose_col = colnames(combined_phTable)[gVars$doseColID]
  samples_ids = combined_phTable[,gVars$sampleColID]
  rownames(combined_phTable) = samples_ids
  
  myCol = rainbow(length(unique(combined_phTable[,time_col])))
  names(myCol) = unique(combined_phTable[,time_col])
  
  topX = 0.1
  topX = nrow(inputGx) * topX
  topG = names(sort(apply(inputGx[, samples_ids],MARGIN = 1,FUN = var),decreasing = T)[1:topX])
  topExp = inputGx[topG, ]
  
  fit = cmdscale(dist(t(topExp)),k = 2)
  fit = cbind(fit, combined_phTable[rownames(fit),])
  colnames(fit)[1:2] = c("x","y")
  fit = as.data.frame(fit)
  fit[,time_col] = as.factor(fit[,time_col])
  fit[,dose_col] = as.factor(fit[,dose_col])
  
  p = ggplot(fit, aes(x = x, 
                  y = y, 
                  col = !!sym(input$mds_colors_id))) +
    geom_point()
  

  # p = ggplot2::ggplot(fit, ggplot2::aes_string(x = "x", y = "y", col = input$mds_colors_id)) + geom_point()
  return(p)
}