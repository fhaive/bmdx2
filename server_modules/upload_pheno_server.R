upload_pheno_server_function = function(gVars, logVars,input){#, output, server){
  
  factorize_cols <- function(phTable, idx) {
    for (i in idx) {
      phTable[,i] <- factor(phTable[,i])
    }
    return(phTable)
  }
  
  phTable <- gVars$inputPh()
  doseColID <- NULL
  TPColID <- NULL
  sampleColID <- as.integer(input$sampleIDCol)
  gVars$sampleColID = sampleColID # setting sample ID column in the variable list
  
  doseColID <- as.integer(input$doseCol)
  gVars$doseColID = doseColID # setting dose ID column in the variable list
  
  TPColID <- as.integer(input$TPCol)
  gVars$TPColID = TPColID # setting time point ID column in the variable list
  
  for (i in 1:length(phTable)) {
    phTable[[i]] = phTable[[i]][order(phTable[[i]][,doseColID]),]
  }
  
  
  doseColName <- NULL
  TPColName <- NULL
  sampleColName <- lapply(phTable, FUN = function(elem){colnames(elem)[as.integer(input$sampleIDCol)]})
  sampleIDs = lapply(1:length(phTable), FUN = function(i)phTable[[i]][sampleColName[[i]]])
  
  for (i in 1:length(phTable)) {
    if (any(duplicated(sampleIDs[[i]]))) {
      shinyjs::info(paste0("Sample IDs are not unique!\n\nPlease check the phenotype data and ensure that the selected Sample ID column has unique values!"))
      return(NULL)
    }else if (any(sampleIDs[[i]] == "") || any(sampleIDs[[i]] == " ") || any(is.na(sampleIDs[[i]]))) {
      shinyjs::info(paste0("Sample IDs contain BLANK and/or NA values!\n\nPlease check the phenotype data and ensure that the selected Sample ID column has complete information!"))
      return(NULL)
    }
  }
  
  for (i in 1:length(phTable)) {
    #Make sample ID column character
    if (is.integer(phTable[[i]][,sampleColName[[i]]])) {
      phTable[[i]][,sampleColName[[i]]] <- as.character(phTable[[i]][,sampleColName[[i]]])
    }
  }
  
  
  #Create factorized phTable
  phRH <- rhandsontable::hot_to_r(input$phenoTypesRH)
  factorIdx <- which(phRH$Type == "factor")
  factorCols <- NULL
  if (length(factorIdx) > 0) {
    factorCols <- lapply(phTable, FUN = function(elem) colnames(elem)[factorIdx])
    phFactor <- lapply(1:length(phTable), FUN = function(i) factorize_cols(phTable[[i]], factorIdx))
  }else{
    phFactor <- phTable
  }
  
  gVars$phTable <- phTable
  gVars$phFactor <- phFactor
  gVars$factorCols <- factorCols
  gVars$doseColID <- doseColID
  gVars$TPColID <- TPColID
  gVars$sampleColID <- sampleColID
  gVars$doseColName <- doseColName
  gVars$TPColName <- TPColName
  gVars$sampleColName <- sampleColName
  gVars$totalSamples <- nrow(phTable)
  gVars$filteredSamples <- nrow(phTable)
  gVars$removedSamples <- 0
  gVars$removedSamplesInfo <- NULL
  gVars$is_pheno_uploaded = TRUE
  gVars$optVarsGroup = input$optVarsGroup
  
  
  if (gVars$in_glp_mode) {
    #GLP MODE
    shinyalert(
      title = "GLP Mode Justification",
      text = "Please provide a description of your pheno file.",
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
      callbackR = function(x) {logVars$phenoDescription = x},
      inputId = "GLP_phenoDescription"
    )
  }
  
  #save for report
  ts = Sys.time()
  logVars$phenoTimestamp = ts
  logVars$phenoFile = input$fPheno$name
  logVars$phenoDoseVar = colnames(gVars$inputPh()[[1]])[as.numeric(input$doseCol)]
  logVars$phenoTPVar = colnames(gVars$inputPh()[[1]])[as.numeric(input$TPCol)]
  logVars$phenoSampleVar = colnames(gVars$inputPh()[[1]])[as.numeric(input$sampleIDCol)]
  logVars$phenoOptVar = input$optVarsGroup
  
  return(list(gVars = gVars, logVars = logVars))
}


phenotype_colnames = function(gVars){
  if (is.null(gVars$inputPh()))
    return(NULL)
  
  phTable <- gVars$inputPh()[[1]]
  colNames <- list("c" = NULL,"n" = NULL)
  coltypes <- unlist(lapply(phTable, class))
  coltypes.charOnly.idx <- which(coltypes == "character")
  coltypes.nonChar.idx <- which(!coltypes == "character")
  coltypes.charOnly.len <- length(coltypes.charOnly.idx)
  coltypes.nonChar.len <- length(coltypes.nonChar.idx)
  if (coltypes.charOnly.len > 0) {
    colNames[["c"]] <- colnames(phTable)[coltypes.charOnly.idx]
  }
  if (coltypes.nonChar.len > 0) {
    colNames[["n"]] <- colnames(phTable)[coltypes.nonChar.idx]
  }
  return(colNames)
}

phenotype_column_choices = function(gVars){
  if (is.null(gVars$phLoaded))
    return(c("NA"))
  phenoList = gVars$inputPh()
  choicesVec <- seq(1,ncol(phenoList[[1]]))
  choicesNames <- as.list(colnames(gVars$inputPh()[[1]]))
  names(choicesVec) <- choicesNames
  
  return(choicesVec)
}

render_number_of_samples = function(gVars){
  if (is.null(gVars$inputPh())) {
    nRow <- "NA"
  }else{
    nRow <- nrow(gVars$inputPh()[[1]])
  }
  return(paste0("Samples: ", nRow))
}

render_number_of_columns = function(gVars){
  if(is.null(gVars$inputPh())){
    nCol <- "NA"
  }else{
    nCol <- ncol(gVars$inputPh()[[1]])
  }
  return(paste0("Variables: ", nCol))
}


render_pheno_as_rhandsontable = function(gVars){
  shiny::validate(
    need(!is.null(gVars$inputPh()), "No phenotype file!")
  )
  phenoList = gVars$inputPh()
  phTable <- phenoList[[1]]
  colNames <- paste0(colnames(phTable), " [", c(1:ncol(phTable)), "]")
  colTypesList <- gVars$phColTypes()
  colClass <- unlist(lapply(phTable, class))
  
  colTypesDF <- data.frame(Variable=colNames, Type="-", Class=colClass, t(head(phTable)), stringsAsFactors=FALSE)
  colnames(colTypesDF)[c(4:ncol(colTypesDF))] <- paste0("Sample", c(1:(ncol(colTypesDF)-3)))
  if(!is.null(colTypesList[["c"]])){
    cIdx <- which(colnames(phTable) %in% colTypesList[["c"]])
    if(length(cIdx>0)){
      colTypesDF[cIdx,"Type"] <- "factor"
    }
  }
  if(!is.null(colTypesList[["n"]])){
    nIdx <- which(colnames(phTable) %in% colTypesList[["n"]])
    if(length(nIdx>0)){
      colTypesDF[nIdx,"Type"] <- "vector"
    }
  }
  rhandsontable::rhandsontable(colTypesDF, rowHeaders=NULL, readOnly=TRUE, contextMenu=FALSE) %>%
    hot_col("Type", readOnly=FALSE, type="dropdown", source=c("factor","vector"),
            renderer = "function (instance, td, row, col, prop, value, cellProperties) {
              Handsontable.renderers.NumericRenderer.apply(this, arguments);
              if (value == 'factor') {
              td.style.background = 'lightblue';
              } else if (value == 'vector') {
              td.style.background = 'lightgreen';
              }
              }"
    )
}

ggplot_from_pheno = function(gVars, input){
  
  phenoList = gVars$inputPh()
  combined_phTable = phenoList[[input$select_experiment_pheno]]
  combined_phTable = cbind(combined_phTable,input$select_experiment_pheno)
  colnames(combined_phTable)[ncol(combined_phTable)] = "Experiment"
  
  experiment = "Experiment"
  time_col = colnames(combined_phTable)[gVars$TPColID]
  dose_col = colnames(combined_phTable)[ gVars$doseColID]

  rel_cols = c(experiment, dose_col,time_col)
  
  pheno_df = combined_phTable[, rel_cols]
  
  library(ggplot2)
  
  plotList = list()
  
  for(i in 1:ncol(pheno_df)){
    tab = table(pheno_df[,i])
    df = data.frame(element = names(tab), percentage = as.numeric(tab))
    pie = ggplot(df, aes(x="", y=percentage, fill=element)) + geom_bar(stat="identity", width=1)
    pie = pie + coord_polar("y", start=0) + geom_text(aes(label = paste0(percentage)), 
                                                      position = position_stack(vjust = 0.5))
    pie = pie + labs(x = NULL, y = NULL, fill = NULL, title = colnames(pheno_df)[i])
    pie = pie + theme_classic() + 
      # theme(axis.line = element_blank(),
      #                   axis.text = element_blank(),
      #                   axis.ticks = element_blank(),
      #                   plot.title = element_text(hjust = 0.5, color = "#666666"))
    theme(axis.line = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          plot.title = element_text(size = 16, hjust = 0.5, color = "#666666"),  # Increase title font
          legend.text = element_text(size = 14),  # Increase legend text font
          legend.key.size = unit(1, "cm")  # Increase legend key size
    )
          
    
    plotList[[i]] = pie
  }
  
  p = easyGgplot2::ggplot2.multiplot(plotList[[1]],plotList[[2]],plotList[[3]],cols=3)
  
  
  return(p)
}


render_pheno_as_DT_datatable = function(gVars, input){
  phenoList = gVars$inputPh()
  
  phTable = phenoList[[input$select_experiment_pheno]]
  phTable = cbind(phTable,input$select_experiment_pheno)
  colnames(phTable)[ncol(phTable)] = "Experiment"
  
  gVars$rendered_pheno_table = phTable

  return(gVars)    
  
}

display_pheno_optional_variables = function(gVars, input){
  
  #returns the index of the list not the actual value
  sampleIDCol = strtoi(input$sampleIDCol)
  doseCol = strtoi(input$doseCol)
  tpCol = strtoi(input$TPCol)
  bmdcols = as.list(colnames(gVars$inputPh()[[1]]))
  
  bmdcols[c(doseCol, sampleIDCol, tpCol)] = NULL 
  print(bmdcols)
  
  
  tags$div(align = 'left',class = 'multicol', 
           checkboxGroupInput("optVarsGroup", label = "Optional Variables", 
                              #data needs to come from remaining columns of pheno file
                              choices = bmdcols,
                              inline = FALSE, width = '80%')
  )
}