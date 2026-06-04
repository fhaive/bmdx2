render_compare_exp_relvars = function(gVars, input, id){
  if (is.null(gVars$bmdlatestTable)) { 
    return(NULL) 
  }
  other_variables_id_col = colnames(gVars$inputPh()[[1]])[as.numeric(gVars$TPColID)]
  options = (c("Experiment", "Model",other_variables_id_col, gVars$optVarsGroup))
  selectInput(id, "Relevant Variable", choices = options, selected = other_variables_id_col)
}


render_compare_exp_group_by = function(gVars, input, id){
  if (is.null(gVars$bmdlatestTable)) { 
    return(NULL) 
  }
  other_variables_id_col = colnames(gVars$inputPh()[[1]])[as.numeric(gVars$TPColID)]
  options = (c("None","Experiment", "Model",other_variables_id_col, gVars$optVarsGroup))
  selectInput(id, "Group By", choices = options, selected = "None")
}

render_compare_exp_othervars = function(gVars, input, id){
  if (is.null(gVars$bmdlatestTable)) { 
    return(NULL) 
  }
  other_variables_id_col = colnames(gVars$inputPh()[[1]])[as.numeric(gVars$TPColID)]
  options = (c("None","Experiment","Model", other_variables_id_col, gVars$optVarsGroup))
  selectInput(id, "Other Variables", choices = options, multiple = TRUE, 
              selected = "None", selectize = FALSE)
  
}