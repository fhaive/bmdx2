#options(java.parameters = c("-XX:+UseConcMarkSweepGC", "-Xmx8192m"))
# this instruction increase the maximum size of the file that can be uploaded in a shiny application
options(shiny.maxRequestSize = 300*1024^2)
set.seed(12)
srcDir <- dirname(getSrcDirectory(function(x){x})) #Get source directory

source("server_modules/import_libaries_and_source_files.R")
library(InteractiveComplexHeatmap)
library(ComplexHeatmap)

#Function to hide bsCollapsePanel
hideBSCollapsePanel <- function(session, panel.name) {
  session$sendCustomMessage(type = 'hideBSCollapsePanelMessage',
                            message = list(name = panel.name))
}


shinyServer(function(input, output, session) {

  source("server_modules/create_global_variable_lists.R")   # Global variable list

  #Module: Set Mode. Used to identify if the user run the tool in "GLP-like" model. If so, users need to justify their choices

      observeEvent(input$mode, {
        print(input$mode)
        res = create_directory_for_log(input$mode, gVars,logVars)
        gVars = res[[1]]
        logVars = res[[2]]
        if (input$mode == "TRUE") {       #FALSE is default and user has to close it themselve
          shinyBS::updateCollapse(session, "bsSidebar1", open="Load Phenotype Data", style=list("Set Mode"="success","Load Phenotype Data"="warning"))
        }
      })

      observe({logVars = glp_alerts(logVars)})
      observe({log_project_description(logVars)})



  #Module: Load data. This module includes two steps. Load phenodata that describes the samples in the experimental data and load experimental data that contains the actual measurements
  # Here some checks about data compatibility is performed. In particular to ensure that the two excel files (containing phenodata and experimental data) includes tabs that are named the same in the same order.

      gVars$inputPh <- eventReactive(input$load_pheno_submit, {
        if (is.null(input$fPheno)) return(NULL)
        metadata = bmdx::read_excel_allsheets(filename =  input$fPheno$datapath,check_numeric = F)
        gVars$phLoaded <- 1
        return(metadata)
      })

      # submodule: upload phenodata. it upload an en Excel file with N sheets such as the number of experiments that we are considering
          observeEvent(input$upload_pheno_submit, {
            shiny::validate(need(!is.null(gVars$inputPh()), "No Phenotype File Provided!"))
            res  = upload_pheno_server_function(gVars,logVars, input)
            gVars = res[[1]]
            logVars = res[[2]]
            gVars$last_step_performed = "phenodata"
            shinyBS::toggleModal(session, "importPhenoModal", toggle="close")
            shinyBS::updateButton(session, "import_pheno_submit", style="success", icon=icon("check-circle"))
            shinyBS::updateCollapse(session, "bsSidebar1", open="Load Experimental Data", style=list("Load Phenotype Data"="success","Load Experimental Data"="warning"))
          })

          observe({ log_load_pheno(logVars) })

          gVars$phColTypes <- reactive({return(phenotype_colnames(gVars))})
          gVars$phColChoices <- reactive({return(phenotype_column_choices(gVars))})

          output$select_experiment_pheno = renderUI({
            shiny::validate( need(!is.null(gVars$inputPh()),
                                  "No experimental data loaded!") )
            selectInput(inputId = "select_experiment_pheno", label = "Select experiment",
                        choices = names(gVars$inputPh()),
                        selected = names(gVars$inputPh())[1])
          })

    # submodule functionality: render phenodata table

          output$phenoDT <- DT::renderDataTable({
            shiny::validate( need(!is.null(gVars$inputPh()) & !is.null(input$select_experiment_pheno),
                                  "No experimental data loaded!") )

            gVars = render_pheno_as_DT_datatable(gVars, input)

            DT::datatable(gVars$rendered_pheno_table, filter="top",
                          selection = 'single',
                          options = list(
                            search = list(regex=TRUE, caseInsensitive=FALSE),
                            scrollX=TRUE,
                            ordering=T,
                            rownames = FALSE
                          ))

          },server=TRUE)

    # submodule functionality: render piechart that shows how the samples are distributed across the experimental conditions

      output$pheno_barplot = renderPlot({
        shiny::validate( need(!is.null(gVars$inputPh()) &
                              !is.null(gVars$TPColID) &
                              !is.null(gVars$doseColID) &
                              !is.null(input$select_experiment_pheno),
                              "No phenodata data loaded!") )

        p = ggplot_from_pheno(gVars, input)
        gVars$pheno_data_pie_chart_description = p
        p

      })

      output$phenoTypesRH <- rhandsontable::renderRHandsontable({
        render_pheno_as_rhandsontable(gVars)
      })

      #make hidden div appear
      observeEvent(input$set_opt_vars, {shinyjs::show("phenoOptVarsDiv")})

      #allows user to select other pheno columns to be considered in the bmd
      output$phenoOptVars = renderUI({
        display_pheno_optional_variables(gVars, input)
      })

      output$selSampleIDCol <- renderUI({ selectInput("sampleIDCol", "Sample ID Variable", choices = gVars$phColChoices(),selected = gVars$phColChoices()[1]) })
      output$selDoseCol <- renderUI({ selectInput("doseCol", "Dose Variable", choices = gVars$phColChoices(), selected = gVars$phColChoices()[2]) })
      output$selTPCol <- renderUI({ selectInput("TPCol", "Time Point Variable", choices = gVars$phColChoices(), selected = gVars$phColChoices()[3]) })


    # Submodule: load gene expression data

      observeEvent(input$upload_gx_submit, {

        shinyjs::html(id = "loadingText", "IMPORTING DATA")
        shinyjs::show(id = "loading-content")

        res = upload_gene_expression(gVars, logVars, input)
        gVars = res[[1]]
        logVars = res[[2]]

        if(gVars$error_input_data == TRUE){
          on.exit({ shinyjs::hide(id = "loading-content", anim = TRUE, animType = "fade") })

          shinyalert("Error with input data!", "The names of the tabs in the metadata and experimental data excel file do not match!", type = "error")
          shinyBS::updateButton(session, "import_pheno_submit", style="danger", icon=icon("exclamation-circle"))
          shinyBS::updateCollapse(session, "bsSidebar1", open="Load Phenotype Data", style=list("Load Phenotype Data"="danger","Load Phenotype Data"="warning"))
          shinyBS::updateCollapse(session, "bsSidebar1", open="Load Experimental Data", style=list("Load Phenotype Data"="dangelr","Load Experimental Data"="warning"))
          shinyBS::toggleModal(session, "importGxModal", toggle = "close")

        }

        shiny::validate( need(!gVars$error_input_data, "No experimental data loaded!") )

        gVars$last_step_performed = "expression_data"

        on.exit({ shinyjs::hide(id = "loading-content", anim = TRUE, animType = "fade") })

        shinyBS::toggleModal(session, "importGxModal", toggle = "close")
        shinyBS::updateButton(session, "import_expr_submit", style = "success", icon = icon("check-circle"))
        shinyBS::updateCollapse(session, "bsSidebar1", open = "Filtering", style = list("Load Experimental Data" = "success","Filtering" = "warning"))
        shiny::updateTabsetPanel(session, "display",selected = "gExpTab")
      })

      observe({log_expression_description(logVars)})

      output$select_experiment_gExpMat = renderUI({
        shiny::validate( need(!is.null(gVars$original_experimental_data),
                              "No experimental data loaded!") )
        render_experiment_list(gVars)
      })

      output$mds_colors <- renderUI({
        shiny::validate( need(!is.null(gVars$inputPh()) &
                                !is.null(gVars$TPColID) &
                                !is.null(gVars$doseColID ),  "No pheno data loaded!") )

        choices = c(gVars$TPColID, gVars$doseColID)
        selectInput(inputId = "mds_colors_id",label = "Color",
                                 choices = colnames(gVars$inputPh()[[1]])[choices],
                                 selected = colnames(gVars$inputPh()[[1]])[choices][1])
      })

  # Submodule functionality: plot MDS of the samples in the experimental data

          output$expression_mds = renderPlot({
            shiny::validate( need(!is.null(gVars$inputPh()) &
                                    !is.null(gVars$TPColID) &
                                    !is.null(gVars$doseColID) &
                                    !is.null(input$mds_colors_id) &
                                    !is.null(gVars$original_experimental_data) &
                                    !is.null(input$experiment_for_data),
                                  "No phenodata data loaded!") )

            p = render_gene_expression_mds(gVars, input)
            gVars$expression_mds = p
            p
          })

          output$gExpMat <- DT::renderDataTable({
            shiny::validate( need(!is.null(gVars$original_experimental_data),
                                  "No experimental data loaded!") )
            shiny::validate( need(!is.null(input$experiment_for_data),
                                  "No experiment selected!") )

            render_gene_expression_DT_datatable(gVars, input)
            },server = TRUE)


  # Module gene filtering. This remove genes from the experimental data to speed-up the computations. User can skip this step.
  # This functionality can be performed with different approaches, including: anova, trend test and differential analysis

      # TODO: this need to be updated. When the filtering is skipped, the original data list of expression data should not be touched!
      observeEvent(input$skip_anova_filtering_button, {
        shiny::validate( need(!is.null(gVars$phTable), "No Phenotype File Provided!") )
        shiny::validate( need(!is.null(gVars$inputGx), "No Expression File Provided!") )

        res = skip_filtering(gVars, logVars, input)
        gVars = res[[1]]
        logVars = res[[2]]
        gVars$last_step_performed = "filtering"

        shinyBS::updateButton(session, "anova_filtering_button", style = "success", icon = icon("check-circle"))
        shinyBS::updateButton(session, "skip_anova_filtering_button", style = "success", icon = icon("check-circle"))
        shinyBS::updateButton(session, "anova_filtering_button", style="warning", icon = icon("check-circle"))
        shinyBS::updateButton(session, "trend_filtering_button", style="warning", icon = icon("check-circle"))
        shinyBS::updateCollapse(session, "bsSidebar1", open = "Compute BMD", style = list("Filtering" = "success","Compute BMD" = "warning"))
      })

      observe({ log_skip_filtering(logVars) })

      observeEvent(input$trend_analysis,{
        shiny::validate( need(!is.null(gVars$phTable), "No Phenotype File Provided!") )
        shiny::validate( need(!is.null(gVars$inputGx), "No Phenotype File Provided!") )

        shinyjs::html(id = "loadingText", "TREND FILTERING")
        shinyjs::show(id = "loading-content")

    # Submodule: trend test

              res = run_trend_filtering(gVars, logVars, input)
              gVars = res[[1]]
              logVars = res[[2]]
              gVars$last_step_performed = "filtering"

              on.exit({ shinyjs::hide(id = "loading-content", anim = TRUE, animType = "fade") })

              shinyBS::toggleModal(session, "computeTrend", toggle = "close")
              shinyBS::updateButton(session, "fc_filtering_button", style = "success", icon = icon("check-circle"))
              shinyBS::updateButton(session, "trend_filtering_button", style = "success", icon = icon("check-circle"))
              shinyBS::updateButton(session, "skip_anova_filtering_button", style = "warning", icon = icon("check-circle"))
              shinyBS::updateButton(session, "anova_filtering_button", style = "warning", icon = icon("check-circle"))
              shinyBS::updateCollapse(session, "bsSidebar1", open = "Compute BMD", style = list("Filtering"="success","Compute BMD" = "warning"))
              shiny::updateTabsetPanel(session, "display",selected = "AnovaTab")
            })

            observe({ log_trend_filter(logVars)})

  # Submodule anova analysis

          observeEvent(input$anova_analysis, {
            shiny::validate(need(!is.null(gVars$phTable), "No Phenotype File Provided!"))
            shiny::validate( need(!is.null(gVars$inputGx), "No Phenotype File Provided!"))

            shinyjs::html(id = "loadingText", "ANOVA FILTERING")
            shinyjs::show(id = "loading-content")

            res_anova = run_anova_analysis(gVars, logVars, input)
            gVars = res_anova[[1]]
            logVars = res_anova[[2]]
            gVars$last_step_performed = "filtering"

            on.exit({ shinyjs::hide(id="loading-content", anim=TRUE, animType="fade") })

            shinyBS::toggleModal(session, "computeAnova", toggle="close")
            shinyBS::updateButton(session, "fc_filtering_button", style="success", icon=icon("check-circle"))
            shinyBS::updateButton(session, "anova_filtering_button", style="success", icon=icon("check-circle"))
            shinyBS::updateButton(session, "trend_filtering_button", style="warning", icon=icon("check-circle"))
            shinyBS::updateButton(session, "skip_anova_filtering_button", style="warning", icon=icon("check-circle"))
            shinyBS::updateCollapse(session, "bsSidebar1", open="Compute BMD", style=list("Filtering"="success","Compute BMD"="warning"))
            shiny::updateTabsetPanel(session, "display",selected = "AnovaTab")
          })


          observe({ log_anova_filtering(logVars) })

  # Submodule fold-change analysis

        observeEvent(input$fc_analysis, {
          shiny::validate(need(!is.null(gVars$phTable), "No Phenotype File Provided!"))
          shiny::validate( need(!is.null(gVars$inputGx), "No Phenotype File Provided!"))

          shinyjs::html(id="loadingText", "FC FILTERING")
          shinyjs::show(id="loading-content")

          res_fc = run_fc_analysis(gVars, logVars, input)
          gVars = res_fc[[1]]
          logVars = res_fc[[2]]
          gVars$last_step_performed = "filtering"

          on.exit({ shinyjs::hide(id="loading-content", anim=TRUE, animType="fade") })

          if(gVars$was_logFC_filtering_performed == TRUE){
            shinyBS::updateButton(session, "fc_filtering_button", style="success", icon=icon("check-circle"))
            shinyBS::updateButton(session, "anova_filtering_button", style="warning", icon=icon("check-circle"))
            shinyBS::updateButton(session, "trend_filtering_button", style="warning", icon=icon("check-circle"))
            shinyBS::updateButton(session, "skip_anova_filtering_button", style="warning", icon=icon("check-circle"))
            shinyBS::updateCollapse(session, "bsSidebar1", open="Compute BMD", style=list("Filtering"="success","Compute BMD"="warning"))
          }
          shinyBS::toggleModal(session, "computeFoldChange", toggle="close")
          shiny::updateTabsetPanel(session, "display",selected = "AnovaTab")
        })

        observe({ log_fc_filtering(logVars) })

        output$ExptimeSelAnova <- renderUI({ render_experiment_selection_anova_interface(gVars) })
        output$timePointSel <- renderUI({ render_time_selection_anova_interface(gVars, input) })

  # Submodule functionality: plot the result of the filtering in terms of statistics and pie chart to show percentage of variable genes

          #also used for skip & trend analysis display
          output$Anova_table <- DT::renderDataTable({
            gVars = compute_anova_tab(gVars, input)
            render_anova_tab_DT_datatable(gVars)
          },server=TRUE) #,sanitize.text.function=function(x){x}

          output$anovaPlot <- renderPlot({
            p = pie_chart_anova(gVars, input)
            gVars$anova_pie = p
            p
          })

          # Submodule functionality: download the data.frame with the result of the filtering step

          #TODO error handling not working when table is 0
          output$downloadAnovaData <- downloadHandler(
            filename = function() {
              paste("filtered_table.xlsx", sep = "")
            },
            content = function(file) {
              if(length(gVars$PValMat)==0){
                return(NULL)
              }

              shinyjs::html(id="loadingText", "Saving tables")
              shinyjs::show(id="loading-content")
              on.exit({
                print("inside on exit")
                shinyjs::hide(id="loading-content", anim=TRUE, animType="fade")
              })

              firstCall = TRUE

              # TODO: download the whole dataframe of anova results
              write.xlsx(gVars$PValMat, file)
              print("Anova table stored!")
            }
          )

  ####### BMD ANALYSIS  #######
  # Module: BMD analysis

      output$bmd_checkbox = renderUI({ render_model_selection_for_bmd_analysis(gVars, input) })

      observeEvent(input$bmd_analysis, {

        shiny::validate( need(!is.null(gVars$phTable), "No Phenotype File Provided!") )
        shiny::validate( need(!is.null(gVars$filtered_bmd), "No Phenotype File Provided!") )
        shinyjs::html(id="loadingText", "COMPUTING BMD")
        shinyjs::show(id="loading-content")

        res = run_bmd_analysis(gVars, logVars, input)
        gVars = res[[1]]
        logVars = res[[2]]
        gVars$is_average_computed = FALSE
        gVars$last_step_performed = "bmd"

        on.exit({ shinyjs::hide(id="loading-content", anim=TRUE, animType="fade") })

        shinyBS::toggleModal(session, "computeBMD", toggle="close")
        shinyBS::updateButton(session, "bmd_button", style="success", icon=icon("check-circle"))
        shinyBS::updateCollapse(session, "bsSidebar1", open=c("FunMappOne","AOP"), style=list("Compute BMD"="success","FunMappOne"="warning","AOP"="warning"))
        shiny::updateTabsetPanel(session, "display",selected = "BMDTab")
      })

      observe({ log_bmd(logVars) })

      ##### END IN BMD --> BMD VALUES #####

      # Render UI for BMD analysis
      output$ExptimeSelBMD <- renderUI({
        if(is.null(gVars$EXP_FIL)){ return(NULL) }
        experiment_names = unique(unlist(lapply(strsplit(x = names(gVars$filtered_bmd), split = "_"),FUN = function(elem)elem[[1]])))
        selectInput("experiment_bmd", "Experiment", choices=c("All", experiment_names), selected = "All")
      })

      output$timePointSel2 <- renderUI({
        if(is.null(input$experiment_bmd)){ return(NULL) }
        times = unique(unlist(lapply(strsplit(x = names(gVars$filtered_bmd), split = "_"),FUN = function(elem)elem[[2]])))
        selectInput("time_point_id_visual2", "Time Points", choices=c("All", times), selected = "All")
      })

  #Module functionality: render result of BMD statistics

      output$BMD_table <- DT::renderDataTable({
        gVars = render_main_bmd_table_results(gVars, input)
        DT::datatable(gVars$BMD_tab, filter="top",
                      selection = 'single',
                      options = list(
                        search = list(regex=TRUE, caseInsensitive=FALSE), #MAYBE remove search bars
                        scrollX=TRUE,
                        ordering=T,
                        rowCallback = JS('function(nRow, aData, iDisplayIndex, iDisplayIndexFull) {
                                        // Bold and green cells for conditions
                                        if (parseFloat(aData[2]) <=0.05)
                                        $("td:eq(3)", nRow).css("font-weight", "bold");
                                        }')
                      )
        )

      }, server = TRUE)

      # Submodule: Filter BMD results
          observeEvent(input$Apply_filter, {
            shiny::validate( need(!is.null(gVars$phTable), "No Phenotype File Provided!") )
            shiny::validate( need(!is.null(gVars$filtered_bmd), "No Expression File Provided!") )
            shiny::validate( need(!is.null(gVars$BMDMQ_latest), "No BMD computed!") )

            if(length(gVars$BMDMQ_latest)==0){
              shinyalert("Error with filtering!", "No filtering can be performed. Empty BMD table", type = "error")
            }

            shiny::validate( need(!length(gVars$BMDMQ_latest)==0, "No BMD computed!") )


            shinyjs::html(id="loadingText", "FILTERING BMD MODELS")
            shinyjs::show(id="loading-content")

            res = filter_bmd(gVars, logVars, input)
            gVars = res[[1]]
            logVars = res[[2]]
            on.exit({ shinyjs::hide(id="loading-content", anim=TRUE, animType="fade") })
            shiny::updateTabsetPanel(session, "display",selected = "BMDTab")
          })

          observe({ log_filter_bmd(logVars) })

      # Submodule: Perform model average
          observeEvent(input$modelaverage, {
            shiny::validate( need(!is.null(gVars$phTable), "No Phenotype File Provided!") )
            shiny::validate( need(!is.null(gVars$filtered_bmd), "No Expression File Provided!") )
            shiny::validate( need(!is.null(gVars$BMDMQ_latest), "No BMD computed!") )

            if(length(gVars$BMDMQ_latest)==0){
              shinyalert("Error with model average!", "No model average can be performed. Empty BMD table", type = "error")
            }

            shiny::validate( need(!length(gVars$BMDMQ_latest)==0, "No BMD computed!") )


            shinyjs::html(id="loadingText", "COMPUTE AVERAGE MODELS")
            shinyjs::show(id="loading-content")
            print("compute model average")
            res = compute_model_average(gVars, logVars, input)
            gVars = res[[1]]
            logVars = res[[2]]
            on.exit({ shinyjs::hide(id="loading-content", anim=TRUE, animType="fade") })
            shiny::updateTabsetPanel(session, "display",selected = "BMDTab")
          })

          observe({ log_model_average(logVars)  })

      # Submodule: Optimal model selection

          observeEvent(input$select_best_model,{
            shiny::validate( need(!is.null(gVars$phTable), "No Phenotype File Provided!") )
            shiny::validate( need(!is.null(gVars$filtered_bmd), "No Expression File Provided!") )
            shiny::validate( need(!is.null(gVars$BMDMQ_latest), "No BMD computed!") )
            shiny::validate( need(!is.null(input$select_best_model_options), "No method selected!") )

            if (length(gVars$BMDMQ_latest) == 0) {
              shinyalert("Empty list of models", "No models left in the list", type = "error")

            }else{
              print("select optimal model")
              res = select_optimal_model(gVars, logVars, input)
              gVars = res[[1]]
              logVars = res[[2]]
            }
          })

          observe({ log_optimal_model_selection(logVars)  })

      # Submodule: Reset model filtering

          observeEvent(input$resetmodelfilteringaverage, {
            shiny::validate( need(!is.null(gVars$phTable), "No Phenotype File Provided!") )
            shiny::validate( need(!is.null(gVars$filtered_bmd), "No Expression File Provided!") )
            shiny::validate( need(!is.null(gVars$BMDMQ_latest), "No BMD computed!") )

            res = reset_model_filter_average(gVars, logVars, input)
            gVars = res[[1]]
            logVars = res[[2]]
            shiny::updateTabsetPanel(session, "display",selected = "BMDTab")
          })

          observe({ log_reset_model_filter_average(logVars) })

          #both cannot be TRUE at the same time
          observeEvent(input$constantVar,{
            if(input$constantVar == TRUE & input$BMDmodelVar == TRUE){
              shinyalert("Model Variance and Constant Variance cannot be TRUE at the same time!", "", type = "info")
              #set both to FALSE
              shiny::updateCheckboxInput(session, "constantVar", value=FALSE)
              shiny::updateCheckboxInput(session, "BMDmodelVar", value=FALSE)
            }
          })

          observeEvent(input$BMDmodelVar,{
            if(input$constantVar == TRUE & input$BMDmodelVar == TRUE){
              shinyalert("Model Variance and Constant Variance cannot be TRUE at the same time!", "", type = "info")
              shiny::updateCheckboxInput(session, "constantVar", value=FALSE)
              shiny::updateCheckboxInput(session, "BMDmodelVar", value=FALSE)
            }
          })

          observeEvent(input$BMDmodelVar,{
            if(input$BMDmodelVar == TRUE){ shinyjs::enable("LOOF") } else { shinyjs::disable("LOOF") }
          })

      # Submodule functionality: Plot fitted model

          output$bmd_fitting = renderPlot({
            res = bmd_model_plot(gVars, logVars, input)
            gVars = res[[1]]
            logVars = res[[2]]

            p = gVars$plotted_gene
            if(!is.null(p)){
              p
            }
          })

          # Submodule functionality: Plot fitted model formula

          output$model_formula = renderText({
            res =  bmd_print_model_formula(gVars, logVars, input)
            gVars = res[[1]]
            logVars = res[[2]]
            p = gVars$current_mod_formula
            if(!is.null(p)){
              print(p)
            }

          })

  # Submodule: Compare the result of BMD analysis across multiple time points

  ### COMPARE BMD DISTRIBUTION BY HISTOGRAM
          output$BMDVals_colorby   <- renderUI({ render_compare_tp_color_by(   gVars, input, id="BMDVals_colorby_input")  })
          output$BMDVals_groupby1  <- renderUI({ render_compare_tp_group_by(   gVars, input, id="BMDVals_groupby1_input") })
          output$BMDVals_groupby2  <- renderUI({ render_compare_tp_group_by2(  gVars, input, id="BMDVals_groupby2_input") })
          output$BMDVals_filtercol <- renderUI({ render_compare_tp_filter_cols(gVars, input, id="BMDVals_filtercol_input") })
          output$BMDVals_filterby  <- renderUI({ render_tp_bmd_filt_val(gVars, input) })

          observeEvent(input$plotBMDVals, {
            res = create_histogram_plot_from_bmd_table(gVars, logVars, input)
            gVars = res$gVars
            logVars = res$logVars
          })

          output$BMD_dist_TP = renderPlotly({ if(is.null(gVars$BMD_dist_TP)){ return(NULL) } else {return(ggplotly(gVars$BMD_dist_TP))}  })
          output$BMDL_dist_TP = renderPlotly({ if(is.null(gVars$BMDL_dist_TP)){ return(NULL) } else {return(ggplotly(gVars$BMDL_dist_TP))}  })
          output$BMDU_dist_TP = renderPlotly({ if(is.null(gVars$BMDU_dist_TP)){ return(NULL) } else {return(ggplotly(gVars$BMDU_dist_TP))}  })
          output$BMR_dist_TP = renderPlotly({ if(is.null(gVars$BMR_dist_TP)){ return(NULL) } else {return(ggplotly(gVars$BMR_dist_TP))}  })
          output$IC50_dist_TP = renderPlotly({ if(is.null(gVars$IC50_dist_TP)){ return(NULL) } else {return(ggplotly(gVars$IC50_dist_TP))}  })

  # Submodule: Compare the fitting statistics

  ### COMPARE LACK OF FIT P-VALUE DISTRIBUTION BY HISTOGRAM
          output$LOOF_colorby   <- renderUI({ render_compare_tp_color_by(   gVars, input, id="LOOF_colorby_input") })
          output$LOOF_groupby1  <- renderUI({ render_compare_tp_group_by(   gVars, input, id="LOOF_groupby1_input") })
          output$LOOF_groupby2  <- renderUI({ render_compare_tp_group_by2(  gVars, input, id="LOOF_groupby2_input") })
          output$LOOF_filtercol <- renderUI({ render_compare_tp_filter_cols(gVars, input, id="LOOF_filtercol_input") })
          output$LOOF_filterby  <- renderUI({ render_tp_filter_loof(gVars, input)})

          observeEvent(input$plotLOOF,{
            res =  create_histrogram_plot_for_lack_of_fit_pvalue(gVars, logVars, input)
            gVars = res$gVars
            logVars = res$logVars
          })

          output$BMD_pval_fitting = renderPlotly({ if(is.null(gVars$BMD_pval_fitting)){ return(NULL) } else {return(ggplotly(gVars$BMD_pval_fitting))} })
          output$BMD_r2_fitting = renderPlotly({ if(is.null(gVars$BMD_r2_fitting)){ return(NULL) } else {return(ggplotly(gVars$BMD_r2_fitting))} })
          output$BMD_aic_fitting = renderPlotly({ if(is.null(gVars$BMD_aic_fitting)){ return(NULL) } else {return(ggplotly(gVars$BMD_aic_fitting))} })


  # Submodule: Plot number of gene by time points
  ### COMPARE GENE BY TIME POINT
          output$tp_ff <- renderUI({   #this should be dependent on the previous
            if(is.null(gVars$bmdlatestTable)){ return(NULL) }
            other_variables_id_col = colnames(gVars$inputPh()[[1]])[as.numeric(gVars$TPColID)]
            #since setting the table names is hard coded we can do it here as well
            options = (c("Experiment", other_variables_id_col, gVars$optVarsGroup, "Feature", "Model"))
            selectInput("tp_ff_input", "First Feature", choices=options, selected="Model")
          })

          output$tp_groupby1  <- renderUI({ render_compare_tp_group_by(   gVars, input, id="tp_groupby1_input", add_time = FALSE) })
          output$tp_filtercol <- renderUI({ render_compare_tp_filter_cols(gVars, input, id="tp_filtercol_input") })
          output$tp_filterby <- renderUI({ render_tp_filterby(gVars, input)})    #this should be dependent on the previous

          observeEvent(input$plottp,  {
            print("test")
            res = plot_number_of_genes_by_time_point(gVars, logVars, input)
            gVars = res$gVars
            logVars = res$logVars
          })
          output$NGTime = renderPlotly({ if(is.null(gVars$NGTime)){ return(NULL) } else {return( ggplotly(gVars$NGTime))} })

  # Submodule: Plot BMD/BMDL distribution
  #### COMPARE BMD/BML DISTRIBUTION

          output$BMDBMDL_colorby   <- renderUI({ render_compare_tp_color_by(gVars, input, id="BMDBMDL_colorby_input")  })
          output$BMDBMDL_groupby1  <- renderUI({ render_compare_tp_group_by(gVars, input, id="BMDBMDL_groupby1_input") })
          output$BMDBMDL_groupby2  <- renderUI({ render_compare_tp_group_by2(gVars, input, id="BMDBMDL_groupby2_input") })
          output$BMDBMDL_filtercol <- renderUI({ render_compare_tp_filter_cols(gVars, input, id="BMDBMDL_filtercol_input") })
          output$BMDBMDL_filterby  <- renderUI({ render_compare_tp_bmd_bmdl_filterby(gVars, input) })   #this should be dependent on the previous

          observeEvent(input$plotBMDBMDL,  {
            res = plot_bmd_bmdl(gVars, logVars, input)
            gVars = res$gVars
            logVars = res$logVars
          })
          output$BMD_BMDL = renderPlotly({ if(is.null(gVars$BMD_BMDL)){ return(NULL) } else{ return(ggplotly(gVars$BMD_BMDL)) } })

  ### COMPARE FITTED MODELS
  # Submodule: Plot piechart of fitted models

          output$Fittedmodels_groupby1  <- renderUI({ render_compare_tp_group_by(gVars, input, id="Fittedmodels_groupby1_input") })
          output$Fittedmodels_groupby2  <- renderUI({ render_compare_tp_group_by2(gVars, input, id="Fittedmodels_groupby2_input") })
          output$Fittedmodels_filtercol <- renderUI({ render_compare_tp_filter_cols(gVars, input, id="Fittedmodels_filtercol_input") })
          output$Fittedmodels_filterby  <- renderUI({ render_compare_tp_fitted_models_filterby(gVars, input)})   #this should be dependent on the previous

          observeEvent(input$plotFittedmodels,  {
            res =  plot_fitted_models(gVars, logVars, input)
            gVars = res$gVars
            logVars = res$logVars
          })
          output$BMD_dist_models = renderPlot({ if(is.null(gVars$BMD_dist_models)){ return(NULL) } else{return(gVars$BMD_dist_models)}})


  # Submodule: Plot ECDF and Upset plots

          output$BMDComExp_relvar   <- renderUI({render_compare_exp_relvars(gVars, input, id="BMDComExp_relvar_input")  })
          output$BMDComExp_groupby2   <- renderUI({render_compare_exp_group_by(gVars, input, id="BMDComExp_groupby2_input")  })
          output$BMDComExp_othervars   <- renderUI({render_compare_exp_othervars(gVars, input, id="BMDComExp_othervars_input")  })
          output$compare_exp_filtercol <- renderUI({ render_compare_tp_filter_cols(gVars, input, id="compare_experiments_filtercol_input") })
          output$compare_exp_filterby  <- renderUI({ render_compare_compare_experiments_filterby(gVars, input) })

          output$BMDComExp_relvar_upset   <- renderUI({render_compare_exp_relvars(gVars, input, id="BMDComExp_relvar_input_upset")  })
          output$BMDComExp_groupby2_upset   <- renderUI({render_compare_exp_group_by(gVars, input, id="BMDComExp_groupby2_input_upset")  })
          output$BMDComExp_othervars_upset   <- renderUI({render_compare_exp_othervars(gVars, input, id="BMDComExp_othervars_input_upset")  })
          output$compare_exp_filtercol_upset <- renderUI({ render_compare_tp_filter_cols(gVars, input, id="compare_experiments_filtercol_input_upset") })
          output$compare_exp_filterby_upset  <- renderUI({render_compare_compare_experiments_filterby_upset(gVars, input) })


          observeEvent(input$plot_compare_experiments_ecdf, {
            res = make_compare_experiments_ecdf_plot(gVars, logVars,input)
            gVars = res[[1]]
            logVars = res[[2]]
          })

          observeEvent(input$plot_compare_experiments_upset, {
            res = make_compare_experiments_upset_plot(gVars, logVars,input)
            gVars = res[[1]]
            logVars = res[[2]]
          })

          output$cumulative_plot = renderPlotly({
            ecdf_plot = gVars$bmdcompareexpRes_ecdf_plot
            shiny::validate(need(!is.null(ecdf_plot), "No plots available yet"))
            ecdf_plot
          })

          output$upsetplot = renderPlot({
            upset_plot = gVars$bmdcompareexpRes_upset_plot
            shiny::validate(need(!is.null(upset_plot), "No plots available yet"))
            upset_plot
          })

  # Submodule: Download results of BMD analysis

          output$downloadBMDData <- downloadHandler(
            filename = function() {
              paste("bmd_table.xlsx", sep = "")
            },
            content = function(file) {
              if(length(gVars$bmdlatestTable)==0){
                print("No data table to save!")
                return(NULL)
              }

              shinyjs::html(id="loadingText", "Saving tables")
              shinyjs::show(id="loading-content")
              on.exit({
                print("inside on exit")
                shinyjs::hide(id="loading-content", anim=TRUE, animType="fade")
              })

              write.xlsx(gVars$bmdlatestTable, file)
            }
          )

  # Module: Compute Gene Frequencies across experimental conditions

          output$geneFrequency_groupby  <- renderUI({ render_compare_gene_frequency_group_by(gVars, input, id = "gene_frequency_groupby_input") })
          output$geneFrequency_splitby   <- renderUI({render_compare_gene_frequency_split_by(gVars, input, id = "gene_frequency_splitby_input")  })

          observeEvent(input$GeneFrequency,{

            shiny::validate( need(!is.null(gVars$bmdlatestTable), "No BMD performed!"))
            shiny::validate( need(!is.null(input$organism_gene_freq), "No Organism selected!"))
            shiny::validate( need(!is.null(input$correction_gene_freq), "No correction selected!"))
            shiny::validate( need(!is.null(input$percentage_th_gene_freq), "No percentage selected!"))

            # TODO ADD Logs

            shinyjs::html(id = "loadingText", "COMPUTE GENE FREQUENCY")
            shinyjs::show(id = "loading-content")
            res = compute_gene_freq(gVars, logVars,input)
            gVars = res$gVars
            logVars = res$logVars

            if(gVars$Error_compute_freq == TRUE){
              shinyalert("Error gene frequency!", "Cannot compute the frequency of dose dependent genes! More experimental conditions are needed", type = "error")
            }

            on.exit({ shinyjs::hide(id = "loading-content", anim = TRUE, animType = "fade") })
          })

          output$select_exp = shiny::renderUI({
            shiny::validate( need(!is.null(gVars$gene_freq_m_list), ""))
            selectInput(inputId = "select_exp_id", label = "Choose Split",
                        choices = names(gVars$gene_freq_m_list), multiple = FALSE, selected = "NONE")
          })

          output$gene_freq_m_list = DT::renderDataTable({
            shiny::validate( need(!is.null(input$select_exp_id), "No data to plot!"))
            shiny::validate( need(!is.null(gVars$gene_freq_m_list), "No data to plot!"))
            print(input$select_exp_id)
            DT::datatable(gVars$gene_freq_m_list[[input$select_exp_id]])
          })


          output$sliderUIGeneFreq <- renderUI({
            shiny::validate( need(!is.null(gVars$gene_freq_gsea_most_freq_genes$result), "No enrichment to plot!"))
            sliderInput(
              "selectTopEnrichmetGeneFreq",
              "Select a Row Number:",
              min = 1,
              max = nrow(gVars$gene_freq_gsea_most_freq_genes$result),
              value = 10,
              step = 1
            )
          })

          # Render UI only if the variable exists
          output$sourceSelectUIGeneFreq <- renderUI({
            shiny::validate( need(!is.null(gVars$gene_freq_gsea_most_freq_genes$result), "No enrichment to plot!"))
            selectInput(
              "selectSourceEnrichmentGeneFreq",
              "Choose a source:",
              choices = unique(gVars$gene_freq_gsea_most_freq_genes$result$source),
              selected = NULL
            )
          })

          output$bubble_enrichment_gene_freq = renderPlotly({
            shiny::validate( need(!is.null(gVars$gene_freq_gsea_most_freq_genes$result), "No enrichment to plot!"))
            shiny::validate( need(!is.null(input$selectSourceEnrichmentGeneFreq), "No enrichment to plot!"))
            shiny::validate( need(!is.null(input$selectTopEnrichmetGeneFreq), "No enrichment to plot!"))

            if(input$gene_frequency_splitby_input=="None"){
              plot_enrichment_gost_bubble(data = gVars$gene_freq_gsea_most_freq_genes$result,
                                          source_filter = input$selectSourceEnrichmentGeneFreq,
                                          top_n = as.numeric(input$selectTopEnrichmetGeneFreq))
            }else{
              plot_enrichment_gost_bubble(data = gVars$gene_freq_gsea_most_freq_genes$result,
                                          source_filter = input$selectSourceEnrichmentGeneFreq,
                                          top_n = as.numeric(input$selectTopEnrichmetGeneFreq),facet_column = "query")
            }



          })

          output$gene_freq_gsea_most_freq_genes_table = DT::renderDataTable({
            shiny::validate( need(!is.null(input$select_exp_id), "No data to plot!"))
            shiny::validate( need(!is.null(gVars$gene_freq_gsea_most_freq_genes), "No data to plot!"))
            # DT::datatable(gVars$gene_freq_gsea_most_freq_genes$result)

            DT::datatable(gVars$gene_freq_gsea_most_freq_genes$result, filter="top",
                          selection = 'single',
                          options = list(
                            search = list(regex=TRUE, caseInsensitive=FALSE),
                            scrollX=TRUE,
                            ordering=T,
                            rowCallback = JS('function(nRow, aData, iDisplayIndex, iDisplayIndexFull) {
                                        // Bold and green cells for conditions
                                        if (parseFloat(aData[2]) <=0.05)
                                        $("td:eq(3)", nRow).css("font-weight", "bold");
                                        }')
                          )
            )

          })





          output$download_gosttable <- downloadHandler(
            filename = function() {
              paste0("GSEA_Gene_Frequency_Table.xlsx")  # Set filename dynamically
            },
            content = function(file) {
              df = gVars$gene_freq_gsea_most_freq_genes$result

              if(is.null(df)){
                return(NULL)
              }

              shinyjs::html(id="loadingText", "Saving tables")
              shinyjs::show(id="loading-content")
              on.exit({
                print("inside on exit")
                shinyjs::hide(id="loading-content", anim=TRUE, animType="fade")
              })

              firstCall = TRUE


              openxlsx::write.xlsx(df, file)
              print("Gost table stored!")
            }
          )

          output$gene_freq_lollipol_plot_list = renderPlotly({
            shiny::validate( need(!is.null(input$select_exp_id), "No data to plot!"))
            shiny::validate( need(!is.null(gVars$gene_freq_lollipol_plot_list), "No data to plot!"))
            plot(gVars$gene_freq_lollipol_plot_list[[input$select_exp_id]])

          })

          output$gene_freq_lineplot_plot_list = renderPlotly({
            shiny::validate( need(!is.null(input$select_exp_id), "No data to plot!"))
            shiny::validate( need(!is.null(gVars$gene_freq_lineplot_plot_list), "No data to plot!"))
            plot(gVars$gene_freq_lineplot_plot_list[[input$select_exp_id]])

          })

          output$gene_freq_gsea_most_freq_genes = renderPlotly({
            shiny::validate( need(!is.null(gVars$gene_freq_gsea_most_freq_genes), "No data to plot!"))
            gostplot(gVars$gene_freq_gsea_most_freq_genes, capped = TRUE, interactive = TRUE)
          })

          output$download_gostplot <- downloadHandler(
            filename = function() {
              paste0("gostplot_", Sys.Date(), ".html")
            },
            content = function(file) {
              p <- gostplot(gVars$gene_freq_gsea_most_freq_genes, capped = TRUE, interactive = TRUE)
              htmlwidgets::saveWidget(p, file)
            }
          )

  # Module: Pathways enrichment analysis with FunMappONE. Doi of the article: 10.1186/s12859-019-2639-2


          #RENDER UI
          output$choosePath <- renderUI({render_choice_pathway_for_gene_heatmap(gVars, input, id = "selPath")})
          output$timePointSelPat <- renderUI({ render_time_point_pathway_selection(gVars, input, id="time_point_id_visualPat")})
          output$chose_lev1 <- renderUI({ selectInput("lev1", "Level 1", gVars$lev1_h,multiple = TRUE,selected = "All")})
          output$chose_lev2 <- renderUI({ render_level_2_selection(gVars, input, id = "lev2")})
          output$chose_lev3 <- renderUI({ render_level_3_selection(gVars, input, id = "lev3") })
          output$selectColumn <- renderUI({ selectInput("colID","Samples",c("All",rownames(gVars$KEGG_MAT)),multiple = TRUE,selected = "All")})
          output$timePointSelTab <- renderUI({ render_time_point_select_time(gVars, input, id="time_point_id_table") })


          # PERFORM ENRICHMENT ANALYSIS FUNMAPPONE
          observeEvent(input$enrichment_analysis, {
            if(is.null(gVars$bmdlatestTable)){
              shinyalert("Enrichment Error!", "No enrichment can be performed, empty BMD table.", type = "error")

            }

            shiny::validate( need(!is.null(gVars$bmdlatestTable), "No BMD performed!"))
            shinyjs::html(id = "loadingText", "COMPUTING ENRICHMENT")
            shinyjs::show(id = "loading-content")

            if(is.null( gVars$genes_human)){
              res = AOPfingerprintR::load_biomaRt_data()
              gVars$genes_human = res$genes_human
              gVars$genes_mouse = res$genes_mouse
              gVars$genes_rat = res$genes_rat
            }
            
            res = tryCatch({
              funmappone_perform_enrichment(gVars, logVars, input)
            },
            error = function(cond){
              shinyalert(paste("Something is wrong: ",cond, " Please check your internet connection", sep = ""), "", type = "error")
            })
            # res = funmappone_perform_enrichment(gVars, logVars, input)
            gVars = res[[1]]
            logVars = res[[2]]
            gVars$last_step_performed = "funmappone"

            shinyBS::toggleModal(session, "enrichPathways", toggle = "close")
            shinyBS::updateButton(session, "enrich_button", style = "success", icon = icon("check-circle"))
            shinyBS::updateCollapse(session, "bsSidebar1", open="AOP", style=list("Compute BMD"="success","AOP"="warning"))
            shinyBS::updateCollapse(session, "bsSidebar1", open = "Pairs Analysis", style = list("FunMappOne" = "success","Pairs Analysis" = "warnings"))
            shiny::updateTabsetPanel(session, "display",selected = "enrTab")
            observe({ log_enrichment_params(logVars) })
          })

          output$downloadFunmapponeInput <- downloadHandler(
            filename = function() {
              paste("input_funmappone.xlsx", sep = "")
            },
            content = function(file) {
              if (is.null(gVars$GList) || is.null(gVars$phTable)) {
                return(NULL)
              }

              shinyjs::html(id = "loadingText", "Saving tables")
              shinyjs::show(id = "loading-content")
              on.exit({
                print("inside on exit")
                shinyjs::hide(id = "loading-content", anim = TRUE, animType = "fade")
              })

              write.xlsx(gVars$GList[[1]], file, sheetName = names(gVars$GList)[1], row.names = FALSE)

              if (length(gVars$GList) > 1) {
                for (i in 2:length(gVars$GList)) {
                  write.xlsx(gVars$GList[[i]], file, sheetName = names(gVars$GList)[i], append = TRUE, row.names = FALSE)
                }
              }

              openxlsx::write.xlsx(x = gVars$phTable, file = file, sheetName = "Groups", append = TRUE, row.names = FALSE)
            }
          )


          ## PLOT FUNMAPPONE HEATMAP
          output$heatmap <- renderPlot({

            shiny::validate(need(expr = !is.null(gVars$EnrichDatList),message = "No data to plot in plot funmappone heatmap"))

            if(!is.null(gVars$EnrichDatList)){
              if( sum(unlist(lapply(gVars$EnrichDatList, nrow)))==0){ # if no enrichmet is present in any experimental condition
                shinyalert("Error enrichment!", "Nothing to show, no results from the enrichment.", type = "error")
              }
            }

            shiny::validate(need(expr = !is.null(gVars$toPlot),message = "No data to plot in plot funmappone heatmap"))

            print(ggplotify::as.ggplot(gVars$toPlot))
          },
          width = function(){
            if (!is.null(gVars$KEGG_MAT) && !is.null(gVars$toPlot)) {
              mwidth = (nrow(gVars$KEGG_MAT) * 20 ) + 10 *  max(sapply(rownames(gVars$KEGG_MAT),nchar))
              mwidth = max(600, mwidth)
              return(mwidth)
            }else{
              return("auto")
            }
          },
          height = function(){
            if (!is.null(gVars$KEGG_MAT) && !is.null(gVars$toPlot)) {
              hier = gVars$hierarchy[as.character(gVars$hierarchy$ID) %in% colnames(gVars$KEGG_MAT),]
              if(nrow(hier)>0){
                hier = hier[,as.numeric(input$level)]
                hier = as.character(unique(hier))
                mysize = (length(hier) * 20 ) + 10 * max(sapply(hier,nchar))
                # mysize = min(mysize,30e3)
                mysize = min(max(600, mysize),30e3)

                return(mysize)
              }else{
                return("auto")
              }

            }else{
              return("auto")
            }
          }


          )

          ### BUILD FUNMAPPONE HEATMAP
          observeEvent(c(input$do,
                         input$level,
                         input$lev1,
                         input$lev2,
                         input$lev3,
                         input$colID), {

                           gVars = build_funmappone_heatmap(gVars, logVars, input)

                         })


          observe({ log_build_funmmappone_heatmap(logVars) })

          ### DOWNLOAD FUNMAPPONE HEATMAP
          output$downloadData <- downloadHandler(
            filename = paste(tempdir(),"/maps.pdf",sep = ""),
            content = function(file) {
              wd = as.numeric(input$img_width)
              hi = as.numeric(input$img_height)
              # ts  =  Sys.time()
              # file = paste("funmappone_outputs/map_", toString(ts),".pdf", sep = "")
              if(!dir.exists("funmappone_outputs")){
                dir.create("funmappone_outputs/")
              }
              pdf("funmappone_outputs/map.pdf",width = wd,height = hi)
              plot(gVars$toPlot)
              dev.off()
              file.copy("funmappone_outputs/map.pdf", file)

            }
          )

          # output$timePointSelPatRangePlot <- renderUI({ render_time_point_pathway_selection(gVars, input, id="time_point_id_range_plot")})

          output$range_funmappone_groupby   <- renderUI({render_compare_exp_group_by(gVars, input, id = "range_funmappone_groupby_input")  })
          output$range_funmappone_groupby2   <- renderUI({render_compare_exp_othervars(gVars, input, id = "range_funmappone_groupby2_input")  })
          output$range_funmappone_filtercol <- renderUI({ render_compare_tp_filter_cols(gVars, input, id = "range_funmappone_filtercol_input") })
          output$range_funmappone_filterby  <- renderUI({ funmappone_render_compare_compare_experiments_filterby(gVars, input) })

          observeEvent(input$range_plot_funmappone,{
            print("in range plot funmappone")

            shinyjs::html(id = "loadingText", "BUILDING RANGE PLOT")
            shinyjs::show(id = "loading-content")

            funmappone_range_plot = render_pathway_range(gVars, logVars,input)
            gVars$funmappone_range_plot = funmappone_range_plot

            on.exit({ shinyjs::hide(id = "loading-content", anim = TRUE, animType = "fade") })

          })


          #
          output$pathways_range = renderPlotly({

            shiny::validate(need(expr = !is.null(gVars$funmappone_range_plot), message = "No data to plot"))
            if(!is.null(gVars$funmappone_range_plot)){
              ggplotly(gVars$funmappone_range_plot)
            }

          })

          # output$meanBMD_timepoint = renderPlotly({
          #   shiny::validate(need(expr = !is.null(gVars$EnrichDatList), message = "No data to plot"))
          #   ggp = plot_mean_BMD_time_plot(gVars, input, output)
          #   ggplotly(ggp)
          #
          # })

          # output$pathway_bubble = renderPlotly({
          #
          #   shiny::validate(need(expr = !is.null(gVars$EnrichDatList), message = "No data to plot"))
          #   p = enrichment_bubble_plot(gVars, input, output)
          #   gVars$enrichment_bubble_plot = p
          #
          #   if(!is.null(p)) ggplotly(p)
          # })


          output$path_bmd_dist = renderPlotly({

            p = plot_bmd_of_the_genes_in_a_pathway(gVars,input, output,experiment_col = "Experiment" ,enrich_ppi_info = FALSE)
            gVars$plot_bmd_of_the_genes_in_a_pathway = p
            if(is.null(p)) return(NULL)
            ggplotly(p)

          })
          
          output$path_bmd_dist_by_ppi_degree = renderPlotly({
            
            shiny::validate(need(expr = !is.null(input$organism), message = "No organism provided"))
            shiny::validate(need(expr = !is.null(input$idtype), message = "No gene type provided"))
            
            p = plot_bmd_of_the_genes_in_a_pathway(gVars,
                                                   input, 
                                                   output,
                                                   experiment_col = "Experiment",
                                                   enrich_ppi_info = TRUE,
                                                   gene_id_type = input$idtype,
                                                   organism = tolower(input$organism))
            
            gVars$plot_bmd_of_the_genes_in_a_pathway = p
            if(is.null(p)) return(NULL)
            ggplotly(p)
            
          })

          # Render list of enriched pathways for a specified experiment and time
          output$PAT_table = DT::renderDataTable({

            shiny::validate(need(expr = !is.null(gVars$EnrichDatList), message = "No data to plot"))
            shiny::validate(need(expr = !is.null(input$time_point_id_visualPat), message = "No data to plot"))

            exp_combination = input$time_point_id_visualPat
            render_patways_table(exp_combination,gVars, input, output)
          })


          output$PatTable = DT::renderDataTable({

            render_pathways_table_for_download(gVars, input, output)
          })

          output$downloadEnrichedPathwayTables = downloadHandler(
            filename = function() {
              paste("terms_enrichment_table.xlsx", sep = "")
            },
            content = function(file) {
              download_enriched_pathways_table(gVars, file)
            }
          )

          # GENES HEATMAP CODEs

          # GENERATE GENE HEATMAP
          observeEvent(input$doGeneHeatMap, {

            shinyjs::html(id="loadingText", "Rendering Map")
            shinyjs::show(id="loading-content")

            on.exit({
              print("inside on exit")
              shinyjs::hide(id="loading-content", anim=TRUE, animType="fade")
            })

# browser()
            heatmap_params = do_gene_heat_map(KEGG_MAT_GENES = gVars$KEGG_MAT_GENES,
                                              kegg_hierarchy = gVars$hierarchy,
                                              GList =  gVars$GList,
                                              KEGG_MAT = gVars$KEGG_MAT,
                                              pheno=gVars$pheno_funmappone,#pheno, #missing
                                              hls = gVars$hls,
                                              cls = gVars$cls,
                                              aspectRatio=F, #missing
                                              selPath = input$selPath,
                                              levelGene=as.numeric(input$levelGene),
                                              selScoreType = input$selScoreType) #returns list

            gVars$heatmap_params = heatmap_params
          })

          # PLOT GENE HEATMAP
          output$heatmapGenes <- renderPlot({

            gVars = plot_gene_heatmap(gVars, input)
            if(!is.null(gVars$grid_draw_plot)){
              plot(gVars$grid_draw_plot)
            }


          })


          observe({
            M = gVars$DATA
            if(is.null(M)){
              shinyjs::disable("computePathways")
            }else{
              shinyjs::enable("computePathways")
            }

            if(is.null(gVars$KEGG_MAT)){
              shinyjs::disable("do")
              shinyjs::disable("downloadData")
              shinyjs::disable("doCluster")
              shinyjs::disable("resetCluster")
            }else{
              shinyjs::enable("do")
              shinyjs::enable("downloadData")
              shinyjs::enable("doCluster")
              shinyjs::enable("resetCluster")
            }
          })

          observe({
            disable_ann <- FALSE
            disable_de <- FALSE
            disable_sva <- FALSE
            disable_combat <- FALSE
            disable_corr_skip <- FALSE
            disable_limma <- FALSE

            if(is.null(input$fPheno)){
              shinyjs::disable("load_pheno_submit")
            }else{
              shinyjs::enable("load_pheno_submit")
            }

            if(is.null(gVars$phTable)){
              shinyjs::disable("dirButton")
            }else{
              shinyjs::enable("dirButton")
            }

            if(is.null(gVars$phLoaded)){
              shinyjs::hide("phenoPreviewDiv")
            }else{
              shinyjs::show("phenoPreviewDiv")
            }
          })


# Module: Gene Pairs analysis

        output$gene_pair_experiments_filtercol <- renderUI({ render_compare_gene_pairs_filter_cols(gVars, input, id="gene_pairs_filter_experiment") })
        output$gene_pair_experiments_filterby  <- renderUI({ render_compare_gene_pairs_filter_vals(gVars, input)})

        output$gene_pair_time_filtercol <- renderUI({ render_compare_gene_pairs_time_filter_cols(gVars, input, id="gene_pairs_filter_time") })
        output$gene_pair_time_filterby  <- renderUI({ render_compare_gene_pairs_time_filter_vals(gVars, input)})

        observeEvent(input$gene_pairs_comparison_analysis, {


          shiny::validate( need(!is.null(gVars$bmdlatestTable), "No BMD performed!"))

          shinyjs::html(id="loadingText", "COMPARE PAIRS OF GENES")
          shinyjs::show(id="loading-content")
          
          # browser()
          
          if(is.null( gVars$genes_human)){
            res = AOPfingerprintR::load_biomaRt_data()
            gVars$genes_human = res$genes_human
            gVars$genes_mouse = res$genes_mouse
            gVars$genes_rat = res$genes_rat
          }

          res = run_gene_pairs_comparison_all_experiments(gVars, logVars, input)

          gVars   = res$gVars
          logVars = res$logVars
          gVars$last_step_performed = "gene_pairs"

          on.exit({ shinyjs::hide(id="loading-content", anim=TRUE, animType="fade") })

          shinyBS::toggleModal(session, "compareGenes", toggle="close")
          shinyBS::updateButton(session, "gene_comparison_button", style="success", icon=icon("check-circle"))
          shinyBS::updateCollapse(session, "bsSidebar1", style=list("Pairs Analysis"="success"))
          # shinyBS::updateCollapse(session, "bsSidebar1", style=list("GENEPAIRS"="success"))

          shiny::updateTabsetPanel(session, "display",selected = "GenePairsTab")
        })

        observe({log_compare_gene_pairs(logVars) })

        observeEvent(input$perform_clustering_gene_pairs,{
          shiny::validate( need(!is.null(gVars$gene_pair_comparison), "No gene pairs analysis performed!"))
          
          shinyjs::html(id="loadingText", "BUILDING HEATMAP")
          shinyjs::show(id="loading-content")
          
          res = cluster_gene_pairs_shiny(gVars, logVars, input)

          gVars = res[[1]]
          logVars = res[[2]]

          shiny::validate( need(!is.null(gVars$complex_heatmap), "No gene pairs analysis performed!"))
          makeInteractiveComplexHeatmap(input, output, session, draw(gVars$complex_heatmap), "complex_heatmap")

          on.exit({ shinyjs::hide(id="loading-content", anim=TRUE, animType="fade") })

          

          
        })

        observe({log_cluster_gene_pairs(logVars) })


        output$sliderUIGenePairs <- renderUI({
          shiny::validate( need(!is.null(gVars$res_GSEA_over_corr_clust$result), "No enrichment to plot!"))
          sliderInput(
            "selectTopEnrichmetGenePairs",
            "Select a Row Number:",
            min = 1,
            max = nrow(gVars$res_GSEA_over_corr_clust$result),
            value = 1,
            step = 1
          )
        })

        # Render UI only if the variable exists
        output$sourceSelectUIGenePairs <- renderUI({
          shiny::validate( need(!is.null(gVars$res_GSEA_over_corr_clust$result), "No enrichment to plot!"))
          selectInput(
            "selectSourceEnrichmentGenePairs",
            "Choose a source:",
            choices = unique(gVars$res_GSEA_over_corr_clust$result$source),
            selected = NULL
          )
        })

        output$bubble_enrichment_gene_pairs= renderPlotly({
          shiny::validate( need(!is.null(gVars$res_GSEA_over_corr_clust$result), "No enrichment to plot!"))
          shiny::validate( need(!is.null(input$selectSourceEnrichmentGenePairs), "No enrichment to plot!"))
          shiny::validate( need(!is.null(input$selectTopEnrichmetGenePairs), "No enrichment to plot!"))

          plot_enrichment_gost_bubble(data = gVars$res_GSEA_over_corr_clust$result,
                                      source_filter = input$selectSourceEnrichmentGenePairs,
                                      top_n = as.numeric(input$selectTopEnrichmetGenePairs),facet_column = "query")

        })

        output$GSEA_over_corr_clust_query_data_frame = DT::renderDataTable({
          shiny::validate( need(!is.null(gVars$GSEA_over_corr_clust_query), "No gene pairs analysis performed!"))

          query = gVars$GSEA_over_corr_clust_query
          query_df = c()

          for(i in 1:length(query)){
            query_df = rbind(query_df,cbind(names(query)[i], query[[i]]))
          }

          colnames(query_df) = c("cluster", "feature")
          gVars$query_df =query_df
          # DT::datatable(query_df)

          DT::datatable(query_df, filter="top",
                        selection = 'single',
                        options = list(
                          search = list(regex=TRUE, caseInsensitive=FALSE),
                          scrollX=TRUE,
                          ordering=T,
                          rowCallback = JS('function(nRow, aData, iDisplayIndex, iDisplayIndexFull) {
                                        // Bold and green cells for conditions
                                        if (parseFloat(aData[2]) <=0.05)
                                        $("td:eq(3)", nRow).css("font-weight", "bold");
                                        }')
                        )
          )
        })

        output$gost_clust_plot = renderPlotly({

          # shiny::validate(need(!is.null(gVars$heatmaply_input_mat), "No data to plot!"))
          # shiny::validate(need(ncol(gVars$heatmaply_input_mat)>1, "No data to plot!"))
          shiny::validate(need(!is.null(gVars$res_GSEA_over_corr_clust), "No data to plot!"))

          gostplot(gVars$res_GSEA_over_corr_clust, capped = TRUE, interactive = TRUE)
        })

        output$download_gost_clust_plot <- downloadHandler(
          filename = function() {
            paste0("gostplot_", Sys.Date(), ".html")
          },
          content = function(file) {
            p <- gostplot(gVars$res_GSEA_over_corr_clust, capped = TRUE, interactive = TRUE)
            htmlwidgets::saveWidget(p, file)
          }
        )


        output$gost_clust_table =  DT::renderDataTable({
          shiny::validate(need(!is.null(gVars$res_GSEA_over_corr_clust), "No data to plot!"))
          # DT::datatable(gVars$res_GSEA_over_corr_clust$result)

          DT::datatable(gVars$res_GSEA_over_corr_clust$result, filter="top",
                        selection = 'single',
                        options = list(
                          search = list(regex=TRUE, caseInsensitive=FALSE),
                          scrollX=TRUE,
                          ordering=T,
                          rowCallback = JS('function(nRow, aData, iDisplayIndex, iDisplayIndexFull) {
                                        // Bold and green cells for conditions
                                        if (parseFloat(aData[2]) <=0.05)
                                        $("td:eq(3)", nRow).css("font-weight", "bold");
                                        }')
                        )
          )

        })

        output$download_gost_clust <- downloadHandler(
          filename = function() {
            paste0("GSEA_Cluster_Table.xlsx")  # Set filename dynamically
          },
          content = function(file) {
            # Validate if the table exists
            shiny::validate(need(!is.null(gVars$res_GSEA_over_corr_clust), "No data available for download!"))

            # Extract the result table
            table_data <- gVars$res_GSEA_over_corr_clust$result

            # Write to an Excel file
            write.xlsx(table_data, file, row.names = FALSE)
          }
        )


        output$gene_pairs_table <- DT::renderDataTable({

          shiny::validate(need(!is.null(gVars$gene_pair_comparison), "No comparison performed!"))
          statistics = gVars$gene_pair_comparison
          
          # browser()
          
          if(input$convert_to_gene_symbol){
            genes_to_be_converted =  union(statistics$`Feature 1`,statistics$`Feature 2` )
            
            if(is.null( gVars$genes_human)){
              res = AOPfingerprintR::load_biomaRt_data()
              gVars$genes_human = res$genes_human
              gVars$genes_mouse = res$genes_mouse
              gVars$genes_rat = res$genes_rat
            }
            
            genes_human = gVars$genes_human
            genes_mouse = gVars$genes_mouse
            genes_rat = gVars$genes_rat
            
            gene_id_type = input$gene_comparison_gene_id_type
            organism  = input$gene_comparison_organism
            
            genes_converted = AOPfingerprintR::convert_genes_to_symbol(genes = genes_to_be_converted,
                                                                       gene_id_type =gene_id_type,
                                                                       organism = organism,
                                                                       genes_human=genes_human, 
                                                                       genes_mouse=genes_mouse, 
                                                                       genes_rat=genes_rat)
            
            genes_converted_uniques = unlist(lapply(genes_converted,FUN = function(elem){
              unlist(strsplit(x = elem,split = ";"))[1]
            }))
            
            df = data.frame(genes_to_be_converted=genes_to_be_converted,
                            genes_converted=genes_converted,
                            genes_converted_uniques=genes_converted_uniques)
            rownames(df)=genes_to_be_converted
            
            statistics$`Gene Symbol 1`= df[statistics$`Feature 1`,"genes_converted_uniques"]
            statistics$`Gene Symbol 2`= df[statistics$`Feature 2`,"genes_converted_uniques"]
            
            
            statistics = statistics[,c("Feature 1","Gene Symbol 1","Model 1", "Feature 2","Gene Symbol 2", "Model 2", "CorGenePatteerns", "NormalizedEuclideanDistance")]
            colnames(statistics)[(ncol(statistics)-1):ncol(statistics)] = c("Pearson Correlation", "Normalized Euclidean Distance")
          }else{
            statistics = statistics[,c("Feature 1","Model 1", "Feature 2", "Model 2", "CorGenePatteerns", "NormalizedEuclideanDistance")]
            colnames(statistics)[(ncol(statistics)-1):ncol(statistics)] = c("Pearson Correlation", "Normalized Euclidean Distance")
          }
          
          graph = load_ppi_data(organism=input$gene_comparison_organism,
                                gene_id_type=input$gene_comparison_gene_id_type,
                                graph_type = "ppi")
          
          edge_df = statistics
          edge_presence = check_edges_in_graph(graph, edge_df,feature1 = "Feature 1",feature2 = "Feature 2")
          edge_presence_ppi = edge_presence[which(edge_presence$`presence[rownames(edge_df)]`),]
          colnames(edge_presence_ppi)[ncol(edge_presence_ppi)] = "PPI connection"
          # View(edge_presence_ppi)
          
          exp = strsplit(x = rownames(statistics)[1],split = "_")[[1]][1]
          tp = strsplit(x = rownames(statistics)[1],split = "_")[[1]][2]
          
          rownames(edge_presence_ppi) = unlist(lapply(strsplit(x = rownames(edge_presence_ppi),split = "\\|"), FUN = function(elem){
            paste(exp,"_",tp,"_",elem[1],"____",exp,"_",tp,"_",elem[2], sep = "")
          }))
          
          gVars$edge_presence_ppi = edge_presence_ppi
          
          DT::datatable(edge_presence_ppi, filter="top",
                        rownames = F, selection = 'single',
                        options = list(
                          search = list(regex=TRUE, caseInsensitive=FALSE),
                          scrollX=TRUE,
                          ordering=T))

        })

        output$download_gene_pairs <- downloadHandler(
          filename = function() {
            paste0("Gene_Pairs_Comparison.xlsx")  # Name the file dynamically
          },
          content = function(file) {
            # Validate if the table exists
            shiny::validate(need(!is.null(gVars$edge_presence_ppi), "No comparison performed!"))

            # Prepare the data for export
            statistics <- gVars$edge_presence_ppi
            # statistics = statistics[,c("Feature 1","Model 1", "Feature 2", "Model 2", "CorGenePatteerns", "NormalizedEuclideanDistance")]
            # colnames(statistics)[(ncol(statistics)-1):ncol(statistics)] = c("Pearson Correlation", "Normalized Euclidean Distance")


            # Write the data to an Excel file
            write.xlsx(statistics, file)
          }
        )



        output$plot_gene_pairs = renderPlotly({
          
          # browser()
          
          shiny::validate(need(expr = length(input$gene_pairs_table_rows_selected)>0,
                               message = "Select data"))
          p = gene_pairs_print_models(gVars, logVars, input)
          gVars$gene_pairs_print_models = p
          plot(p)
        })

        output$pathway_genes_dose_dependent_patterns_comparison = renderUI({
          shiny::validate(need(expr = !is.null(gVars$EnrichDatList), message = "No data to plot"))
          shiny::validate(need(expr = !is.null(input$Experiment_gene_comparison_filter_value), message = "No data to plot"))
          shiny::validate(need(expr = !is.null(input$Time_gene_comparison_filter_value), message = "No data to plot"))

          render_pathway_genes_dose_dependent_patterns_comparison(gVars, input)

        })
        
        
        output$KE_genes_dose_dependent_patterns_comparison = renderUI({
          shiny::validate(need(expr = !is.null(gVars$KE_enrich_res), message = "No data to plot"))
          shiny::validate(need(expr = !is.null(input$Experiment_gene_comparison_filter_value), message = "No data to plot"))
          shiny::validate(need(expr = !is.null(input$Time_gene_comparison_filter_value), message = "No data to plot"))
          
          render_KE_genes_dose_dependent_patterns_comparison(gVars, input)
        })

        # output$cluster_gene_profiles_in_pathways = ({
        #   shiny::validate(need(expr = !is.null(input$pathways_gene_comparison), message = "No data to plot"))
        #   shiny::validate(need(expr = !is.null(input$Experiment_gene_comparison_filter_value), message = "No data to plot"))
        #   shiny::validate(need(expr = !is.null(input$Time_gene_comparison_filter_value), message = "No data to plot"))
        #   shiny::validate(need(expr = !is.null(gVars$BMDMQ_latest), message = "No data to plot"))
        #   shiny::validate(need(expr = !is.null(gVars$pathways_gene_pairs), message = "No data to plot"))
        #   shiny::validate(need(expr = !is.null(gVars$newdata), message = "No data to plot"))
        #   shiny::validate(need(expr = !is.null(gVars$gene_comparison_organism), message = "No data to plot"))
        #   
        #   shiny::validate(need(expr = !(input$pathways_gene_comparison %in% rownames(gVars$pathways_gene_pairs)==FALSE), message = "Please update the enrichment!"))
        #   browser()
        #   
        #   cluster_gene_profiles_in_pathways(selected_pathway = "Cytokine-cytokine receptor interaction", 
        #                                     experiment = "bleomycin",
        #                                     time = 24,
        #                                     fitted_models= optimal_models,
        #                                     enrichedList,
        #                                     newdata,
        #                                     plot_ppi_info = TRUE,
        #                                     gene_id_type = "ENSEMBL",
        #                                     organism = "human")  
        #   
        #   makeInteractiveComplexHeatmap(input, output, session, draw(gVars$complex_heatmap), "complex_heatmap")
        #   
        # })
        
        
        output$plot_pathways_patterns = renderPlotly({
          shiny::validate(need(expr = !is.null(input$pathways_gene_comparison), message = "No data to plot"))
          shiny::validate(need(expr = !is.null(input$Experiment_gene_comparison_filter_value), message = "No data to plot"))
          shiny::validate(need(expr = !is.null(input$Time_gene_comparison_filter_value), message = "No data to plot"))
          shiny::validate(need(expr = !is.null(gVars$BMDMQ_latest), message = "No data to plot"))
          shiny::validate(need(expr = !is.null(gVars$pathways_gene_pairs), message = "No data to plot"))
          shiny::validate(need(expr = !is.null(gVars$newdata), message = "No data to plot"))
          shiny::validate(need(expr = !is.null(gVars$gene_comparison_organism), message = "No data to plot"))
          
          shiny::validate(need(expr = !(input$pathways_gene_comparison %in% rownames(gVars$pathways_gene_pairs)==FALSE), message = "Please update the enrichment!"))

          p=plot_gene_profiles_in_pathways(gVars, input)
          
          
          selected_pathway  =  input$pathways_gene_comparison
          experiment  =  input$Experiment_gene_comparison_filter_value
          time  =  input$Time_gene_comparison_filter_value
          fitted_models  =  gVars$BMDMQ_latest #always to be used
          newdata  =  gVars$newdata
          enrichedList = gVars$EnrichDatList

          res = cluster_gene_profiles_in_pathways(selected_pathway = selected_pathway, 
                                            experiment = experiment,
                                            time = time,
                                            fitted_models= fitted_models,
                                            enrichedList,
                                            newdata,
                                            plot_ppi_info = input$funmappone_plot_ppi,
                                            method = input$Funmappone_clustering_distance,
                                            nclust = input$Funmappone_nclust,
                                            gene_id_type = input$idtype,
                                            organism = tolower(input$organism))
          
          makeInteractiveComplexHeatmap(input, output, session, draw(res), "complex_heatmap_gene_pathways")
          
          gVars$plot_gene_profiles_in_pathways = p
          plot(p)

        })

        
        output$plot_KE_patterns = renderPlotly({
          
          # browser()
          
          shiny::validate(need(expr = !is.null(input$KE_gene_comparison), message = "No data to plot"))
          shiny::validate(need(expr = !is.null(input$Experiment_gene_comparison_filter_value), message = "No data to plot"))
          shiny::validate(need(expr = !is.null(input$Time_gene_comparison_filter_value), message = "No data to plot"))
          shiny::validate(need(expr = !is.null(gVars$BMDMQ_latest), message = "No data to plot"))
          
          shiny::validate(need(expr = !is.null(gVars$KE_gene_pairs), message = "No data to plot"))
          
          shiny::validate(need(expr = !is.null(gVars$newdata), message = "No data to plot"))
          shiny::validate(need(expr = !is.null(gVars$gene_comparison_organism), message = "No data to plot"))
          
          shiny::validate(need(expr = !(input$KE_gene_comparison %in% gVars$KE_gene_pairs[,"Ke_description"]==FALSE), message = "Please update the enrichment!"))
          
          p=plot_gene_profiles_in_ke_shiny(gVars, input)
          
          
          selected_ke  =  input$KE_gene_comparison
          experiment  =  input$Experiment_gene_comparison_filter_value
          time  =  input$Time_gene_comparison_filter_value
          fitted_models  =  gVars$BMDMQ_latest #always to be used
          newdata  =  gVars$newdata
          ke_enrichment_results = gVars$KE_gene_pairs
          
          
          res = cluster_gene_profiles_in_KE(selected_ke= selected_ke,
                                            experiment,
                                            time,
                                            fitted_models,
                                            ke_enrichment_results,
                                            newdata,
                                            method = input$KE_clustering_distance,
                                            nclust = input$KE_nclust,
                                            plot_ppi_info = input$KE_plot_ppi,
                                            gene_id_type = input$ke_gene_identifier,
                                            organism = tolower(input$ke_organism))  
          
          
          makeInteractiveComplexHeatmap(input, output, session, draw(res), "complex_heatmap_gene_KE")
          
          gVars$plot_gene_profiles_in_pathways = p
          plot(p)
          
        })
        
        
        observeEvent(input$PPIButton,{

          shiny::validate(need(expr = !is.null(gVars$gene_comparison_organism), message = "No data to plot"))

          organism = gVars$gene_comparison_organism

          shinyjs::html(id="loadingText", "LOADING DATA")
          shinyjs::show(id="loading-content")

          if(is.null(gVars$ppi_g)){
            load(paste("data/",organism,"_ppi_tfs_regulation_graph.RData",sep = ""))
            gVars$ppi_g = g
          }

          on.exit({ shinyjs::hide(id="loading-content", anim=TRUE, animType="fade") })

          correction_method = input$gostPPIcorrection

          shiny::validate(need(!is.null(gVars$gene_pair_comparison), "No comparison performed!"))

          shinyjs::html(id="loadingText", "BUILD INTERACTION NETWORK")
          shinyjs::show(id="loading-content")

          # browser()
          
          res_buildPPI = buildPPI(gVars, input, logVars, gVars$ppi_g,organism,correction_method)

          on.exit({ shinyjs::hide(id="loading-content", anim=TRUE, animType="fade") })


          gVars = res_buildPPI$gVars
          logVars = res_buildPPI$logVars
        })

        observe({log_compare_gene_pairs_PPI(logVars) })

        output$gost_ppi_plot = renderPlotly({
          shiny::validate(need(!is.null(gVars$res_gost_ppi), "No data to plot!"))
          gostplot(gVars$res_gost_ppi, capped = TRUE, interactive = TRUE)
        })


        output$gost_ppi_table =  DT::renderDataTable({
          shiny::validate(need(!is.null(gVars$res_gost_ppi), "No data to plot!"))
          DT::datatable(gVars$res_gost_ppi$result)
        })

        output$plot_PPI = renderForceNetwork({
          shiny::validate(need(!is.null(gVars$PPI), "No data to plot!"))
          print(gVars$PPI)
        })

        output$PPI_stats <- DT::renderDataTable({
          shiny::validate(need(!is.null(gVars$PPI_stats), "No data to show!"))

          DT::datatable(gVars$PPI_stats, filter="top",
                        selection = 'single',
                        options = list(
                          search = list(regex=TRUE, caseInsensitive=FALSE), #MAYBE remove search bars
                          scrollX=TRUE,
                          ordering=T)
          )

        }, server = TRUE)

        output$plot_enrichment = renderPlotly({

          shiny::validate(need(!is.null(gVars$gene_pair_comparison), "No comparison performed!"))


          statistics = gVars$gene_pair_comparison
          statistics = statistics[,c(1:4,7)]

          correlation = statistics[,ncol(statistics)]

          neg = statistics[correlation<0,c(1,3)]
          neg = unique(as.vector(as.matrix(neg)))

          pos = statistics[correlation>0,c(1,3)]
          pos = unique(as.vector(as.matrix(pos)))

          if(length(neg)>0 & length(pos)>0){
            L = list("positive"=pos,"negative"=neg)
            multi_query = TRUE
          }else{
            multi_query = FALSE
            if(length(neg)>0 & length(pos)==0){
              L = neg
            }
            if(length(pos)>0 & length(neg)==0){
              L = pos
            }
            if(length(neg)==0 & length(pos)==0){
              return(NULL)
            }
          }

          gg = unlist(L)[1]
          if(grepl(pattern = "ENSG",x = gg)) organism = "hsapiens"
          if(grepl(pattern = "ENSR",x = gg)) organism = "rnorvegicus"
          if(grepl(pattern = "ENSM",x = gg)) organism = "mmusculus"

          res = gost(query = L,organism = organism,ordered_query = FALSE, correction_method = "bonferroni", domain_scope = "annotated",multi_query = multi_query)
          save(res,file = "gost.rdata")
          DT::datatable(res$result)

          publish_gosttable(res,
                            highlight_terms = res$result,
                            use_colors = TRUE,
                            show_columns = c("source", "term_name", "term_size", "intersection_size"),
                            filename = NULL)


          edge_list = rbind(neg,pos)
        })

# Module: Key event, AOP, AOP-fingerprint and KE-KE network analysis
# Doi of the articles: https://doi.org/10.1002/advs.202203984, https://doi.org/10.1002/advs.202400389

              output$range_ke_groupby   <- renderUI({ke_render_compare_exp_group_by(gVars, input, id="range_ke_groupby_input")  })
              output$range_ke_groupby2   <- renderUI({ke_render_compare_exp_othervars(gVars, input, id="range_ke_groupby2_input")  })
              output$range_ke_filtercol <- renderUI({ ke_render_compare_tp_filter_cols(gVars, input, id="range_ke_filtercol_input") })
              output$range_ke_filterby  <- renderUI({ ke_render_compare_compare_experiments_filterby(gVars, input) })

              output$range_AOP_fingerprint_groupby   <- renderUI({ke_render_compare_exp_group_by(gVars, input, id="range_AOP_fingerprint_groupby_input")  })
              output$range_AOP_fingerprint_groupby2   <- renderUI({ke_render_compare_exp_othervars(gVars, input, id="range_AOP_fingerprint_groupby2_input")  })
              output$range_AOP_fingerprint_filtercol <- renderUI({ ke_render_compare_tp_filter_cols(gVars, input, id="range_AOP_fingerprint_filtercol_input") })
              output$range_AOP_fingerprint_filterby  <- renderUI({ AOP_fingerprint_render_compare_compare_experiments_filterby(gVars, input) })

              output$range_AOP_fingerprint_bubble_plot_groupby   <- renderUI({AOP_fingerprint_bubble_plot_render_compare_exp_group_by(gVars, input, id="range_AOP_fingerprint_bubble_plot_groupby_input")  })
              output$range_AOP_fingerprint_bubble_plot_x_axis   <- renderUI({AOP_fingerprint_bubble_plot_render_compare_exp_x_axis(gVars, input, id="range_AOP_fingerprint_bubble_plot_x_axis_input")  })
              # output$range_AOP_fingerprint_bubble_plot_groupby2   <- renderUI({ke_render_compare_exp_othervars(gVars, input, id="range_AOP_fingerprint_bubble_plot_groupby2_input")  })
              output$range_AOP_fingerprint_bubble_plot_filtercol <- renderUI({ AOP_fingerprint_bubble_plot_render_compare_tp_filter_cols(gVars, input, id="range_AOP_fingerprint_bubble_plot_filtercol_input") })
              output$range_AOP_fingerprint_bubble_plot_filterby  <- renderUI({ AOP_fingerprint_bubble_plot_render_compare_compare_experiments_filterby(gVars, input) })

              # KE enrichment
              
              observeEvent(input$ke_enrichment_analysis,{
                shinyjs::html(id = "loadingText", "KEs ENRICHMENT")
                shinyjs::show(id = "loading-content")
                
                if(is.null( gVars$genes_human)){
                  res = AOPfingerprintR::load_biomaRt_data()
                  gVars$genes_human = res$genes_human
                  gVars$genes_mouse = res$genes_mouse
                  gVars$genes_rat = res$genes_rat
                }
                
                
                res = perform_ke_enrichment(gVars, logVars, input)
                gVars = res$gVars
                logVars = res$logVars

                observe({log_enrich_aop(logVars)  })


                on.exit({ shinyjs::hide(id = "loading-content", anim = TRUE, animType = "fade") })
                shinyBS::toggleModal(session, "KEenrichment", toggle = "close")
                shinyBS::updateButton(session, "KE_enrichment_button", style = "success", icon = icon("check-circle"))
                shinyBS::updateCollapse(session, "bsSidebar1", open = "Pairs Analysis", style = list("AOP" = "success","Pairs Analysis" = "warnings"))
                shiny::updateTabsetPanel(session, "display",selected = "aop")
                
              })


              # KE table
              output$KE_enrichment_stat_dataframe <- DT::renderDataTable({
                
                # browser()
                
                shiny::validate(need(!is.null(input$KEr_enrichment_table_convert_to_symbol), "No comparison performed!"))
                
                
                if(is.null( gVars$genes_human)){
                  res = AOPfingerprintR::load_biomaRt_data()
                  gVars$genes_human = res$genes_human
                  gVars$genes_mouse = res$genes_mouse
                  gVars$genes_rat = res$genes_rat
                }
                
                toPlotData = render_KE_enriched_data_frame(gVars, logVars, input)
                time_var = colnames(gVars$inputPh()[[1]])[as.numeric(gVars$TPColID)]
                toPlotData = toPlotData[,c("Experiment",
                                           time_var,
                                           "TermID",
                                           "Ke_description",
                                           "BMD","BMDL","BMDU",
                                           "pval","padj",
                                           "relevantGenesInGeneSet","GeneSetSize",
                                           "Genes",
                                           "a.name" ,                 
                                           "key_event_name",
                                           "Ke_type",
                                           "level",
                                           "system",                  
                                           "organ_tissue",
                                           "cell",
                                           "cell_component",          
                                           "secondary_system",
                                           "secondary_organ_tissue",
                                           "secondary_cell",          
                                           "secondary_cell_component")]
                
                if(input$KEr_enrichment_table_convert_to_symbol){
                  genes_human = gVars$genes_human
                  genes_mouse = gVars$genes_mouse
                  genes_rat =  gVars$genes_rat
                  ke_organism  =  tolower(input$ke_organism)
                  ke_gene_identifier  =  input$ke_gene_identifier
                  
                  toPlotData = AOPfingerprintR::convert_enrichment_genes_to_symbols(enrichment_df = toPlotData,
                                                                                    gene_id_type = ke_gene_identifier,
                                                                                    organism = ke_organism,
                                                                                    genes_human = genes_human,
                                                                                    genes_mouse = genes_mouse,
                                                                                    genes_rat = genes_rat)
                }
               
                
                gVars$KE_enrichment_res = toPlotData
                
                DT::datatable(toPlotData,
                              filter  =  "top",
                              selection  =  'single',
                              options  =  list(search  =  list(regex  =  TRUE, caseInsensitive  =  FALSE), #MAYBE remove search bars
                                               scrollX  =  TRUE,
                                               ordering  =  T,
                                               pageLength = 5))
                
              }, server = TRUE)
              
              
              output$KE_bmd_dist = renderPlotly({
                
                shiny::validate(need(!is.null(input$KE_enrichment_stat_dataframe_rows_selected), "No KE selected!"))
                
                # if(!is.null(input$KE_enrichment_stat_dataframe_rows_selected)){
                #   selectedrowindex  =  input$KE_enrichment_stat_dataframe_rows_selected[length(input$KE_enrichment_stat_dataframe_rows_selected)]
                #   selectedrowindex  =  as.numeric(selectedrowindex)
                if(is.null( gVars$genes_human)){
                  res = AOPfingerprintR::load_biomaRt_data()
                  gVars$genes_human = res$genes_human
                  gVars$genes_mouse = res$genes_mouse
                  gVars$genes_rat = res$genes_rat
                }
                
                  p = plot_bmd_of_the_genes_in_a_ke(gVars,logVars,
                                                    input, 
                                                    output,
                                                    experiment_col = "Experiment",
                                                    enrich_ppi_info = FALSE,
                                                    gene_id_type = input$ke_gene_identifier,
                                                    organism = tolower(input$ke_organism))
                  gVars$plot_bmd_of_the_genes_in_a_ke =p
                #   gVars$plot_bmd_of_the_genes_in_a_ke = p
                #   if(is.null( gVars$plot_bmd_of_the_genes_in_a_ke)) return(NULL)
                #   p
                # }
                p
              })
              
              output$KE_bmd_dist_by_ppi_degree = renderPlotly({
                
                shiny::validate(need(!is.null(input$KE_enrichment_stat_dataframe_rows_selected), "No KE selected!"))
                
              
                # if(!is.null(input$KE_enrichment_stat_dataframe_rows_selected)){
                #   selectedrowindex  =  input$KE_enrichment_stat_dataframe_rows_selected[length(input$KE_enrichment_stat_dataframe_rows_selected)]
                #   selectedrowindex  =  as.numeric(selectedrowindex)
                # browser()
                
                if(is.null( gVars$genes_human)){
                  res = AOPfingerprintR::load_biomaRt_data()
                  gVars$genes_human = res$genes_human
                  gVars$genes_mouse = res$genes_mouse
                  gVars$genes_rat = res$genes_rat
                }
                
                  p = plot_bmd_of_the_genes_in_a_ke(gVars,logVars,
                                                    input, 
                                                    output,
                                                    experiment_col = "Experiment",
                                                    enrich_ppi_info = TRUE,
                                                    gene_id_type = input$ke_gene_identifier,
                                                    organism = tolower(input$ke_organism))
                  
                  # gVars$plot_bmd_of_the_genes_in_a_ke = p
                  # if(is.null( gVars$plot_bmd_of_the_genes_in_a_ke)) return(NULL)
                  p
                # }
                
              })
              
              output$BMD_boxplot_ke_type_bmdl = renderPlotly({
                if(!is.null(gVars$KE_enrich_res)){
                  time_var = colnames(gVars$inputPh()[[1]])[as.numeric(gVars$TPColID)]
                  ke_enrichment_results = gVars$KE_enrich_res
                  ke_enrichment_results$Experiment = paste(ke_enrichment_results$Experiment,ke_enrichment_results[,time_var],sep = "_")
                  
                  res = plot_bmd_boxplots_by_ke_type(ke_enrichment_results,
                                                     fill_colors = c(MIE = "#235888", KE = "#F7BD03", AO = "#357D8A"),
                                                     text_size = 16,
                                                     facet_scales = "free",
                                                     angle_x = 0)
                  
                  pbmdl = res$BMDL
                  # pbmd= res$BMD
                  # pbmdu = res$BMDU
                  
                  # easyGgplot2::ggplot2.multiplot(pbmdl,pbmd, pbmdu, cols = 1)
                  # patchwork::wrap_plots(pbmdl,pbmd, pbmdu, cols = 1)
                }
                
              })
              
              output$BMD_boxplot_ke_type_bmd = renderPlotly({
                if(!is.null(gVars$KE_enrich_res)){
                  time_var = colnames(gVars$inputPh()[[1]])[as.numeric(gVars$TPColID)]
                  ke_enrichment_results = gVars$KE_enrich_res
                  ke_enrichment_results$Experiment = paste(ke_enrichment_results$Experiment,ke_enrichment_results[,time_var],sep = "_")
                  
                  res = plot_bmd_boxplots_by_ke_type(ke_enrichment_results,
                                                     fill_colors = c(MIE = "#235888", KE = "#F7BD03", AO = "#357D8A"),
                                                     text_size = 16,
                                                     facet_scales = "free",
                                                     angle_x = 0)
                  
                  # pbmdl = res$BMDL
                  pbmd= res$BMD
                  # pbmdu = res$BMDU
                  
                  # easyGgplot2::ggplot2.multiplot(pbmdl,pbmd, pbmdu, cols = 1)
                  # patchwork::wrap_plots(pbmdl,pbmd, pbmdu, cols = 1)
                }
                
              })
              
              output$BMD_boxplot_ke_type_bmdu = renderPlotly({
                if(!is.null(gVars$KE_enrich_res)){
                  time_var = colnames(gVars$inputPh()[[1]])[as.numeric(gVars$TPColID)]
                  ke_enrichment_results = gVars$KE_enrich_res
                  ke_enrichment_results$Experiment = paste(ke_enrichment_results$Experiment,ke_enrichment_results[,time_var],sep = "_")
                  
                  res = plot_bmd_boxplots_by_ke_type(ke_enrichment_results,
                                                     fill_colors = c(MIE = "#235888", KE = "#F7BD03", AO = "#357D8A"),
                                                     text_size = 16,
                                                     facet_scales = "free",
                                                     angle_x = 0)
                  
                  # pbmdl = res$BMDL
                  # pbmd= res$BMD
                  pbmdu = res$BMDU
                  
                  # easyGgplot2::ggplot2.multiplot(pbmdl,pbmd, pbmdu, cols = 1)
                  # patchwork::wrap_plots(pbmdl,pbmd, pbmdu, cols = 1)
                }
                
              })
              
              output$BMD_boxplot_ke_level = renderPlotly({
                if(!is.null(gVars$KE_enrich_res)){
                  
                  time_var = colnames(gVars$inputPh()[[1]])[as.numeric(gVars$TPColID)]
                  ke_enrichment_results = gVars$KE_enrich_res
                  ke_enrichment_results$Experiment = paste(ke_enrichment_results$Experiment,ke_enrichment_results[,time_var],sep = "_")
                  
                  p <- plot_bmd_by_ke_and_level(
                    ke_enrichment_results,
                    Biological_system_annotations,
                    text_size = 14,
                    angle_x = 30,
                    facet_scales = "free_y"
                  )
                  p
                }
                
              })
              
              
              output$BMD_boxplot_by_system = renderPlotly({
                if(!is.null(gVars$KE_enrich_res)){
                  
                  time_var = colnames(gVars$inputPh()[[1]])[as.numeric(gVars$TPColID)]
                  ke_enrichment_results = gVars$KE_enrich_res
                  ke_enrichment_results$Experiment = paste(ke_enrichment_results$Experiment,ke_enrichment_results[,time_var],sep = "_")
                  
                  p <- ke_bmd_distribution_by_system(ke_enrichment_results, 
                                                     Biological_system_annotations,
                                                     c("#235888", "#F7BD03", "#357D8A", "#FFDD00", 
                                                       "#68A5D4", "#FFB600", "#D9D7CD", "#641DFF",
                                                       "#0022d2", "#505a74", "#bf212f"))
                  p
                }
                
              })
              
              output$AOP_by_ssbdcategory = renderPlotly({ #renderPlotly({
                if(!is.null(gVars$aop_enrichment_results) & !is.null(input$ssbd_text_size)){
                  
                  time_var = colnames(gVars$inputPh()[[1]])[as.numeric(gVars$TPColID)]
                  aop_enrichment_results = gVars$aop_enrichment_results
                  aop_enrichment_results$Experiment = paste(aop_enrichment_results$Experiment,aop_enrichment_results[,time_var],sep = "_")
                  
                  # browser()
                  
                  p <- plot_aop_distribution_over_ssbd_categories(
                    aop_enrichment_results,
                    Annotate_AOPs,
                    text_size = as.numeric(input$ssbd_text_size)
                  )
                  
                  plotly::ggplotly(p)
                  
                }
                
              })
              
              

              ### Download KE/AOPs/AOPfingerprint results

              output$downloadAOP_fingerprint_results <- downloadHandler(
                filename = function() {
                  paste("AOP_fingerprints.xlsx", sep = "")
                },
                content = function(file) {
                  if (is.null(gVars$aop_fingerprint_results$detailed_results_only_enriched)) {
                    return(NULL)
                  }

                  shinyjs::html(id = "loadingText", "Saving tables")
                  shinyjs::show(id = "loading-content")
                  on.exit({
                    print("inside on exit")
                    shinyjs::hide(id = "loading-content", anim = TRUE, animType = "fade")
                  })

                  write.xlsx(gVars$aop_fingerprint_results$detailed_results_only_enriched, file, row.names = FALSE)

                }
              )

              output$downloadKE_enrich_res <- downloadHandler(
                filename = function() {
                  paste("KE_enrich_res.xlsx", sep = "")
                },
                content = function(file) {
                  if (is.null(gVars$KE_enrich_res)) {
                    return(NULL)
                  }

                  shinyjs::html(id = "loadingText", "Saving tables")
                  shinyjs::show(id = "loading-content")
                  on.exit({
                    print("inside on exit")
                    shinyjs::hide(id = "loading-content", anim = TRUE, animType = "fade")
                  })

                  write.xlsx(gVars$KE_enrich_res, file, row.names = FALSE)

                }
              )

              output$downloadAop_enrichment_results <- downloadHandler(
                filename = function() {
                  paste("Aop_enrichment_results.xlsx", sep = "")
                },
                content = function(file) {
                  if (is.null(gVars$aop_enrichment_results)) {
                    return(NULL)
                  }

                  shinyjs::html(id = "loadingText", "Saving tables")
                  shinyjs::show(id = "loading-content")
                  on.exit({
                    print("inside on exit")
                    shinyjs::hide(id = "loading-content", anim = TRUE, animType = "fade")
                  })

                  write.xlsx(gVars$aop_enrichment_results, file, row.names = FALSE)

                }
              )


              ### AOP Fingerprint

              observeEvent(input$AOP_fingerprint_bubble_plot,{
                # browser()
                res = make_render_aop_fingerprint_bubble_plot(gVars, logVars, input)
                gVars = res$gVars
                logVars = res$logVars


              })

              output$bubble_plot_AOP_fingerprint = renderPlotly({
                if(is.null(gVars$bubble_plot_AOP_fingerprint)) {
                  # shinyalert("Error AOP fingerprint!", "No AOP fingerprint found", type = "error")
                  p = make_empty_plot()
                }else{
                  p = gVars$bubble_plot_AOP_fingerprint
                  ggplotly(p)
                }

              })

              output$downloadbubble_plot_AOP_fingerprint <- downloadHandler(
                filename = function() {
                  paste("bubbleplot_AOP_fingerprint.pdf", sep = "")
                },
                content = function(file) {
                  if (is.null( gVars$bubble_plot_AOP_fingerprint)) {
                    return(NULL)
                  }

                  shinyjs::html(id = "loadingText", "Saving tables")
                  shinyjs::show(id = "loading-content")
                  on.exit({
                    print("inside on exit")
                    shinyjs::hide(id = "loading-content", anim = TRUE, animType = "fade")
                  })

                  ggsave(filename = file, plot =  gVars$bubble_plot_AOP_fingerprint,units = "cm",
                         width = as.numeric(input$AOP_fingerprint_bubble_plot_width),
                         height = as.numeric(input$AOP_fingerprint_bubble_plot_height), limitsize = FALSE)
                }
              )

              observeEvent(input$range_plot_AOP_fingerprint,{
                
                shinyjs::html(id = "loadingText", "BUILDING RANGE PLOT")
                shinyjs::show(id = "loading-content")
                res = render_AOP_fingerprint_range(gVars, logVars,input)
                gVars = res$gVars
                logVars = res$logVars
                on.exit({ shinyjs::hide(id = "loading-content", anim = TRUE, animType = "fade") })

              })

              output$range_AOP_fingerprint_plotly = renderPlotly({
                if(is.null(gVars$AOP_fingerprint_range_plot)) p = make_empty_plot() else p = gVars$AOP_fingerprint_range_plot
                
                fs = as.numeric(input$range_AOP_fingerprint_font_size)
                
                p = p + theme(
                  plot.title = element_text(size = fs),
                  axis.title = element_text(size = fs),
                  axis.text = element_text(size = fs),
                  legend.text = element_text(size = fs),
                  legend.title = element_text(size = fs)
                )
                ggplotly(p)
              })

              output$range_ke_plotly = renderPlotly({
                if(is.null(gVars$KE_range_plot)) p = make_empty_plot() else p = gVars$KE_range_plot
                ggplotly(p)
              })


              output$ExptimeSelKE <- renderUI({
                if (is.null(gVars$KE_enrich_res)) { return(NULL) }
                experiment_names = unique(gVars$KE_enrich_res[,"Experiment"])
                selectInput("experiment_selection_KE", "Experiment", choices = c("All", experiment_names), selected = "All")
              })

              output$timePointSelKE <- renderUI({
                if (is.null(gVars$KE_enrich_res)) { return(NULL) }
                time_var = colnames(gVars$inputPh()[[1]])[as.numeric(gVars$TPColID)]
                times =  unique(gVars$KE_enrich_res[,time_var])
                selectInput("time_point_selection_KE", "Time Points", choices = c("All", times), selected = "All")
              })

              ### KEr network
              output$ExptimeSelKEr <- renderUI({ render_experiment_selection_KEr_interface(gVars) })
              output$timePointSelKEr <- renderUI({ render_time_selection_KEr_interface(gVars, input) })

              observeEvent(input$Create_KEr_network,{

                if(input$KEr_group_by_ssbd == TRUE){
                  group_by = "ssbd"
                }else{
                  group_by = "aop"
                }
                # browser()
                
                shinyjs::html(id="loadingText", "BUILDING KEr NETWORKS")
                shinyjs::show(id="loading-content")
                
                res = compute_ke_networks(gVars, logVars, input, group_by)
                
                
                on.exit({ shinyjs::hide(id="loading-content", anim=TRUE, animType="fade") })
                


                gVars = res$gVars
                logVars = res$logVars

              })

              observe({log_create_ker_network(logVars)  })


              output$PlotKErList_A <- renderUI({

                shiny::validate(need(!is.null(gVars$KEr_net_list), "KEr are missing!"))
                shiny::validate(need(length(gVars$KEr_net_list)>1, "Not enough networks!"))

                selectInput("PlotKErList_A_id", "Network A", choices  =  names((gVars$KEr_net_list)), selected  =  names((gVars$KEr_net_list))[1])


              })

              output$PlotKErList_B <- renderUI({

                shiny::validate(need(!is.null(gVars$KEr_net_list), "KEr are missing!"))
                shiny::validate(need(length(gVars$KEr_net_list)>1, "Not enough networks!"))

                selectInput("PlotKErList_B_id", "Network B", choices  =  names((gVars$KEr_net_list)), selected  =  names((gVars$KEr_net_list))[2])


              })



              output$display_intersect = renderVisNetwork({
                
                shiny::validate(need(!is.null(gVars$KEr_net_list), "KEr are missing!"))
                shiny::validate(need(length(gVars$KEr_net_list)>1, "Not enough networks!"))

                shiny::validate(need(!is.null(input$PlotKErList_B_id), "KEr are missing!"))
                shiny::validate(need(!is.null(input$PlotKErList_A_id), "KEr are missing!"))


                netList = gVars$KEr_net_list

                NetA = netList[[input$PlotKErList_A_id]]
                NetB = netList[[input$PlotKErList_B_id]]

                netList2 = list(NetA, NetB)
                node_list = list()
                edge_list = list()

                for(i in 1:length(netList2)){
                  node_list[[i]] = netList[[i]]$x$nodes
                  edge_list[[i]] = netList[[i]]$x$edges

                }
                common_nodes <- Reduce(function(x, y) merge(x, y, by = "id"), node_list)
                common_edges <- Reduce(function(x, y) merge(x, y, by = c("from", "to")), edge_list)

                common_nodes$padj = pmin(common_nodes$padj.x,common_nodes$padj.y)
                common_nodes$BMDL = pmin(common_nodes$BMDL.x,common_nodes$BMDL.y)
                common_nodes$BMD = pmin(common_nodes$BMD.x,common_nodes$BMD.y)
                common_nodes$BMDU = pmin(common_nodes$BMDU.x,common_nodes$BMDU.y)


                idx = which(colnames(common_nodes)=="label.x")
                colnames(common_nodes)[idx] = "label"
                vn = plot_visNetwork(nodes = common_nodes,
                                     edges = common_edges,
                                     group_by = "group.x",
                                     numerical_variables = c("BMDL","BMD","BMDU"))

                vn = vn %>%
                  visPhysics(enabled = TRUE) %>%
                  visEvents(doubleClick = "
                      function(params) {
                        if(params.nodes.length > 0){
                          var nodeId = params.nodes[0];
                          var node = this.body.data.nodes.get(nodeId); // get full node data
                          node.physics = false; // change only physics
                          this.body.data.nodes.update(node); // update with full data
                        }
                      }")
                
                vn
              })

              output$display_difference = renderVisNetwork({
                
                shiny::validate(need(!is.null(gVars$KEr_net_list), "KEr are missing!"))
                shiny::validate(need(length(gVars$KEr_net_list)>1, "Not enough networks!"))

                shiny::validate(need(!is.null(input$PlotKErList_B_id), "KEr are missing!"))
                shiny::validate(need(!is.null(input$PlotKErList_A_id), "KEr are missing!"))

                netList = gVars$KEr_net_list

                NetA = netList[[input$PlotKErList_A_id]]
                NetB = netList[[input$PlotKErList_B_id]]

                nodes_netA = NetA$x$nodes
                nodes_netB = NetB$x$nodes

                edges_netA = NetA$x$edges
                edges_netB = NetB$x$edges


                nodes_diff <- nodes_netA[!nodes_netA$id %in% nodes_netB$id, ]
                edges_diff <- edges_netA[!(paste(edges_netA$from, edges_netA$to) %in% paste(edges_netB$from, edges_netB$to)), ]

                 vn = plot_visNetwork(nodes = nodes_diff,
                                     edges = edges_diff,
                                     group_by = "group.x",
                                     numerical_variables = NULL)
                 
                 vn = vn %>%
                   visPhysics(enabled = TRUE) %>%
                   visEvents(doubleClick = "
                      function(params) {
                        if(params.nodes.length > 0){
                          var nodeId = params.nodes[0];
                          var node = this.body.data.nodes.get(nodeId); // get full node data
                          node.physics = false; // change only physics
                          this.body.data.nodes.update(node); // update with full data
                        }
                      }")

                vn
              })
              
              output$display_union = renderVisNetwork({
                library(dplyr)
                # browser()
                
                shiny::validate(need(!is.null(gVars$KEr_net_list), "KEr are missing!"))
                shiny::validate(need(length(gVars$KEr_net_list)>1, "Not enough networks!"))
                
                shiny::validate(need(!is.null(input$PlotKErList_B_id), "KEr are missing!"))
                shiny::validate(need(!is.null(input$PlotKErList_A_id), "KEr are missing!"))
                
                netList = gVars$KEr_net_list
                
                netList = netList[c(input$PlotKErList_A_id,input$PlotKErList_B_id)]
                node_list = list()
                edge_list = list()
                
                # Extract nodes and edges
                for(i in seq_along(netList)) {
                  node_list[[i]] <- netList[[i]]$x$nodes
                  node_list[[i]]$source <- names(netList)[i]  # tag the source
                  edge_list[[i]] <- netList[[i]]$x$edges
                }
                
                # Combine nodes and handle duplicates
                all_nodes <- bind_rows(node_list)
                
                # Combine duplicate nodes (same id), keep unique rows
                all_nodes_union <- all_nodes %>%
                  group_by(id) %>%
                  summarise(
                    label = first(label),
                    padj = min(padj),
                    genes = paste(unique(genes), collapse = ","),
                    source = paste(unique(source), collapse = "-"),
                    .groups = "drop"
                  )
                
                # Combine edges and deduplicate
                all_edges_union <- bind_rows(edge_list) %>%
                  distinct(from, to, .keep_all = TRUE)
                
                # Plot using updated union network
                vn_union <- plot_visNetwork(
                  nodes = all_nodes_union,
                  edges = all_edges_union,
                  group_by = "source",
                  numerical_variables = NULL
                )
                
                vn_union = vn_union %>%
                  visPhysics(enabled = TRUE) %>%
                  visEvents(doubleClick = "
                      function(params) {
                        if(params.nodes.length > 0){
                          var nodeId = params.nodes[0];
                          var node = this.body.data.nodes.get(nodeId); // get full node data
                          node.physics = false; // change only physics
                          this.body.data.nodes.update(node); // update with full data
                        }
                      }")
                
                vn_union
                
              })


              output$display_make_visNetwork  = renderVisNetwork({
                shiny::validate(need(!is.null(input$experiment_KEr), "Experiment selection is missing!"))
                shiny::validate(need(!is.null(input$time_point_KEr), "Time selection is missing!"))
                shiny::validate(need(!is.null(gVars$KEr_net_list), "KEr are missing!"))

                experiment = input$experiment_KEr
                selected_time = input$time_point_KEr
                netList = gVars$KEr_net_list
                vn = netList[[paste(experiment, selected_time, sep = "_")]]
                
                vn = vn %>%
                  visPhysics(enabled = TRUE) %>%
                visEvents(doubleClick = "
                      function(params) {
                if(params.nodes.length > 0){
                  var nodeId = params.nodes[0];
                  var node = this.body.data.nodes.get(nodeId); // get full node data
                  node.physics = false; // change only physics
                  this.body.data.nodes.update(node); // update with full data
                }
                      }")
                
                
                return(vn)
              })

              output$downloadvisNetworkOutput <- downloadHandler(
                filename = function() {
                  "network_visualization.html"
                },
                content = function(file) {
                  # Save the network to the file
                  shiny::validate(need(!is.null(input$experiment_KEr), "Experiment selection is missing!"))
                  shiny::validate(need(!is.null(input$time_point_KEr), "Time selection is missing!"))
                  shiny::validate(need(!is.null(gVars$KEr_net_list), "KEr missing!"))

                  experiment = input$experiment_KEr
                  selected_time = input$time_point_KEr
                  netList = gVars$KEr_net_list
                  vn = netList[[paste(experiment, selected_time, sep = "_")]]

                  vn <- vn %>% visNetwork::visOptions(height = "900px", width = "1200px")
                  
                  vn = vn %>%
                    visPhysics(enabled = TRUE) %>%
                    visEvents(doubleClick = "
                      function(params) {
                        this.body.data.nodes.update([{id: nodeId, physics: false}], {overwrite: true});
                      }")
                  
                  
                  visSave(vn, file = file)
                }
              )

              # output$KE_tissue_selection <- renderUI({
              #
              #   unique_choices = unique(as.character(Biological_system_annotations$organ_tissue))
              #   unique_choices = unique_choices[!is.na(unique_choices)]
              #
              #   selectInput("KE_tissues", "Tissues",
              #               choices = unique_choices,
              #               selected = c("liver","lung","blood"),multiple = TRUE)
              # })
              #
              # output$KE_tissue_experiment_selection <- renderUI({
              #   if (is.null(gVars$KE_enrich_res)) { return(NULL) }
              #   experiment_names = unique(gVars$KE_enrich_res[,"Experiment"])
              #   selectInput("KE_tissue_filter_experiment", "Filter experiment", choices = c("None", experiment_names), selected = "All")
              # })
              #
              # observeEvent(input$KE_tissue_selection_action_button, {
              #   if(!is.null(input$KE_tissues)){
              #     if(is.null(gVars$KE_enrich_res)){
              #       p = make_empty_plot()
              #     }else{
              #
              #       if(input$KE_tissue_group_by_time){
              #         group_by_time = colnames(gVars$inputPh()[[1]])[as.numeric(gVars$TPColID)]
              #       }else{
              #         group_by_time = NULL
              #       }
              #
              #       if("None" %in% input$KE_tissue_filter_experiment){
              #         filter_experiment = NULL
              #       }else{
              #         filter_experiment = input$KE_tissue_filter_experiment
              #       }
              #
              #       pheno_colnames = c("TermID","Experiment",
              #                          colnames(gVars$inputPh()[[1]])[as.numeric(gVars$TPColID)],
              #                          "organ_tissue","BMD","Ke_description")
              #
              #
              #       p = group_enriched_ke_by_tissue(ke_enrichment_results = gVars$KE_enrich_res[,1:16],
              #                                       relevant_tissues = input$KE_tissues,group_by_time = group_by_time,
              #                                       pheno_colnames = pheno_colnames,
              #                                       filter_experiment = filter_experiment)
              #     }
              #
              #   }else{
              #     p = make_empty_plot()
              #   }
              #   gVars$KE_by_tissue_plot = p
              #
              #
              #   n_ke_by_tissue_background = table(Biological_system_annotations$organ_tissue)
              #
              #   if(!is.null(gVars$KE_enrich_res)){
              #     ke_enrichment_results = gVars$KE_enrich_res
              #
              #     unique_experiment = unique(ke_enrichment_results$Experiment)
              #     time_variable = colnames(gVars$inputPh()[[1]])[as.numeric(gVars$TPColID)]
              #     unique_timepoint = unique(ke_enrichment_results[,time_variable])
              #
              #
              #     df = c()
              #     for(exp_i in unique_experiment){
              #       for(tp in unique_timepoint){
              #         idx = intersect(which(ke_enrichment_results$Experiment == exp_i),
              #                         which(ke_enrichment_results[,time_variable] == tp))
              #
              #         if(length(idx)>0){
              #           n_ke_by_tissue_enrichment = table(ke_enrichment_results[idx,"organ_tissue"])
              #           df2 = data.frame("tissue" = names(n_ke_by_tissue_enrichment),
              #                            "nKE" = -1 * as.numeric(n_ke_by_tissue_enrichment),
              #                            "mode"="enrichment",
              #                            "Experiment" = exp_i,
              #                            time_variable = tp)
              #
              #           df1 = data.frame("tissue" = names(n_ke_by_tissue_background), "nKE" = as.numeric(n_ke_by_tissue_background), "mode"="background")
              #           df1 = cbind(df1, "Experiment" = exp_i, time_variable = tp)
              #
              #           df1 = df1[df1$tissue %in% df2$tissue,]
              #           df2 = df2[order(df2$nKE, decreasing = T),]
              #
              #           df = rbind(df, rbind(df1, df2))
              #         }
              #       }
              #     }
              #
              #     colnames(df)[ncol(df)] = time_variable
              #
              #     p = ggplot(df, aes(x=tissue, y=nKE)) +
              #       geom_segment( aes(xend=tissue, yend=0)) +
              #       geom_point( size=3, color="orange") +
              #       coord_flip() +
              #       theme_bw(base_size = 15) +
              #       xlab("") + facet_wrap(~timepoint)
              #
              #
              #   }else{
              #     p = make_empty_plot()
              #   }
              #
              #   gVars$KE_by_tissue_distribution_ggplot = p
              #
              # })
              #
              #
              # output$KE_by_tissue_distribution = renderPlotly({
              #
              #   if(!is.null( gVars$KE_by_tissue_distribution_ggplot )){
              #     p =  gVars$KE_by_tissue_distribution_ggplot
              #     ggplotly(p)
              #
              #   }
              #
              # })
              #
              # output$KE_tissue = renderPlotly({
              #
              #     if(!is.null(gVars$KE_by_tissue_plot)){
              #       ggplotly(gVars$KE_by_tissue_plot)
              #     }
              #
              # })

             

              # #### AOP table
              output$ExptimeSelAOP <- renderUI({
                if (is.null(gVars$aop_enrichment_results)) { return(NULL) }
                experiment_names = unique(gVars$aop_enrichment_results[,"Experiment"])
                selectInput("experiment_selection_AOP", "Experiment", choices = c("All", experiment_names), selected = "All")
              })

              output$timePointSelAOP <- renderUI({
                if (is.null(gVars$aop_enrichment_results)) { return(NULL) }
                time_var = colnames(gVars$inputPh()[[1]])[as.numeric(gVars$TPColID)]
                times =  unique(gVars$aop_enrichment_results[,time_var])
                selectInput("time_point_selection_AOP", "Time Points", choices = c("All", times), selected = "All")
              })
              
              output$ExptimeSelAOP_visplot <- renderUI({
                if (is.null(gVars$KE_enrich_res)) { return(NULL) }
                experiment_names = unique(gVars$aop_enrichment_results[,"Experiment"])
                selectInput("experiment_selection_AOP_visplot", "Experiment", choices = c(experiment_names), selected = experiment_names[1])
              })
              
              output$timePointSelAOP_visplot <- renderUI({
                if (is.null(gVars$KE_enrich_res)) { return(NULL) }
                time_var = colnames(gVars$inputPh()[[1]])[as.numeric(gVars$TPColID)]
                times =  unique(gVars$aop_enrichment_results[,time_var])
                selectInput("time_point_selection_AOP_visplot", "Time Points", choices = c(times), selected = times[1])
              })

              output$AOP_selection_for_visnet = renderUI({
                
                print("inside AOP_selection_for_visnet")

                shiny::validate(need(expr = !is.null(gVars$KE_enrich_res), message = "No data to plot"))
                shiny::validate(need(expr = !is.null(input$experiment_selection_AOP_visplot), message = "No data to plot"))
                shiny::validate(need(expr = !is.null(input$time_point_selection_AOP_visplot), message = "No data to plot"))
                
                render_AOP_for_visnet(gVars, input)
              
                
              })
              
              output$aop_net_visnetwork <- renderVisNetwork({
                print("inside aop_net_visnetwork")
                
                shiny::validate(need(expr = !is.null(input$experiment_selection_AOP_visplot), message = "No data to plot"))
                shiny::validate(need(expr = !is.null(input$time_point_selection_AOP_visplot), message = "No data to plot"))
                shiny::validate(need(expr = !is.null(input$AOP_visnetwork), message = "No data to plot"))
                shiny::validate(need(expr = !is.null(gVars$KE_enrich_res), message = "No data to plot"))
                
                shiny::validate(need(expr = !gVars$is_optimal_model_selected==FALSE, message = "Select optimal model first!"))
                shiny::validate(need(expr = !is.null(input$AOP_visnet_only_direct), message = "No data to plot"))
                
                
                shinyjs::html(id="loadingText", "RETRIEVE AOP INFORMATION")
                shinyjs::show(id="loading-content")
              
                organism  =  tolower(input$ke_organism)
                gene_id_type  =  input$ke_gene_identifier
                
                time_var = colnames(gVars$inputPh()[[1]])[as.numeric(gVars$TPColID)]
                
                # Use input$n_nodes to control the graph
                aop_id = input$AOP_visnetwork
                experiment = input$experiment_selection_AOP_visplot
                timepoint = input$time_point_selection_AOP_visplot
                
                ker <- read.delim("data/aop_ke_ker_20_march_2025.tsv", header=FALSE)
                colnames(ker) = c("aop", "ke1", "ke2", "rid", "r.type", "evidence", "understanding")
                
                STABLE_KE_edges_with_clusters_and_info_for_KE_Network <- read.csv("data/STABLE_KE_edges_with_clusters_and_info_for_KE_Network.csv")
                STABLE_KE_edges_with_clusters_and_info_for_KE_Network = STABLE_KE_edges_with_clusters_and_info_for_KE_Network[,c("ke1.AOP_ID",
                                                                                                                                 "ke2.AOP_ID",
                                                                                                                                 "ke1.AOP_Event_ID",
                                                                                                                                 "ke2.AOP_Event_ID",
                                                                                                                                 "type.r.")]
                
                aop1 = unlist(lapply(X = strsplit(x = STABLE_KE_edges_with_clusters_and_info_for_KE_Network$ke1.AOP_ID,split = "_"), FUN = function(elem)elem[1]))
                aop2 = unlist(lapply(X = strsplit(x = STABLE_KE_edges_with_clusters_and_info_for_KE_Network$ke2.AOP_ID,split = "_"), FUN = function(elem)elem[1]))
                STABLE_KE_edges_with_clusters_and_info_for_KE_Network$ke1.AOP_ID = aop1
                STABLE_KE_edges_with_clusters_and_info_for_KE_Network$ke2.AOP_ID = aop2
                
                if( all.equal(STABLE_KE_edges_with_clusters_and_info_for_KE_Network$ke1.AOP_ID,STABLE_KE_edges_with_clusters_and_info_for_KE_Network$ke2.AOP_ID) ){
                  STABLE_KE_edges_with_clusters_and_info_for_KE_Network = STABLE_KE_edges_with_clusters_and_info_for_KE_Network[,-2]
                  colnames(STABLE_KE_edges_with_clusters_and_info_for_KE_Network) = c("aop","ke1","ke2","r.type")
                  STABLE_KE_edges_with_clusters_and_info_for_KE_Network$r.type[STABLE_KE_edges_with_clusters_and_info_for_KE_Network$r.type == "DIRECTLY_LEADS_TO"] = "adjacent"
                  STABLE_KE_edges_with_clusters_and_info_for_KE_Network$r.type[STABLE_KE_edges_with_clusters_and_info_for_KE_Network$r.type == "INDIRECTLY_LEADS_TO"] = "non-adjacent"
                  ker = ker[,c("aop","ke1","ke2","r.type")]
                  ker = rbind(ker,STABLE_KE_edges_with_clusters_and_info_for_KE_Network)
                  ker = unique(ker)
                  
                }
                
                
                all_AOPs_and_their_KEs <- read_excel("data/all_AOPs_and_their_KEs.xlsx")
                # colnames(all_AOPs_and_their_KEs)
                #"aopwiki_id" "aop_title"  "ke_wiki_id" "type"       "title"   
                
                ke_enrichment_results = gVars$KE_enrich_res
                optimal_models_stats = gVars$bmdlatestTable
                
                if(is.null( gVars$genes_human)){
                  res = AOPfingerprintR::load_biomaRt_data()
                  gVars$genes_human = res$genes_human
                  gVars$genes_mouse = res$genes_mouse
                  gVars$genes_rat = res$genes_rat
                }
                
                genes_human = gVars$genes_human
                genes_mouse = gVars$genes_mouse
                genes_rat = gVars$genes_rat 

                # browser()
                
                vn = create_aop_visnetwork(aop_id,
                                      experiment,
                                      timepoint,
                                      ker,
                                      all_AOPs_and_their_KEs,
                                      ke_enrichment_results,
                                      optimal_models_stats,
                                      gene_id_type = gene_id_type,
                                      organism = organism,
                                      time_var = time_var,
                                      AOP_visnet_only_direct = input$AOP_visnet_only_direct
                                      )
                
                gVars$KE_and_TF_network_for_AOP = vn
                on.exit({ shinyjs::hide(id="loading-content", anim=TRUE, animType="fade") })
                vn
                
              })
              
              output$aop_based_ppi_genegene_network <- renderVisNetwork({
                print("inside aop_net_visnetwork")
                
                shiny::validate(need(expr = !is.null(input$experiment_selection_AOP_visplot), message = "No data to plot"))
                shiny::validate(need(expr = !is.null(input$time_point_selection_AOP_visplot), message = "No data to plot"))
                shiny::validate(need(expr = !is.null(input$AOP_visnetwork), message = "No data to plot"))
                shiny::validate(need(expr = !is.null(gVars$KE_enrich_res), message = "No data to plot"))
                shiny::validate(need(expr = !gVars$is_optimal_model_selected==FALSE, message = "Select optimal model first!"))
                shiny::validate(need(expr = !is.null(input$AOP_visnet_only_direct), message = "No data to plot"))
                shiny::validate(need(expr = !is.null(gVars$gene_pair_comparison), message = "No gene pairs computed"))
                shiny::validate(need(expr = !is.null(input$aop_ppi_genegene_net_top_n_genes), message = "No nr of genes selected"))
                
                shinyjs::html(id="loadingText", "RETRIEVE AOP INFORMATION")
                shinyjs::show(id="loading-content")
                
                # browser()
                
                organism  =  tolower(input$ke_organism)
                gene_id_type  =  input$ke_gene_identifier
                
                time_var = colnames(gVars$inputPh()[[1]])[as.numeric(gVars$TPColID)]
                
                # Use input$n_nodes to control the graph
                aop_id = input$AOP_visnetwork
                experiment = input$experiment_selection_AOP_visplot
                timepoint = input$time_point_selection_AOP_visplot
                
                ker <- read.delim("data/aop_ke_ker_20_march_2025.tsv", header=FALSE)
                colnames(ker) = c("aop", "ke1", "ke2", "rid", "r.type", "evidence", "understanding")
                
                STABLE_KE_edges_with_clusters_and_info_for_KE_Network <- read.csv("data/STABLE_KE_edges_with_clusters_and_info_for_KE_Network.csv")
                STABLE_KE_edges_with_clusters_and_info_for_KE_Network = STABLE_KE_edges_with_clusters_and_info_for_KE_Network[,c("ke1.AOP_ID",
                                                                                                                                 "ke2.AOP_ID",
                                                                                                                                 "ke1.AOP_Event_ID",
                                                                                                                                 "ke2.AOP_Event_ID",
                                                                                                                                 "type.r.")]
                
                aop1 = unlist(lapply(X = strsplit(x = STABLE_KE_edges_with_clusters_and_info_for_KE_Network$ke1.AOP_ID,split = "_"), FUN = function(elem)elem[1]))
                aop2 = unlist(lapply(X = strsplit(x = STABLE_KE_edges_with_clusters_and_info_for_KE_Network$ke2.AOP_ID,split = "_"), FUN = function(elem)elem[1]))
                STABLE_KE_edges_with_clusters_and_info_for_KE_Network$ke1.AOP_ID = aop1
                STABLE_KE_edges_with_clusters_and_info_for_KE_Network$ke2.AOP_ID = aop2
                
                if( all.equal(STABLE_KE_edges_with_clusters_and_info_for_KE_Network$ke1.AOP_ID,STABLE_KE_edges_with_clusters_and_info_for_KE_Network$ke2.AOP_ID) ){
                  STABLE_KE_edges_with_clusters_and_info_for_KE_Network = STABLE_KE_edges_with_clusters_and_info_for_KE_Network[,-2]
                  colnames(STABLE_KE_edges_with_clusters_and_info_for_KE_Network) = c("aop","ke1","ke2","r.type")
                  STABLE_KE_edges_with_clusters_and_info_for_KE_Network$r.type[STABLE_KE_edges_with_clusters_and_info_for_KE_Network$r.type == "DIRECTLY_LEADS_TO"] = "adjacent"
                  STABLE_KE_edges_with_clusters_and_info_for_KE_Network$r.type[STABLE_KE_edges_with_clusters_and_info_for_KE_Network$r.type == "INDIRECTLY_LEADS_TO"] = "non-adjacent"
                  ker = ker[,c("aop","ke1","ke2","r.type")]
                  ker = rbind(ker,STABLE_KE_edges_with_clusters_and_info_for_KE_Network)
                  ker = unique(ker)
                  
                }
                
                # browser()
                
                all_AOPs_and_their_KEs <- read_excel("data/all_AOPs_and_their_KEs.xlsx")
                
                ke_enrichment_results = gVars$KE_enrich_res
                optimal_models_stats = gVars$bmdlatestTable
                
                if(is.null( gVars$genes_human)){
                  res = AOPfingerprintR::load_biomaRt_data()
                  gVars$genes_human = res$genes_human
                  gVars$genes_mouse = res$genes_mouse
                  gVars$genes_rat = res$genes_rat
                }
                
                genes_human = gVars$genes_human
                genes_mouse = gVars$genes_mouse
                genes_rat = gVars$genes_rat 
                
                # vn = create_aop_visnetwork(aop_id,
                #                            experiment,
                #                            timepoint,
                #                            ker,
                #                            all_AOPs_and_their_KEs,
                #                            ke_enrichment_results,
                #                            optimal_models_stats,
                #                            gene_id_type = gene_id_type,
                #                            organism = organism,
                #                            time_var = time_var,
                #                            AOP_visnet_only_direct = input$AOP_visnet_only_direct
                # )
                
                shiny::validate(need(expr = !(input$time_point_selection_AOP_visplot!=input$Time_gene_comparison_filter_value), 
                                     message = "Time point for gene pairs analysis differs from the one selected for the plot!"))
                
                statistics = gVars$gene_pair_comparison
                
                # browser()
                
                vn = create_aop_based_ppi_genegene_network(
                  aop_id,
                  experiment,
                  timepoint,
                  statistics,
                  ker,
                  all_AOPs_and_their_KEs,
                  ke_enrichment_results,
                  optimal_models_stats,
                  gene_id_type = gene_id_type,
                  organism = organism,
                  time_var = time_var,
                  top_n_genes = input$aop_ppi_genegene_net_top_n_genes
                ) 
                
                gVars$AOP_ppi_key_players =vn
                on.exit({ shinyjs::hide(id="loading-content", anim=TRUE, animType="fade") })
                vn
                
              })
              
              output$AOP_enrichment_stat_dataframe <- DT::renderDataTable({
                render_AOP_enriched_data_frame(gVars, logVars, input)

                # shiny::validate(need(!is.null(gVars$aop_enrichment_results), "No enrichment performed!"))
                # shiny::validate(need(!is.null(input$time_point_selection_AOP), "No time point selection!"))
                # shiny::validate(need(!is.null(input$experiment_selection_AOP), "No experiment selection!"))
                # shiny::validate(need(!is.null(gVars$inputPh()), "No phenodata available!"))
                # shiny::validate(need(!is.null(gVars$TPColID), "No column ID for time point!"))
                #
                # if (input$experiment_selection_AOP == "All") {
                #   exp_idx = 1:nrow(gVars$aop_enrichment_results)
                # }else{
                #   exp_idx = which(gVars$aop_enrichment_results[,"Experiment"] == input$experiment_selection_AOP)
                # }
                #
                # if (input$time_point_selection_AOP == "All") {
                #   time_idx = 1:nrow(gVars$aop_enrichment_results)
                # }else{
                #   time_var = colnames(gVars$inputPh()[[1]])[as.numeric(gVars$TPColID)]
                #   time_idx = which(gVars$aop_enrichment_results[,time_var] == input$time_point_selection_AOP)
                # }
                #
                # good_idx = intersect(exp_idx, time_idx)
                #
                # shiny::validate(need(length(good_idx) > 0, "Nothing in the intersection!"))
                #
                # dataTable = DT::datatable(gVars$aop_enrichment_results[good_idx,], filter = "top",
                #                           selection = 'single',
                #                           options = list(
                #                             search = list(regex = TRUE, caseInsensitive = FALSE), #MAYBE remove search bars
                #                             scrollX = TRUE,
                #                             ordering = T))
                #
                # return(dataTable)
              }, server = TRUE)

              # output$network <- renderVisNetwork({
              #   load("nodes.RData")
              #   load("edges.RData")
              #
              #   visNetwork(nodes, edges) %>%
              #     visIgraphLayout()
              # })

              ### TPOD

              observeEvent(input$run_tpod,{
                shiny::validate(need(!is.null(gVars$bmdlatestTable), "No optimal model identified!"))

                if(gVars$is_optimal_model_selected == TRUE){

                  optimal_models_stats = gVars$bmdlatestTable
                  time_var = colnames(gVars$inputPh()[[1]])[as.numeric(gVars$TPColID)]
                  # Parameters for tPOD computation
                  pod_value <- input$pod_value #"BMD" #or "BMDL", "BMDU"
                  percentile <- as.numeric(input$percentile) #0.20 # a number between 0 and 1
                  lowest_method = input$lowest_method #"lowest" #or "LCRD"
                  
                  # split optimal_models_stats in a list of dataframe, one for each time point.
                  # the names of the dataframes will be model_stats_ timepoint
                  model_stats_list <- setNames(
                    split(optimal_models_stats, interaction(optimal_models_stats[["Experiment"]], 
                                                            optimal_models_stats[[time_var]], 
                                                            drop = TRUE, sep = "_")),
                    levels(interaction(optimal_models_stats[["Experiment"]], 
                                       optimal_models_stats[[time_var]], 
                                       drop = TRUE, sep = "_"))
                  )
                  
                  gVars$model_stats_list_for_tpod = model_stats_list
                  tpod_methods_list <- c("percentile", "mean", "first_mode", "lowest", "accumulation")
                  
                  # Compute tPOD for each experimental condition and method
                  result_tPOD <- lapply(model_stats_list, function(model_stats) {
                    res = apply_tpod_methods(model_stats = model_stats,
                                             tpod_methods_list = tpod_methods_list,
                                             pod_value = pod_value,
                                             percentile = percentile,
                                             lowest_method = lowest_method)
                    rownames(res) = res$Method
                    return(res)
                  })

                  gVars$tpod_statistics = result_tPOD

                  unique_times = unique(optimal_models_stats[,time_var])
                  unique_experiments = unique(optimal_models_stats[,"Experiment"])
                  
                  tPOD_plot_list = list()
                  
                  for(expi in unique_experiments){
                    for(ttp in unique_times){
                      BMD_values <-  gVars$model_stats_list_for_tpod[[paste(expi,"_",ttp,sep = "")]]$BMD
                      
                      lowest       = as.numeric(gVars$tpod_statistics[[paste(expi,"_",ttp,sep = "")]]["lowest","tPOD"])
                      percentile   = as.numeric(gVars$tpod_statistics[[paste(expi,"_",ttp,sep = "")]]["percentile","tPOD"])
                      mean_value   = as.numeric(gVars$tpod_statistics[[paste(expi,"_",ttp,sep = "")]]["mean","tPOD"])
                      accumulation = as.numeric(gVars$tpod_statistics[[paste(expi,"_",ttp,sep = "")]]["accumulation","tPOD"])
                      first_mode   = as.numeric(gVars$tpod_statistics[[paste(expi,"_",ttp,sep = "")]]["first_mode","tPOD"])
                      
                      density_plot = plot_BMD_tPOD_density(BMD_values, lowest = lowest,
                                                           percentile = percentile,
                                                           mean_value = mean_value,
                                                           accumulation = accumulation,
                                                           first_mode = first_mode)
                      
                      accumulaiton_plot = tpod_plot(pod_vector = gVars$model_stats_list_for_tpod[[paste(expi,"_",ttp,sep = "")]]$BMD,
                                                    tpod_method = "accumulation",
                                                    pod_value = "BMD",
                                                    tPOD = as.numeric(gVars$tpod_statistics[[paste(expi,"_",ttp,sep = "")]]["accumulation","tPOD"]),
                                                    xlog = T,
                                                    subtitle = "tPOD bleomycin exposure 24h") + theme(legend.position = "bottom")
                      
                      plots = list("density_plot"=density_plot,
                                   "accumulaiton_plot" = accumulaiton_plot)
                      
                      tPOD_plot_list[[paste(expi,"_",ttp,sep = "")]] = plots
                    }
                  }
                 
                  gVars$tPOD_plot_list = tPOD_plot_list
                }

              })

              output$tpod_select_experiment_pheno = renderUI({
                shiny::validate( need(!is.null(gVars$inputPh()),
                                      "No experimental data loaded!") )
                selectInput(inputId = "tPOD_accumulation_experiment", label = "Select experiment",
                            choices = names(gVars$inputPh()),
                            selected = names(gVars$inputPh())[1])
              })
              
              output$tpod_select_time_pheno = renderUI({
                shiny::validate( need(!is.null(gVars$inputPh()),
                                      "No experimental data loaded!") )
                
                shiny::validate( need(!is.null(gVars$bmdlatestTable),
                                      "No BMD modelling performed!") )
                
                optimal_models_stats = gVars$bmdlatestTable
                time_var = colnames(gVars$inputPh()[[1]])[as.numeric(gVars$TPColID)]
                unique_times = unique(optimal_models_stats[,time_var])
                
                selectInput(inputId = "tPOD_accumulation_time", label = "Select timepoint",
                            choices = unique_times,
                            selected = unique_times[1])
              })
              
              output$tpod_accumulation_plot = renderPlotly({
                shiny::validate( need(!is.null(gVars$tPOD_plot_list), "No tPOD statistic computed"))
                
                experiment = input$tPOD_accumulation_experiment
                time = input$tPOD_accumulation_time
                
                p1 = gVars$tPOD_plot_list[[paste(experiment,"_",time,sep = "")]][["accumulaiton_plot"]]

                ggplotly(p1)
              }
              )
              
              
              output$tpod_density_plot = renderPlotly({
                shiny::validate( need(!is.null(gVars$tPOD_plot_list), "No tPOD statistic computed"))
                
                experiment = input$tPOD_accumulation_experiment
                time = input$tPOD_accumulation_time
                
                p1 = gVars$tPOD_plot_list[[paste(experiment,"_",time,sep = "")]][["density_plot"]]
                
                ggplotly(p1)
              }
              )

              output$tpod_statistics <- DT::renderDataTable({
                shiny::validate( need(!is.null(gVars$tpod_statistics), "No optimal model computed!"))

                experiment = input$tPOD_accumulation_experiment
                time = input$tPOD_accumulation_time
                
                df = gVars$tpod_statistics[[paste(experiment,"_",time,sep = "")]]
                rownames(df) = NULL
                
                DT::datatable(df, filter="top",
                              selection = 'single',
                              options = list(
                                search = list(regex=TRUE, caseInsensitive=FALSE),
                                scrollX=TRUE,
                                ordering=T,
                                rownames = FALSE
                              ))

              },server=TRUE)


# Module: Final Report

              output$exportRpt <- shiny::downloadHandler(
                filename = function(){
                  paste("BMDx_Analysis_Report_", Sys.Date(), '.html', sep = '')
                },
                content = function(con){

                  #start loading screen
                  shinyjs::html(id="loadingText", "CREATING ANALYSIS REPORT")
                  shinyjs::show(id="loading-content")

                  on.exit({
                    shinyjs::hide(id="loading-content", anim=TRUE, animType="fade")
                  })
                  #Disable Warning
                  oldw <- getOption("warn")
                  options(warn = -1)

                  tempReport <- file.path(tempdir(), "report.Rmd")
                  file.copy("report.Rmd", tempReport, overwrite=TRUE)

                  #params <- list(gVars=gVars, input=input)
                  params <- list(gVars=gVars, input=input, logVars=logVars, srcDir=srcDir)
                  rmarkdown::render(tempReport, output_file=con,
                                    params=params,
                                    envir=new.env(parent=globalenv())
                  )

                  #Enable Warning
                  options(warn = oldw)
                }
              )

# Module: Save Session

              output$save_session <- shiny::downloadHandler(
                filename = function(){
                  paste("BMDx2_session_", Sys.Date(),"_",Sys.time(), '.rds', sep='')
                },
                content = function(con){

                  #start loading screen
                  shinyjs::html(id="loadingText", "SAVING SESSION DATA")
                  shinyjs::show(id="loading-content")

                  on.exit({
                    shinyjs::hide(id="loading-content", anim=TRUE, animType="fade")
                  })

                  saved_gVars = reactiveValuesToList(gVars)
                  saved_logVars = reactiveValuesToList(logVars)
                  saved_input=reactiveValuesToList(input)

                  save(saved_gVars,saved_logVars,saved_input, file = con)
                }
              )

# Module: Upload Session

              observeEvent(input$upload_session, {

                shiny::validate(need(!is.null(input$upload_session), "No file selected!"))

                #start loading screen
                shinyjs::html(id="loadingText", "LOAD SESSION DATA")
                shinyjs::show(id="loading-content")

                on.exit({
                  shinyjs::hide(id="loading-content", anim=TRUE, animType="fade")
                })

                session_file = input$upload_session
                load(file = session_file$datapath)


                for(elem in  names(saved_gVars)){
                  gVars[[elem]]= saved_gVars[[elem]]
                }

                gVars$inputPh = eventReactive(input$upload_session,{
                  input_ph = saved_gVars$inputPh()
                  return(input_ph)
                })

                # gVars$phColTypes = reactive(saved_gVars$phColTypes)
                # gVars$phColChoices = reactive(saved_gVars$phColChoices)

                for(elem in  names(saved_logVars)){
                  logVars[[elem]]= saved_logVars[[elem]]
                }


                inputIDs      <- names(saved_input)
                inputvalues   <- unlist(saved_input)
                for (i in 1:length(saved_input)) {
                  session$sendInputMessage(inputIDs[i],  list(value=inputvalues[[i]]) )
                }

                if(!is.null(gVars$last_step_performed)){
                  switch(gVars$last_step_performed,
                         phenodata={
                           shinyBS::updateButton(session, "import_pheno_submit", style="success", icon=icon("check-circle"))
                           shinyBS::updateCollapse(session, "bsSidebar1", open="Load Experimental Data",
                                                   style=list("Load Phenotype Data"="success","Load Experimental Data"="warning"))
                         },
                         expression_data={
                           shinyBS::updateButton(session, "import_pheno_submit", style="success", icon=icon("check-circle"))
                           shinyBS::updateButton(session, "import_expr_submit", style="success", icon=icon("check-circle"))
                           shinyBS::updateCollapse(session, "bsSidebar1", open="Filtering",
                                                   style=list("Load Phenotype Data"="success","Load Experimental Data"="success","Filtering"="warning"))
                           shiny::updateTabsetPanel(session, "display",selected = "gExpTab")
                         },
                         filtering = {
                           if(gVars$was_anova_performed ==TRUE){
                             shinyBS::updateButton(session, "fc_filtering_button", style="success", icon=icon("check-circle"))
                             shinyBS::updateButton(session, "anova_filtering_button", style="success", icon=icon("check-circle"))
                             shinyBS::updateButton(session, "trend_filtering_button", style="warning", icon=icon("check-circle"))
                             shinyBS::updateButton(session, "skip_anova_filtering_button", style="warning", icon=icon("check-circle"))
                             shiny::updateTabsetPanel(session, "display",selected = "AnovaTab")
                           }

                           if(gVars$was_trend_test_performed == TRUE){
                             shinyBS::updateButton(session, "fc_filtering_button", style="success", icon=icon("check-circle"))
                             shinyBS::updateButton(session, "trend_filtering_button", style="success", icon=icon("check-circle"))
                             shinyBS::updateButton(session, "skip_anova_filtering_button", style="warning", icon=icon("check-circle"))
                             shinyBS::updateButton(session, "anova_filtering_button", style="warning", icon=icon("check-circle"))
                             shiny::updateTabsetPanel(session, "display",selected = "AnovaTab")
                           }

                           if(gVars$was_logFC_filtering_performed == TRUE){
                             shinyBS::updateButton(session, "fc_filtering_button", style="success", icon=icon("check-circle"))
                             shinyBS::updateButton(session, "anova_filtering_button", style="success", icon=icon("check-circle"))
                             shinyBS::updateButton(session, "trend_filtering_button", style="warning", icon=icon("check-circle"))
                             shinyBS::updateButton(session, "skip_anova_filtering_button", style="warning", icon=icon("check-circle"))
                             shiny::updateTabsetPanel(session, "display",selected = "AnovaTab")
                           }

                           if(is.null(gVars$was_filtered_skipped)){
                             shinyBS::updateButton(session, "anova_filtering_button", style="success", icon=icon("check-circle"))
                             shinyBS::updateButton(session, "skip_anova_filtering_button", style="success", icon=icon("check-circle"))
                             shinyBS::updateButton(session, "anova_filtering_button", style="warning", icon=icon("check-circle"))
                             shinyBS::updateButton(session, "trend_filtering_button", style="warning", icon=icon("check-circle"))
                           }

                           shinyBS::updateCollapse(session, "bsSidebar1", open="Compute BMD",
                                                   style=list("Load Phenotype Data"="success",
                                                              "Load Experimental Data"="success",
                                                              "Filtering"="success",
                                                              "Compute BMD"="warning"))
                           shiny::updateTabsetPanel(session, "display",selected = "BMDTab")


                         },
                         bmd = {
                           shinyBS::updateButton(session, "bmd_button", style="success", icon=icon("check-circle"))
                           shinyBS::updateCollapse(session, "bsSidebar1", open="FunMappOne",
                                                   style=list("Load Phenotype Data"="success",
                                                              "Load Experimental Data"="success",
                                                              "Filtering"="success",
                                                              "Compute BMD"="success",
                                                              "FunMappOne"="warning"))

                         },
                         funmappone = {
                           shinyBS::updateButton(session, "enrich_button", style="success", icon=icon("check-circle"))
                           shinyBS::updateCollapse(session, "bsSidebar1", open="Pairs Analysis",
                                                   style=list("Load Phenotype Data"="success",
                                                                         "Load Experimental Data"="success",
                                                                         "Filtering"="success",
                                                                         "Compute BMD"="success",
                                                                         "FunMappOne"="success",
                                                                         "Pairs Analysis"="warnings"))
                           shiny::updateTabsetPanel(session, "display",selected = "enrTab")

                         },
                         gene_pairs = {
                           shinyBS::updateButton(session, "gene_comparison_button", style="success", icon=icon("check-circle"))
                           shinyBS::updateCollapse(session, "bsSidebar1",
                                                   style=list("Load Phenotype Data"="success",
                                                              "Load Experimental Data"="success",
                                                              "Filtering"="success",
                                                              "Compute BMD"="success",
                                                              "FunMappOne"="success",
                                                              "Pairs Analysis"="success"))

                         }
                  )
                }

                # if(gVars$is_pheno_uploaded){
                #   shinyBS::updateButton(session, "import_pheno_submit", style="success", icon=icon("check-circle"))
                #   shinyBS::updateCollapse(session, "bsSidebar1", open="Load Experimental Data", style=list("Load Phenotype Data"="success","Load Experimental Data"="warning"))
                # }
                #
                # if(gVars$is_pheno_uploaded & gVars$is_gene_expression_uploaded){
                #   shinyBS::updateButton(session, "import_pheno_submit", style="success", icon=icon("check-circle"))
                #   shinyBS::updateButton(session, "import_expr_submit", style="success", icon=icon("check-circle"))
                #   shinyBS::updateCollapse(session, "bsSidebar1", open="Filtering", style=list("Load Phenotype Data"="success","Load Experimental Data"="success","Filtering"="warning"))
                #   shiny::updateTabsetPanel(session, "display",selected = "gExpTab")
                # }
                #
                # if(gVars$was_anova_performed ==TRUE){
                #   shinyBS::updateButton(session, "fc_filtering_button", style="success", icon=icon("check-circle"))
                #   shinyBS::updateButton(session, "anova_filtering_button", style="success", icon=icon("check-circle"))
                #   shinyBS::updateButton(session, "trend_filtering_button", style="warning", icon=icon("check-circle"))
                #   shinyBS::updateButton(session, "skip_anova_filtering_button", style="warning", icon=icon("check-circle"))
                #   shinyBS::updateCollapse(session, "bsSidebar1", open="Compute BMD", style=list("Filtering"="success","Compute BMD"="warning"))
                #   shiny::updateTabsetPanel(session, "display",selected = "AnovaTab")
                # }
                #
                # if(gVars$was_trend_test_performed == TRUE){
                #   shinyBS::updateButton(session, "fc_filtering_button", style="success", icon=icon("check-circle"))
                #   shinyBS::updateButton(session, "trend_filtering_button", style="success", icon=icon("check-circle"))
                #   shinyBS::updateButton(session, "skip_anova_filtering_button", style="warning", icon=icon("check-circle"))
                #   shinyBS::updateButton(session, "anova_filtering_button", style="warning", icon=icon("check-circle"))
                #   shinyBS::updateCollapse(session, "bsSidebar1", open="Compute BMD", style=list("Filtering"="success","Compute BMD"="warning"))
                #   shiny::updateTabsetPanel(session, "display",selected = "AnovaTab")
                # }
                #
                # if(gVars$was_logFC_filtering_performed == TRUE){
                #   shinyBS::updateButton(session, "fc_filtering_button", style="success", icon=icon("check-circle"))
                #   shinyBS::updateButton(session, "anova_filtering_button", style="success", icon=icon("check-circle"))
                #   shinyBS::updateButton(session, "trend_filtering_button", style="warning", icon=icon("check-circle"))
                #   shinyBS::updateButton(session, "skip_anova_filtering_button", style="warning", icon=icon("check-circle"))
                #   shinyBS::updateCollapse(session, "bsSidebar1", open="Compute BMD", style=list("Filtering"="success","Compute BMD"="warning"))
                #   shiny::updateTabsetPanel(session, "display",selected = "AnovaTab")
                # }
                #
                # if(is.null(gVars$was_filtered_skipped)){
                #   shinyBS::updateButton(session, "anova_filtering_button", style="success", icon=icon("check-circle"))
                #   shinyBS::updateButton(session, "skip_anova_filtering_button", style="success", icon=icon("check-circle"))
                #   shinyBS::updateButton(session, "anova_filtering_button", style="warning", icon=icon("check-circle"))
                #   shinyBS::updateButton(session, "trend_filtering_button", style="warning", icon=icon("check-circle"))
                #   shinyBS::updateCollapse(session, "bsSidebar1", open="Compute BMD", style=list("Filtering"="success","Compute BMD"="warning"))
                #
                #
                # }

              })

# Module: Manual

            #   output$markdown <- renderUI({
            #     HTML(markdown::markdownToHTML(knitr::knit('manual/manual.Rmd',quiet = T),
            #                                   options = c("toc",getOption("markdown.HTML.options")),
            #                                   title = "Manual"))
            # library(markdown)
            #     HTML(markdown::mark_html(knitr::knit('manual/manual.Rmd',quiet = T),
            #                                   options = c("+toc",markdown_options()[5:length(markdown_options())])))
            #
            #     HTML(markdown::mark(knitr::knit('manual/manual.Rmd',quiet = T), format = "html",options = c("+hardbreaks",
            #                                                                                      "+mathjax_embed",
            #                                                                                      "+tagfilter",
            #                                                                                      "+toc",
            #                                                                                      "+autolink",
            #                                                                                      "+base64_images",
            #                                                                                      "+highlight_code",
            #                                                                                      "+latex_math",
            #                                                                                      "+mathjax",
            #                                                                                      "+smart",
            #                                                                                      "+smartypants",
            #                                                                                      "+standalone",
            #                                                                                      "+strikethrough",
            #                                                                                      "+subscript",
            #                                                                                      "+superscript",
            #                                                                                      "+table" ,
            #                                                                                      "+tasklist")))
            #
            #
            #     includeHTML("manual/manual.html")
            #
            #
            #   })

})




