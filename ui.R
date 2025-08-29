suppressMessages(library(shiny))
suppressMessages(library(shinyjs))
suppressMessages(library(shinyBS))
suppressMessages(library(shinydashboard))
suppressMessages(library(shinyFiles))
suppressMessages(library(DT))
suppressMessages(library(rhandsontable))
suppressMessages(library(shinycssloaders))

library(plotly)
library(xtable)
library(shinyhelper)
library(networkD3)

source("ui_functions/my_sidebar.R")
source("ui_functions/dashbord_body_analysis_parameters.R")
source("ui_functions/dashbord_body_results.R")

appCSS <- "
//.modal-lg {
//width: 95%;
//}
//.main-header { z-index: 100000; }
.main-sidebar { background-color: white !important;}
.sidebar { color: black; max-height: 900px; overflow-y: scroll; }
.sidebar a { color: blue; margin: 20px !important;}
.content-wrapper { margin-left: 15%; }
//.panel { background-color: #222d32; }
.panel-title a { font-weight: bold; color: white !important; }
.panel-warning .panel-heading { background-color: #00c0ef; }
.panel-warning { border-color: #8de9ff; }
.panel-sample .panel-heading { background-color: #2C6C99; }
.panel-sample { border-color: #2C6C99; }
.panel-danger .panel-heading { background-color: #dd4b39; }
.panel-success .panel-heading { background-color: #00a65a; }
.panel-info .panel-heading { background-color: #7e46ff; }
.panel-primary .panel-heading { background-color: #3079ae; }
.multicol { 
height: 300px;
-webkit-column-count: 4; /* Chrome, Safari, Opera */ 
-moz-column-count: 4;    /* Firefox */ 
column-count: 4; 
-moz-column-fill: auto;
-column-fill: auto;
} 
#loading-content {
position: absolute;
background: white !important;
opacity: 0.8;
z-index: 1000000;
left: 0;
right: 0;
top: 0;
bottom: 0;
font-size: 50px;
text-align: center;
color: #black;
}
#loading-gif { 
opacity: 0.8; 
display: block;
margin-left: auto;
margin-right: auto;
vertical-align: middle;
z-index: 1000000;
}

"

jsCode <- "
callback = 'function(table) {
table.on('click.dt', 'tr', function() {
table.$('tr.selected').removeClass('selected');
$(this).toggleClass('selected');            
Shiny.onInputChange('rows',
table.rows('.selected').data()[0][0]);
});
}'
"

fluidPage(
  useShinyjs(),
  inlineCSS(appCSS),
  hidden(div(id="loading-content",
             img(id="loading-gif", src="screen-loading.gif"),
             p(id="loadingText", "WORKING"),
             p("...")
  )),
  tags$head(tags$style(HTML("
                               body {
                                  width: 100% !important;
                                  max-width: 100% !important;
                               }
                               "))),
  dashboardPage(
    dashboardHeader(title="BMDx: dose response analysis for expression data", titleWidth="35%"),
    dashboardSidebar(disable=FALSE,
                     bsCollapse(id="bsSidebar1", open="Set Mode",
                                set_mode_panel(id = "Set Mode", style = "danger"),
                                load_phenodata_panel(id = "Load Phenotype Data", style = "danger"),
                                load_expression_panel(id="Load Experimental Data", style = "danger"),
                                gene_filtering_panel(id="Filtering", style = "danger"),
                                compute_bmd_panel(id="Compute BMD", style = "danger"),
                                enrichment_panel(id="FunMappOne", style = "danger"),
                                aop_ke_analysis(id="AOP", style = "danger"),
                                gene_comparison(id="Pairs Analysis", style = "danger")
                     ),
                     bsCollapse(id="bsSidebar0", open="More Info",
                                download_panel(id = "Download Report", style = "sample"),
                                info_panel(id="More Info", style= "sample")
                     )
                     # bsCollapse(id = "bsSidebar3", open = "Manage session",
                     #      bsCollapsePanel("Save Session",style="sample",
                     #          fluidRow(
                     #            column(12, align="center",
                     #                   downloadButton(outputId = "save_session", label = "Save session"),
                     #            )
                     #          )            
                     #      ),
                     #      bsCollapsePanel("Upload Session",style="sample",
                     #          fluidRow(
                     #            column(12, align="center",
                     #                   fileInput("upload_session", "Upload session")
                     #            )
                     #          )            
                     #      )
                     # )
    ),
    dashboardBody(
      
      ### diplaying shiny bsModal dashbords for parameter selection
      import_pheno_parameters(),
      import_gene_expression_parameters(),
      compute_anova_parameters(),
      compute_trend_parameters(),
      compute_fc_parameters(),
      compute_bmd_parameters(),
      funmappone_parameters(),
      compare_gene_pairs_parameters(),
      ke_enrichment_parameters(),
      
      # UI for results
      fluidRow(column(12,
              tabBox(id="display", title="", width=12,
                     tabPanel(value="pdTab", title="Metadata Data",
                        fluidRow(uiOutput("select_experiment_pheno")),   
                        fluidRow(
                          tags$h4("Distribution of number of samples in the phenodata for each relevant variable", 
                                  style = "text-align: center;")
                        ),
                        fluidRow(column(12, plotOutput("pheno_barplot"))),
                        fluidRow(column(12,DT::dataTableOutput("phenoDT")))
                    ),
                     tabPanel(value="gExpTab", title="Experimental Data",
                        fluidRow(
                          column(6,uiOutput("select_experiment_gExpMat")),
                          column(6,uiOutput("mds_colors"))
                        ),  
                        fluidRow(column(12, plotOutput("expression_mds"))),
                        fluidRow(column(12,DT::dataTableOutput("gExpMat")))
                     ),
                     filtering_results(),
                     bmd_results(), #end tabpanel
                     funmappone_results(),
                     aop_results(),
                     gene_comparison_results(),
                     tPOD_results()
                    
                     # bmdx_manual_tab() 
                    


              )
      ))
    )
  )
)

