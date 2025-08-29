library(InteractiveComplexHeatmap)
library(ComplexHeatmap)
shiny::runApp(
	appDir="/srv/shiny-server/bmdx2/", 
	port=3838, 
	launch.browser=FALSE, 
	host="0.0.0.0"
)
