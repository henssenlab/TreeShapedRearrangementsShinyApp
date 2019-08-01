[![DOI](https://zenodo.org/badge/180385829.svg)](https://zenodo.org/badge/latestdoi/180385829)

You can run this app online to explore own data and the data from the paper at [https://kons.shinyapps.io/trees/](https://kons.shinyapps.io/trees/). 


Download and run locally:
```
git clone https://github.com/henssenlab/TreeShapedRearrangementsShinyApp.git
cd TreeShapedRearrangementsShinyApp
Rscript -e 'shiny::runApp("app.R", launch.browser=TRUE)'
```

Requirements:
- R 3.5.1
- shiny 1.2.0 
- ggplot2 3.1.0
- dplyr 0.8.0.1
- tidyr 0.8.3
- circlize 0.4.5
- DT 0.5


Contact: henssenlab@gmail.com
