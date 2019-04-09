The app is hosted on [https://kons.shinyapps.io/trees/](https://kons.shinyapps.io/trees/).

Requirements: 
- R 3.5.1
- shiny 1.2.0 
- ggplot2 3.1.0
- dplyr 0.8.0.1
- tidyr 0.8.3
- circlize 0.4.5
- DT 0.5

To download and run locally:
```
git clone https://github.com/henssenlab/TreeShapedRearrangementsShinyApp.git
cd TreeShapedRearrangementsShinyApp
Rscript -e 'shiny::runApp("app.R", launch.browser=TRUE)'
```
