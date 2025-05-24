#ui = fluidPage(
#    plotOutput('RPout',width = "850px", height = "1100px")
#)
library(shinyWidgets)
library(dplyr)

source(file='uis/globals.R',local = TRUE)

ui <- fluidPage(
  shinyjs::useShinyjs(),
  
  shinyauthr::loginUI(id = "login"),
  
  div(
    id = "analysis",
    fluidRow(
      column(12,
             div(style='display:inline-block;',h1('Dynamic Analysis of Discourse (DyAD)')),
             progressBar(id = "pb1", value = 0,display_pct=T)
      )),
    source(file='uis/header_form.R',local = TRUE)$value,
    fluidRow(
      column(12,
        tabsetPanel(type="tabs",
          source(file='uis/data_information.R',local = TRUE)$value,        
          source(file='uis/static_analyses.R',local = TRUE)$value,        
          source(file='uis/dynamic_analyses.R',local = TRUE)$value,  
          source(file='uis/windowed_analyses.R',local = TRUE)$value,  
          source(file='uis/statistical_models.R',local = TRUE)$value
        )
      )
    )
  ) %>% shinyjs::hidden()
)

