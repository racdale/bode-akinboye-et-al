library(shiny)
library(ggplot2)
library(DT)
library(ggpubr)
library(tm)
library(shinyjs)
library(lme4)
library(shinyWidgets)
library(shinyauthr)
library(dplyr)
#library(textstem)

source('ui.R')

source('server_functions/display_functions.R')
source('server_functions/processing_functions.R')
source('server_functions/make_box_plots.R') 
source('server_functions/show_windowed_rqa_plots.R')
source('server_functions/show_dialogue_tables.R')
source('server_functions/lmer_model_functions.R')

the.src = 'Coelho'

user_base <- tibble::tibble(
  user = c("REAL"),
  password = c("researchinmotion"),
  permissions = c("standard"),
  name = c("REAL")
)

##############################
# server
##############################
server = function(input,output,session){
  #str$data.src)
  
  shiny::observe({
    shiny::req(credentials()$user_auth)
    shinyjs::show(id = 'analysis')
  })
  
  credentials <- shinyauthr::loginServer(
    id = "login",
    data = user_base,
    user_col = user,
    pwd_col = password,
    log_out = reactive(logout_init())
  )
  
  # call the logout module with reactive trigger to hide/show
  logout_init <- shinyauthr::logoutServer(
    id = "logout",
    active = reactive(credentials()$user_auth)
  )
  
  resetResults = function(path,omit.fs='') {
    print(path)
    load(path)
    
    if (nchar(omit.fs)>0) {
      omit.names = unlist(strsplit(input$omit.fs,','))
      all.crqa.res = all.crqa.res[!(all.crqa.res$f %in% omit.names),]
      all.group2.res = all.group2.res[!(all.group2.res$f %in% omit.names),]
      all.group1.res = all.group1.res[!(all.group1.res$f %in% omit.names),]
      all.auto.res = all.auto.res[!(all.auto.res$f %in% omit.names),]
      windowed.crqa = windowed.crqa[!(windowed.crqa$f %in% omit.names),]
      windowed.auto = windowed.auto[!(windowed.auto$f %in% omit.names),]
      windowed.inv = windowed.inv[!(windowed.inv$f %in% omit.names),]
      windowed.part = windowed.part[!(windowed.part$f %in% omit.names),]
    }
    
    subdat = cbind(all.crqa.res[,c(11,10)],round(all.crqa.res[,c(12,1,2,6,5)],digits=2))
    colnames(subdat) = c('File','Group','Length (words)','Recurrence Rate','Determinism','Entropy','Average line length')
    
    make_box_plots(output,session,interlocs,all.crqa.res,all.group1.res,all.group2.res,all.auto.res)
    show_windowed_rqa_plots(input,output,session,windowed.crqa,windowed.part,windowed.inv,windowed.auto)
    show_dialogue_tables(input,output,session,subdat)
    proc_stat_buttons(input,output,session)
    
    output$model.ivs <- renderUI({
      fl = paste0("analysis/data/",input$data.src,"/covariates.txt")
      covars = read.csv(fl,sep='\t',header=T)
      if (ncol(covars)>=2) {
        choices = colnames(covars)[2:ncol(covars)]
        checkboxGroupInput("covar_checkboxes","Select covariates", choices = choices, selected = choices[1])
      }
    })
    
    output$model.win.ivs <- renderUI({
      fl = paste0("analysis/data/",input$data.src,"/covariates.txt")
      covars = read.csv(fl,sep='\t',header=T)
      if (ncol(covars)>=2) {
        choices = colnames(covars)[2:ncol(covars)]
        checkboxGroupInput("covar_win_checkboxes","Select covariates", choices = choices, selected = choices[1])
      }
    })
  }  
  
  observeEvent(input$data.src, {
    resetResults(paste0("analysis/data/",input$data.src,"/",input$data.level,".overall.Rd"))
    #input$omit.fs = ''
    updateTextInput(session,inputId='omit.fs',value='')
  })

  observeEvent(input$data.level, {
    resetResults(paste0("analysis/data/",input$data.src,"/",input$data.level,".overall.Rd"))
    #input$omit.fs = ''
    updateTextInput(session,inputId='omit.fs',value='')
  })
  
  observeEvent(input$covar_checkboxes, {
  })
  
  observeEvent(input$omit.button, {
    resetResults(paste0("analysis/data/",input$data.src,"/",input$data.level,".overall.Rd"),omit.fs=input$omit.fs)
  })
  observeEvent(input$hide_button, {
    shinyjs::toggle(id="data_table")
  })
  
}

# shinyApp(server=server,ui=ui)
shinyServer(server)



