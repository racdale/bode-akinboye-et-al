show_dialogue_tables = function(input,output,session,subdat) {
  output$data.table = DT::renderDataTable({    
    #datatable(subdat,options=list(pageLength=10),selection='single')
    update_prog_bar(session,100)
    fl = paste0("analysis/data/",input$data.src,"/covariates.txt")
    covars = read.csv(fl,sep='\t',header=T)
    return(covars)
    #return(subdat)
  },options=list(pageLength=10),selection='single')
  
  output$dyad.view = DT::renderDataTable({
    s = input$data.table_rows_selected
    if (length(s)) {
      dat = subdat[s,]
      fl = paste0('analysis/data/',input$data.src,'/',dat$Group,'/',dat$File)
      get.trans(fl)[,2:3]
    }
  },options=list(pageLength=25),selection='single')
  
  output$dyad.name = renderText({
    s = input$data.table_rows_selected
    if (length(s)) {
      dat = subdat[s,]
      fl = paste0('analysis/data/',input$data.src,'/',dat$Group,'/',dat$File)
      as.character(dat$File)
    }
  })
}