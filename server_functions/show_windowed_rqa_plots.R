updateLmerTable = function(output,coefs,group) {
  if (group=='inv') {
    output$win.inv.lmer = DT::renderDataTable({
      round(coefs,4)
    },options=list(pageLength=25,dom = 't'),selection='single')
  }
  else if (group=='part') {
    output$win.par.lmer = DT::renderDataTable({
      round(coefs,4)
    },options=list(pageLength=25,dom = 't'),selection='single')
  }
  else if (group=='crqa') {
    output$win.crqa.lmer = DT::renderDataTable({
      round(coefs,4)
    },options=list(pageLength=25,dom = 't'),selection='single')
  }
  else if (group=='auto') {
    output$win.auto.lmer = DT::renderDataTable({
      round(coefs,4)
    },options=list(pageLength=25,dom = 't'),selection='single')
  }
}

show_windowed_rqa_plots = function(input,output,session,windowed.crqa,windowed.part,windowed.inv,windowed.auto) {

  coefs = c()
  output$win.par.plots = renderPlot({
    
    #subdat = windowed.part[windowed.part$cond==interlocs$folders[1],]
    subdat = windowed.part
    ix = as.numeric(input$windowed.dv)
    ylab = colnames(windowed.part)[ix]
    
    windowed.part$dv = windowed.part[,ix]
    windowed.part$wc = windowed.part$w - mean(windowed.part$w)
    #coefs <<- summary(lm(dv~wc*as.factor(windowed.crqa$cond)))
    ixes = which(!is.na(windowed.part$dv)) # omit any non-defined
    #
    # NOTE: MUST INCLUDE A NOTE ABOUT THE # OF NA IN A MODEL IN THE INTERFACE FOR RESEARCHER
    #
    lmo = lmer(dv~wc*as.factor(cond)+(1+wc|fl),data=windowed.part[ixes,])
    coefs = data.frame(summary(lmo)$coefficients)
    coefs$p = 2*(1-pnorm(abs(coefs$t.value)))
    row.names(coefs) = list('Intercept','Time','Group','Group x Time')
    updateLmerTable(output,coefs,'part')
    update_prog_bar(session,20)
    ggplot(subdat,aes(x=w,y=subdat[,ix],color=cond))+geom_smooth()+ggtitle('Participant - RQA')+ylab(ylab)+xlab('Conversational window (1/6th)')
    
  })    
  
  output$win.inv.plots = renderPlot({
    #subdat = windowed.inv[windowed.inv$cond==interlocs$folders[1],]
    subdat = windowed.inv
    ix = as.numeric(input$windowed.dv)
    ylab = colnames(windowed.inv)[ix]
    
    windowed.inv$dv = windowed.inv[,ix]
    windowed.inv$wc = windowed.inv$w - mean(windowed.inv$w)
    #coefs <<- summary(lm(dv~wc*as.factor(windowed.crqa$cond)))
    ixes = which(!is.na(windowed.inv$dv)) # omit any non-defined
    #
    # NOTE: MUST INCLUDE A NOTE ABOUT THE # OF NA IN A MODEL IN THE INTERFACE FOR RESEARCHER
    #
    lmo = lmer(dv~wc*as.factor(cond)+(1+wc|fl),data=windowed.inv[ixes,])
    coefs = data.frame(summary(lmo)$coefficients)
    coefs$p = 2*(1-pnorm(abs(coefs$t.value)))
    row.names(coefs) = list('Intercept','Time','Group','Group x Time')
    updateLmerTable(output,coefs,'inv')
    update_prog_bar(session,55)
    ggplot(subdat,aes(x=w,y=subdat[,ix],color=cond))+geom_smooth()+ggtitle('Investigator - RQA')+ylab(ylab)+xlab('Conversational window (1/6th)')
    
  })    
  
  output$win.auto.plots = renderPlot({
    #subdat = windowed.auto[windowed.auto$cond==interlocs$folders[1],]
    subdat = windowed.auto
    ix = as.numeric(input$windowed.dv)
    ylab = colnames(windowed.auto)[ix]
    
    windowed.auto$dv = windowed.auto[,ix]
    windowed.auto$wc = windowed.auto$w - mean(windowed.auto$w)
    #coefs <<- summary(lm(dv~wc*as.factor(windowed.crqa$cond)))
    ixes = which(!is.na(windowed.auto$dv)) # omit any non-defined
    #
    # NOTE: MUST INCLUDE A NOTE ABOUT THE # OF NA IN A MODEL IN THE INTERFACE FOR RESEARCHER
    #
    lmo = lmer(dv~wc*as.factor(cond)+(1+wc|fl),data=windowed.auto[ixes,])
    coefs = data.frame(summary(lmo)$coefficients)
    coefs$p = 2*(1-pnorm(abs(coefs$t.value)))
    row.names(coefs) = list('Intercept','Time','Group','Group x Time')
    updateLmerTable(output,coefs,'auto')
    update_prog_bar(session,75)
    ggplot(subdat,aes(x=w,y=subdat[,ix],color=cond))+geom_smooth()+ggtitle('Dyad - RQA')+ylab(ylab)+xlab('Conversational window (1/6th)')
    
  })    
  
  output$win.crqa.plots = renderPlot({
    #subdat = windowed.crqa[windowed.crqa$cond==interlocs$folders[1],]
    subdat = windowed.crqa
    ix = as.numeric(input$windowed.dv)
    ylab = colnames(windowed.crqa)[ix]
    
    windowed.crqa$dv = windowed.crqa[,ix]
    windowed.crqa$wc = windowed.crqa$w - mean(windowed.crqa$w)
    #coefs <<- summary(lm(dv~wc*as.factor(windowed.crqa$cond)))
    ixes = which(!is.na(windowed.crqa$dv)) # omit any non-defined
    #
    # NOTE: MUST INCLUDE A NOTE ABOUT THE # OF NA IN A MODEL IN THE INTERFACE FOR RESEARCHER
    #
    lmo = lmer(dv~wc*as.factor(cond)+(1+wc|fl),data=windowed.crqa[ixes,])
    coefs = data.frame(summary(lmo)$coefficients)
    coefs$p = 2*(1-pnorm(abs(coefs$t.value)))
    row.names(coefs) = list('Intercept','Time','Group','Group x Time')
    updateLmerTable(output,coefs,'crqa')
    update_prog_bar(session,100)
    ggplot(subdat,aes(x=w,y=subdat[,ix],color=cond))+geom_smooth()+ggtitle('Dyad - CRQA')+ylab(ylab)+xlab('Conversational window (1/6th)')
  })    
  
}