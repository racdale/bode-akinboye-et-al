proc_stat_buttons = function(input,output,session) {
  observeEvent(input$run_model_button, {
    fl = paste0("analysis/data/",input$data.src,"/covariates.txt")
    load(paste0("analysis/data/",input$data.src,"/",input$data.level,".overall.Rd"))
    covars = read.csv(fl,sep='\t',header=T,stringsAsFactor=F)
    covars_subset = subset(covars,select=c(input$covar_checkboxes))
    source.dat = eval(parse(text=input$aggregate.source))    
    dv = subset(source.dat,select=c("fl",input$model.dv,"cond"))
    # 
    model_dat = c()
    if (nchar(input$omit.fs)>0) {
      omit.names = unlist(strsplit(input$omit.fs,','))
    } else {
      omit.names = c()
    }
    for (i in 1:nrow(dv)) {
      ix = which(covars$ID==dv$fl[i])
      if (length(ix)>0 & !(dv$fl[i] %in% omit.names)) {
        covar_this = covars_subset[ix,]
        model_dat = rbind(model_dat,data.frame(dv=dv[i,2],cond=dv[i,3],covar_this))
        #covars_subset[i,]$dv = dv[tolower(dv$fl)==tolower(paste0(covars$ID[i],'.cha',sep='')),2]
      }
    }
    colnames(model_dat) = c(input$model.dv,'Group',colnames(covars_subset))
    print(model_dat)
    coefs = lm(model_dat[,1]~.,data=model_dat[,2:ncol(model_dat)])
    #output$model.lmer = DT::renderDataTable({
    #  coefs
    #},options=list(pageLength=25,dom = 't'),selection='single')
    output$model.lmer = renderPrint({summary(coefs)})
  })
  
  observeEvent(input$run_win_model_button, {
    fl = paste0("analysis/data/",input$data.src,"/covariates.txt")
    load(paste0("analysis/data/",input$data.src,"/",input$data.level,".overall.Rd"))
    covars = read.csv(fl,sep='\t',header=T,stringsAsFactor=F)
    covars_subset = subset(covars,select=c(input$covar_win_checkboxes))
    source.dat = eval(parse(text=input$windowed.source))
    dv = subset(source.dat,select=c("fl",input$model.win.dv,"cond","w"))
    # 
    model_dat = c()
    if (nchar(input$omit.fs)>0) {
      omit.names = unlist(strsplit(input$omit.fs,','))
    } else {
      omit.names = c()
    }
    for (i in 1:nrow(dv)) {
      ix = which(covars$ID==dv$fl[i])
      if (length(ix)>0 & !(dv$fl[i] %in% omit.names)) {
        covar_this = covars_subset[ix,]
        model_dat = rbind(model_dat,data.frame(dv=dv[i,2],fl=dv$fl[i],cond=dv[i,3],w=dv[i,4],covar_this))
        #covars_subset[i,]$dv = dv[tolower(dv$fl)==tolower(paste0(covars$ID[i],'.cha',sep='')),2]
      }
    }
    colnames(model_dat) = c(input$model.dv,'fl','Group','w',colnames(covars_subset))
    print(covars_subset)
    if (length(covars_subset)>0) {
      covar_sums = paste(colnames(covars_subset),collapse='+')
      covar_sums = paste('(',covar_sums,')+',collapse='')
    } else {
      covar_sums = ''
    }
    print(covar_sums)
    covar_sums = paste(input$model.dv,'~',covar_sums,'w*Group+(1+w|fl)',collapse='')
    fml = formula(covar_sums)
    print(fml)
    lmo = lmer(fml,data=model_dat)
    coefs = data.frame(summary(lmo)$coefficients)
    coefs$p = 2*(1-pnorm(abs(coefs$t.value)))
    #output$model.lmer = DT::renderDataTable({
    #  coefs
    #},options=list(pageLength=25,dom = 't'),selection='single')
    output$model.win.lmer = renderPrint({coefs})
  })
}