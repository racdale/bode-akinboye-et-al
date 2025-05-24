plot.boxdot = function(dat,the.aes,col,ylab,title,session,prog.val) {
  title.col = rgb(t(col2rgb(col)/255*0.5))
  title.format = theme(
    plot.title = element_text(color=title.col,hjust=0)
  )
  gp = ggplot(dat,the.aes)+geom_boxplot(fill=col,alpha=0.5)+ggtitle(title)+title.format+
    geom_dotplot(binaxis='y',stackdir='center',dotsize = .5,fill=col)+stat_compare_means(aes(label = paste0("p = ")),method='wilcox.test')+
    ylab(ylab)+theme(axis.text = element_text(size = 11),axis.title=element_text(size=11))
  updateProgressBar(
    session = session,
    id = "pb1",
    value = prog.val
  )
  return(gp)
}

update_prog_bar = function(session,prog.val) {
  updateProgressBar(
    session = session,
    id = "pb1",
    value = prog.val
  )
}

get.trans = function(fl_path) {
  sampleTrans = readChar(fl_path,nchars=file.info(fl_path)$size)#read.csv(fl_path,header=F,sep="\t",stringsAsFactors=F)
  sampleTrans = gsub("\r\n\t",' ',sampleTrans)
  sampleTrans = gsub("\r",'',sampleTrans) 
  sampleTrans = unlist(strsplit(sampleTrans,"\n"))
  temp.dat = data.frame(row=1:length(sampleTrans),id='',words='',stringsAsFactors=F)
  for (i in 1:length(sampleTrans)) {
    d = unlist(strsplit(sampleTrans[i],"\t"))
    #print(d[1])
    temp.dat[i,]$id = d[1]
    temp.dat[i,]$words = d[2]
  }
  sampleTrans = temp.dat
  sampleTrans = sampleTrans[grep("^\\*",sampleTrans$id),]
  for (i in 1:nrow(sampleTrans)) {
    sampleTrans[i,]$words = cleanText(sampleTrans[i,]$words,stem=F)
  }
  return(sampleTrans)
}

