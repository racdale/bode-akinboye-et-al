
library(lme4)
library(ggplot2)
library(cowplot)

# FIRST: set R to source file location
# source functions for lexical level analysis
source('functions/rqa/coreFunctions.R')
source('functions/lexical/coreFunctions.R')
source('functions/syntactic/coreFunctions.R')

rt = ''

setwd('data/REAL')
interlocs = list(folders=c('g1','g1'),labels=c('Group 1','Group 1'),
                 g1=c('*LENO:'),
                 g2=c('*ALIN:'))

# setwd('data/Test')
# interlocs = list(folders=c('c1','c2'),labels=c('Group 1','Group 2'),
#                 g1=c('*P1:','*P3:'),
#                 g2=c('*P2:'))

# setwd('data/Coelho')
# interlocs = list(folders=c('N','TB'),labels=c('Investigator','Participant'),
#                  g1=c('*INV:','*INV1:','*INV2:','*INV3:','*INV4:'),
#                  g2=c('*PAR:'))

# setwd('data/Warren')
# interlocs = list(folders=c('Before 3','After 3'),labels=c('Parent','Child'),
#                 g1=c('*MOT:','*FAT:'),
#                 g2=c('*CHI:'))

allRes = c()
all.crqa.res = c()
all.group1.res = c()
all.group2.res = c()
all.auto.res = c()
windowed.crqa = c()
windowed.inv = c()
windowed.part = c()
windowed.auto = c()
drps = c()

bins = 1 # how many splits we want to make on the text series for windowed crqa (# of windows)

for (f in interlocs$folders) {
  
  fls = list.files(paste(rt,f,sep=''))
  for (fl_name in fls) {
    print(fl_name)
    fl = paste(rt,f,'/',fl_name,sep='')
    
    sampleTrans = readChar(fl,nchars=file.info(fl)$size) 
    sampleTrans = gsub("\r\n\t",' ',sampleTrans)
    sampleTrans = gsub("\r",'',sampleTrans) 
    sampleTrans = unlist(strsplit(sampleTrans,"\n"))
    temp.dat = data.frame(row=1:length(sampleTrans),id='',words='',stringsAsFactors=F) # let's handle the extra tabs and word wrapping of CHAT FILES!
    
    for (i in 1:length(sampleTrans)) {
      d = unlist(strsplit(sampleTrans[i],"\t")) # split by the tab once we've unwrapped and such
      #print(d[1])
      temp.dat[i,]$id = d[1]
      temp.dat[i,]$words = d[2]
    }
    sampleTrans = temp.dat # get our transcript back
    sampleTrans = sampleTrans[grep("^\\*",sampleTrans$id),]

    for (i in 1:nrow(sampleTrans)) {
      sampleTrans[i,]$words = cleanText(sampleTrans[i,]$words,stem=T)
      sampleTrans[i,]$words = paste(posAnnotation(sampleTrans[i,]$words),collapse=' ')
    }
    wdCode = unique(unlist(strsplit(paste(sampleTrans$words),' ')))
    
    full = assignWordCodes(sampleTrans$words,wdCode)
    l = length(full) # length of TS
    
    group1 = assignWordCodesDual(-1,sampleTrans$id %in% interlocs$g1,sampleTrans$words,wdCode)
    group2 = assignWordCodesDual(-2,sampleTrans$id %in% interlocs$g2,sampleTrans$words,wdCode)
    
    group1_off = group1; group1_off[group1==-1] = -2
    group2_off = group2; group2_off[group2==-2] = -1
    
    # drpfromts of these two series... show character alignment
    #drp = drpdfromts(investigator,participant,ws=500,datatype='categorical')
    #drps = rbind(drps,data.frame(Lag=-500:500,RR=drp$profile,cond=f,part=fl_name,shuff='original'))
    
    #drp_shuff = drpdfromts(sample(investigator,length(investigator)),sample(participant,length(participant)),ws=500,datatype='categorical')
    #drps = rbind(drps,data.frame(Lag=-500:500,RR=drp_shuff$profile,cond=f,part=fl_name,shuff='shuffled'))
    
    # aggregate rqa over these ts's (since ts's are quite long, we could shorten)
    res.crqa = crqa(group1,
                  group2,embed=2,delay=1,radius=.001,
                  normalize=0,rescale=0,mindiagline=2,minvertline=2)
    deets = data.frame(length=length(full),
                       length.group1=sum(group1>0),
                       length.group2=sum(group2>0),
                       word.types.group1=length(unique(group1))-1,
                       word.types=length(unique(full)),
                       word.types.group2=length(unique(group2))-1)
    all.crqa.res = rbind(all.crqa.res,data.frame(t(unlist(res.crqa[1:9])),cond=f,fl=fl_name,deets))
    
    res.group2 = crqa(group2,
                  group2_off,embed=2,delay=1,radius=.001,
                    normalize=0,rescale=0,mindiagline=2,minvertline=2)

    all.group2.res = rbind(all.group2.res,data.frame(t(unlist(res.group2[1:9])),cond=f,fl=fl_name,deets))

    res.group1 = crqa(group1,
                    group1_off,embed=2,delay=1,radius=.001,
                    normalize=0,rescale=0,mindiagline=2,minvertline=2)
    
    all.group1.res = rbind(all.group1.res,data.frame(t(unlist(res.group1[1:9])),cond=f,fl=fl_name,deets))

    auto.all = crqa(full,
                    full,embed=2,delay=1,radius=.001,
                    normalize=0,rescale=0,mindiagline=2,minvertline=2)
    
    all.auto.res = rbind(all.auto.res,data.frame(t(unlist(auto.all[1:9])),cond=f,fl=fl_name,deets))
    
    win.crqa = wincrqa(group1,group2,windowsize=floor(l/6),windowstep=floor(l/6)-1,
                       embed=2,delay=1,radius=.001,
                       normalize=0,rescale=0,mindiagline=2,minvertline=2)
    colnames(win.crqa) = c('RR','DET','NRLINE','maxL','L','ENTR','rENTR','LAM','TT','w')
    windowed.crqa = rbind(windowed.crqa,data.frame(win.crqa,cond=f,fl=fl_name,deets))
    
    win.part = wincrqa(group2_off,group2,windowsize=floor(l/6),windowstep=floor(l/6)-1,
                       embed=2,delay=1,radius=.001,
                       normalize=0,rescale=0,mindiagline=2,minvertline=2)
    colnames(win.part) = c('RR','DET','NRLINE','maxL','L','ENTR','rENTR','LAM','TT','w')
    windowed.part = rbind(windowed.part,data.frame(win.part,cond=f,fl=fl_name,deets))
    
    win.inv = wincrqa(group1,group1_off,windowsize=floor(l/6),windowstep=floor(l/6)-1,
                      embed=2,delay=1,radius=.001,
                      normalize=0,rescale=0,mindiagline=2,minvertline=2)
    colnames(win.inv) = c('RR','DET','NRLINE','maxL','L','ENTR','rENTR','LAM','TT','w')
    windowed.inv = rbind(windowed.inv,data.frame(win.inv,cond=f,fl=fl_name,deets))
    
    win.auto = wincrqa(full,full,windowsize=floor(l/6),windowstep=floor(l/6)-1,
                       embed=2,delay=1,radius=.001,
                       normalize=0,rescale=0,mindiagline=2,minvertline=2)
    colnames(win.auto) = c('RR','DET','NRLINE','maxL','L','ENTR','rENTR','LAM','TT','w')
    windowed.auto = rbind(windowed.auto,data.frame(win.auto,cond=f,fl=fl_name,deets))
    
        
  }
}

save(file='syntactic.overall.Rd',all.crqa.res,all.group2.res,all.group1.res,all.auto.res,interlocs,windowed.crqa,windowed.auto,windowed.part,windowed.inv)

load("syntactic.overall.Rd")



