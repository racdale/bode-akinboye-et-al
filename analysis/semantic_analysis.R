
library(lme4)
library(ggplot2)
library(cowplot)
library(textstem) 

# FIRST: set R to source file location
# source functions for lexical level analysis
source('functions/rqa/coreFunctions.R')
source('functions/lexical/coreFunctions.R')
source('functions/semantic/coreFunctions.R')

rt = ''

setwd('data/REAL')
interlocs = list(folders=c('baseline','post'),labels=c('Baseline','Post'),
                 g1=c('*TBI:'),
                 g2=c('*CP:'))

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
    }
    # let's make some codes, from words
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
    crqa.RP = semanticRP(group1,group2)
    res.crqa = crqa(crqa.RP,recpt=T,
                  embed=2,delay=1,radius=.001,
                  normalize=0,rescale=0,mindiagline=2,minvertline=2)
    deets = data.frame(length=length(full),
                       length.group1=sum(group1>0),
                       length.group2=sum(group2>0),
                       word.types.group1=length(unique(group1))-1,
                       word.types=length(unique(full)),
                       word.types.group2=length(unique(group2))-1)
    all.crqa.res = rbind(all.crqa.res,data.frame(t(unlist(res.crqa[1:9])),cond=f,fl=fl_name,deets))
    
    group2.RP = semanticRP(group2,group2_off)
    res.group2 = crqa(group2.RP,recpt=T,
                  embed=2,delay=1,radius=.001,
                    normalize=0,rescale=0,mindiagline=2,minvertline=2)
    all.group2.res = rbind(all.group2.res,data.frame(t(unlist(res.group2[1:9])),cond=f,fl=fl_name,deets))

    group1.RP = semanticRP(group1,group1_off)
    res.group1 = crqa(group1.RP,recpt=T,
                    embed=2,delay=1,radius=.001,
                    normalize=0,rescale=0,mindiagline=2,minvertline=2)
    all.group1.res = rbind(all.group1.res,data.frame(t(unlist(res.group1[1:9])),cond=f,fl=fl_name,deets))

    # for full, we have to force turns based on prior groupings
    t1 = c(-10,group1) # so we get first turn
    t2 = c(-10,group2)
    t1_turns = which(diff(t1>0)>0)+1
    t2_turns = which(diff(t2>0)>0)+1
    turnForce = sort(unique(c(t1_turns,t2_turns))) 
    
    auto.RP = semanticRP(full,full,turnForce=turnForce)
    auto.all = crqa(auto.RP,recpt=T,
                    embed=2,delay=1,radius=.001,
                    normalize=0,rescale=0,mindiagline=2,minvertline=2)
    all.auto.res = rbind(all.auto.res,data.frame(t(unlist(auto.all[1:9])),cond=f,fl=fl_name,deets))
    
    win.crqa = semanticWindowed(res.crqa$RP,window.size=floor(l/6),window.shift=floor(l/6)-1)
    win.part = semanticWindowed(res.group1$RP,window.size=floor(l/6),window.shift=floor(l/6)-1)
    win.inv = semanticWindowed(res.group2$RP,window.size=floor(l/6),window.shift=floor(l/6)-1)
    win.auto = semanticWindowed(auto.all$RP,window.size=floor(l/6),window.shift=floor(l/6)-1)

        
    #win.res = wincrqa(group1,group2,windowsize=floor(l/6),windowstep=floor(l/6)-1,
    #                 embed=2,delay=1,radius=.001,
    #                 normalize=0,rescale=0,mindiagline=2,minvertline=2)$crqwin
    #colnames(win.res) = c('RR','DET','NRLINE','maxL','L','ENTR','rENTR','LAM','TT','w')
    windowed.crqa = rbind(windowed.crqa,data.frame(win.crqa,cond=f,fl=fl_name,deets))
    windowed.part = rbind(windowed.part,data.frame(win.part,cond=f,fl=fl_name,deets))
    windowed.inv = rbind(windowed.inv,data.frame(win.inv,cond=f,fl=fl_name,deets))
    windowed.auto = rbind(windowed.auto,data.frame(win.auto,cond=f,fl=fl_name,deets))
    
  }
}

save(file='semantic.overall.Rd',all.crqa.res,all.group2.res,all.group1.res,all.auto.res,interlocs,windowed.crqa,windowed.auto,windowed.part,windowed.inv)

load("semantic.overall.Rd")


