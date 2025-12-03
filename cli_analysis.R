library(lme4)
library(ggplot2)
library(cowplot)
library(crqa)
library(textstem)

source('functions/rqa/coreFunctions.R') # RQA functions to manage measurements
source('functions/lexical/coreFunctions.R') # cleaning lexical level/words

rt = ''

# setup folder to import conversations
setwd('REAL')
interlocs = list(folders=c('baseline','post'),labels=c('TBI','CP'),
                 g1=c('*TBI:'),
                 g2=c('*CP:'))

# initialize different results
allRes = c() # these tables correspond to the types of analysis in DyAD
all.crqa.res = c() # e.g., full cross recurrence
all.group1.res = c() # this would be the participant 
all.group2.res = c() # the control group
all.auto.res = c()
windowed.crqa = c() # cross recurrence between conversation partners
windowed.inv = c() # investigator / participant used for this code
windowed.part = c()
windowed.auto = c()
drps = c()

# use for baseline/shuffling! 
# we need these 2, set in the main loop, and see line 141 below for the shuffled
all_transcripts = list() 
transcript_index = 1

bins = 1 # how many splits we want to make on the text series for windowed crqa (# of windows)

for (f in interlocs$folders) {
  
  fls = list.files(paste(rt,f,sep=''))
  for (fl_name in fls) {
    print(fl_name)
    fl = paste(rt,f,'/',fl_name,sep='')
    
    sampleTrans = readChar(fl,nchars=file.info(fl)$size) # get the transcript
    sampleTrans = gsub("\r\n\t",' ',sampleTrans)
    sampleTrans = gsub("\r",'',sampleTrans) 
    sampleTrans = unlist(strsplit(sampleTrans,"\n"))
    temp.dat = data.frame(row=1:length(sampleTrans),id='',words='',stringsAsFactors=F) # let's handle the extra tabs and word wrapping of CHAT files
    
    for (i in 1:length(sampleTrans)) {
      d = unlist(strsplit(sampleTrans[i],"\t")) # split by the tab once we've unwrapped and such
      #print(d[1])
      temp.dat[i,]$id = d[1]
      temp.dat[i,]$words = d[2]
    }
    sampleTrans = temp.dat # get our transcript back
    sampleTrans = sampleTrans[grep("^\\*",sampleTrans$id),]
    print(sampleTrans) # watch progress, check clean
    
    for (i in 1:nrow(sampleTrans)) {
      sampleTrans[i,]$words = cleanText(sampleTrans[i,]$words,stem=T)
    }
    # store for shuffled baseline
    all_transcripts[[transcript_index]] = sampleTrans
    transcript_index = transcript_index + 1
    
    # let's make some codes, from words
    wdCode = unique(unlist(strsplit(paste(sampleTrans$words),' ')))
    
    full = assignWordCodes(sampleTrans$words,wdCode)
    l = length(full) # length of time series
    
    group1 = assignWordCodesDual(-1,sampleTrans$id %in% interlocs$g1,sampleTrans$words,wdCode)
    group2 = assignWordCodesDual(-2,sampleTrans$id %in% interlocs$g2,sampleTrans$words,wdCode)
    
    group1_off = group1; group1_off[group1==-1] = -2
    group2_off = group2; group2_off[group2==-2] = -1
    
    # aggregate rqa over these time series (since series are quite long, we could shorten)
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
    windowed.crqa = rbind(windowed.crqa,data.frame(win.crqa,cond=f,fl=fl_name,deets))
    
    win.part = wincrqa(group2_off,group2,windowsize=floor(l/6),windowstep=floor(l/6)-1,
                       embed=2,delay=1,radius=.001,
                       normalize=0,rescale=0,mindiagline=2,minvertline=2)
    # colnames(win.part) = c('RR','DET','NRLINE','maxL','L','ENTR','rENTR','LAM','TT','w') # reminder
    windowed.part = rbind(windowed.part,data.frame(win.part,cond=f,fl=fl_name,deets))
    
    win.inv = wincrqa(group1,group1_off,windowsize=floor(l/6),windowstep=floor(l/6)-1,
                      embed=2,delay=1,radius=.001,
                      normalize=0,rescale=0,mindiagline=2,minvertline=2)
    # colnames(win.inv) = c('RR','DET','NRLINE','maxL','L','ENTR','rENTR','LAM','TT','w') # reminder
    windowed.inv = rbind(windowed.inv,data.frame(win.inv,cond=f,fl=fl_name,deets))
    
    win.auto = wincrqa(full,full,windowsize=floor(l/6),windowstep=floor(l/6)-1,
                       embed=2,delay=1,radius=.001,
                       normalize=0,rescale=0,mindiagline=2,minvertline=2)
    # colnames(win.auto) = c('RR','DET','NRLINE','maxL','L','ENTR','rENTR','LAM','TT','w') # reminder
    windowed.auto = rbind(windowed.auto,data.frame(win.auto,cond=f,fl=fl_name,deets))
    
  }
}

# rename for convenience
windowed.auto$w = windowed.auto$win
windowed.crqa$w = windowed.crqa$win
windowed.part$w = windowed.part$win
windowed.inv$w = windowed.inv$win

windowed.auto[is.na(windowed.auto)] = 0 # if there were no points, then NA; so set to 0
windowed.crqa[is.na(windowed.crqa)] = 0
windowed.part[is.na(windowed.part)] = 0
windowed.inv[is.na(windowed.inv)] = 0

# now let's do the shuffled baseline with the data stored from above...
# note there are variables created in the for loop above
baseline.crqa = c()
for (i in 1:length(all_transcripts)) {
  
  # let's choose the i-th entry here
  target_i = all_transcripts[[i]]
  # then let's choose a random other!
  j = sample(setdiff(1:length(all_transcripts), i),1) # remove i from 1:length, sample another index
  other = all_transcripts[[j]] # get *that* transcript too
  
  # make sure wdCode works for all
  wdCode = unique(unlist(strsplit(paste(target_i$words, other$words),' ')))
  
  full = assignWordCodes(target_i$words,wdCode)
  l = length(full) # length of time series
  
  # let's use g1 from target and g2 from other; different people, but TBI & CP
  group1 = assignWordCodesDual(-1,target_i$id %in% interlocs$g1,target_i$words,wdCode)
  group2 = assignWordCodesDual(-2,other$id %in% interlocs$g2,other$words,wdCode)
  
  # OK, we have to align their lengths to use crqa... has to be square
  if (l < length(group2)) {
    group2 = group2[1:l]
  } else if (l > length(group2)) {
    group1 = group1[1:length(group2)]
  }
  
  # aggregate rqa over these time series (since series are quite long, we could shorten)
  res.crqa = crqa(group1,
                  group2,embed=2,delay=1,radius=.001,
                  normalize=0,rescale=0,mindiagline=2,minvertline=2)
  deets = data.frame(target_i=i, other_j=j, length=length(full),
                     length.group1=sum(group1>0),
                     length.group2=sum(group2>0),
                     word.types.group1=length(unique(group1))-1,
                     word.types=length(unique(full)),
                     word.types.group2=length(unique(group2))-1)
  baseline.crqa = rbind(baseline.crqa,data.frame(t(unlist(res.crqa[1:9])),cond=f,fl=fl_name,deets))  
  
}

# paired analysis
all.crqa.res$num = gsub('pre','',all.crqa.res$fl) # separate pre/post
all.crqa.res$num = gsub('post','',all.crqa.res$num)

table(all.crqa.res$num) # make sure there's two of each

pre = all.crqa.res[grep('pre',all.crqa.res$fl),]
colnames(pre) = paste('pre_',colnames(pre),sep='')

post = all.crqa.res[grep('post',all.crqa.res$fl),]
colnames(post) = paste('post_',colnames(post),sep='')

wide_format = cbind(pre,post)
cbind(pre$pre_num,post$post_num) # check alignment

t.test(pre$pre_RR-post$post_RR) # paired format
t.test(pre$pre_DET-post$post_DET)

t.test(pre$pre_DET,post$post_DET) 


