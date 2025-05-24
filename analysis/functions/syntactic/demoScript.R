setwd('~/Dropbox/new.projects/TBI\ politis\ project/coelho-analysis/Final\ Scripts/src/sample-data')

# load in the data
sampleTrans = read.csv('tb08.chat.txt',header=F,sep="\t",stringsAsFactors=F)
colnames(sampleTrans) = c('id','words')
sampleTrans[1,]

source('../rqa/coreFunctions.R')
source('../lexical/coreFunctions.R')
source('../syntactic/coreFunctions.R')

sampleTrans$annot = ''
for (i in 1:nrow(sampleTrans)) {
  print(i)
  sampleTrans[i,]$words = paste(cleanText(sampleTrans[i,]$words,stem=F),'.') # ensure period to end sentence
  sampleTrans[i,]$annot = paste(posAnnotation(sampleTrans[i,]$words),collapse=' ')
}

# now that we have syntactic codes, we can use the lexical library
allPOS = paste(sampleTrans$annot,collapse=' ')
posCodes = unique(unlist(strsplit(allPOS, ' ')))

participant = c()
investigator = c()
for (i in 1:nrow(sampleTrans)) {
  if (sampleTrans[i,]$id=='*PAR:') {
    participant = c(participant,assignWordCodes(sampleTrans[i,]$annot,posCodes))
    investigator = c(investigator,0*assignWordCodes(sampleTrans[i,]$annot,posCodes)-1)
  } else {
    investigator = c(investigator,assignWordCodes(sampleTrans[i,]$annot,posCodes))
    participant = c(participant,0*assignWordCodes(sampleTrans[i,]$annot,posCodes)-2)
  }
}

drp = drpdfromts(investigator,participant,ws=2000,datatype='categorical')
plot(drp$profile,type='l')

rqaRes = crqa(investigator[1:1000],
              participant[1:1000],embed=2,delay=1,radius=.001,
              normalize=0,rescale=0,mindiagline=2,minvertline=2)

# let's do autorecurrence using participant time series
rqaResAuto = crqa(investigator[1:1000],
                  investigator[1:1000],embed=2,delay=1,radius=.001,
                  normalize=0,rescale=0,mindiagline=2,minvertline=2)











  