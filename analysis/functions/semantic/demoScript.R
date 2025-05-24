setwd('~/Dropbox/new.projects/TBI\ politis\ project/coelho-analysis/Final\ Scripts/src/sample-data')

# load in the data
sampleTrans = read.csv('tb08.chat.txt',header=F,sep="\t",stringsAsFactors=F)
colnames(sampleTrans) = c('id','words')
sampleTrans[1,]

source('../rqa/coreFunctions.R')
source('../lexical/coreFunctions.R')

for (i in 1:nrow(sampleTrans)) {
  sampleTrans[i,]$words = cleanText(sampleTrans[i,]$words,stem=F)
}

