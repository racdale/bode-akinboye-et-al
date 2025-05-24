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

# let's make some codes, from words
charCode = unique(unlist(strsplit(paste(sampleTrans$words),'')))

# if we want just simple char identifier sequences
# charTS = unlist(lapply(sampleTrans$words,function(x) {
#  return(assignCharCodes(x,charCode))
# }))

# a bigger function to convert whole turn list based on needs:
investigator = assignCharCodesDual(-1,sampleTrans$id=='*INV:',sampleTrans$words,charCode)
participant = assignCharCodesDual(-2,sampleTrans$id=='*PAR:',sampleTrans$words,charCode)

# drpfromts of these two series... show character alignment
drp = drpdfromts(investigator,participant,ws=2000,datatype='categorical')
plot(drp$profile,type='l')

# aggregate rqa over these ts's (note shorter length -- since ts's are quite long, we shorten)
rqaRes = crqa(investigator[1:1000],
              participant[1:1000],embed=2,delay=1,radius=.001,
              normalize=0,rescale=0,mindiagline=2,minvertline=2)

# let's do autorecurrence using participant time series
rqaResAuto = crqa(investigator[1:1000],
              investigator[1:1000],embed=2,delay=1,radius=.001,
              normalize=0,rescale=0,mindiagline=2,minvertline=2)

# let's run autorecurrence on full string, assuming it's one time series
fullTS = unlist(lapply(sampleTrans$words,function(x) {
  assignCharCodes(x,charCode)  
}))
rqaResAuto = crqa(fullTS[1:1000],
                  fullTS[1:1000],embed=2,delay=1,radius=.001,
                  normalize=0,rescale=0,mindiagline=2,minvertline=2)

# windowed rqa on the ts's
winRes = wincrqa(investigator,participant,windowsize=2000,windowstep=500,
                 embed=2,delay=1,radius=.001,
                 normalize=0,rescale=0,mindiagline=2,minvertline=2)
plot(winRes$crqwin[,6],type='b') # cross recurrence rate (RR) over interaction

# some traditional measures
targetPar = '*PAR:'
sampleTrans$uniqWords = -1 # initialize new data column
sampleTrans$totalWords = -1
sampleTrans$turns = -1

parIxes = sampleTrans$id==targetPar

allWords = sampleTrans$words[parIxes]
sampleTrans$uniqWords[parIxes] = length(unique(unlist(strsplit(paste(allWords), ' '))))
sampleTrans$totalWords[parIxes] = length(unlist(strsplit(paste(allWords), ' ')))
sampleTrans$turns[parIxes] = sum(sampleTrans$id==targetPar)
# now get the uniq words for non participant
allWords = sampleTrans$words[!parIxes]
sampleTrans$uniqWords[!parIxes] = length(unique(unlist(strsplit(paste(allWords), ' '))))
sampleTrans$totalWords[!parIxes] = length(unlist(strsplit(paste(allWords), ' ')))
sampleTrans$turns[!parIxes] = sum(sampleTrans$id!=targetPar)

sampleTrans$wordsPerTurn = sampleTrans$totalWords/sampleTrans$turns
sampleTrans$wordsPerTurnUniq = sampleTrans$uniqWords/sampleTrans$turns



