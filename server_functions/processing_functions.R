### core functions for performing lexical-level analysis of transcripts structured in the manner described in the GitHub repository

require(tm)
# https://cran.r-project.org/web/packages/tm/tm.pdf
require(SnowballC)
# https://cran.r-project.org/web/packages/SnowballC/SnowballC.pdf
require(textstem)

removepunct = function(x) { return(gsub("[[:punct:]]","",x)) }
doublespace = function(x) { return(gsub("  "," ",x)) }

cleanText = function(rawText,stem=F,removeStopwords=F) { # assorted cleaning; can add when new items need to be elided from transcripts
	rawText = gsub("\\\025\\d*_\\d*\\\025",'',rawText) # clear CHAT numbers (audio time indices)
	rawText = tolower(rawText)
	rawText = gsub('_',' ',rawText)
	rawText = gsub('xxx','',rawText) # omit unintelligible
	if (stem) {
		#rawText = wordStem(unlist(strsplit(rawText,' ')),language='english')
	  rawText = lemmatize_words(unlist(strsplit(rawText,' ')))
		rawText = paste(rawText,collapse=' ')
	}
	rawText = Corpus(VectorSource(rawText))    
	# eliminate extra whitespace; requires tm
	if (removeStopwords) { #******
	  #rawText = tm_map(rawText,stopwords)
	}
	rawText = tm_map(rawText, stripWhitespace)
	rawText = tm_map(rawText, removepunct)
	rawText = tm_map(rawText, doublespace)[[1]]
	rawText = gsub("^ ",'',rawText)
	rawText = gsub(" $",'',rawText)
	
	return(PlainTextDocument(rawText)$content) # purge the text content from the tm Corpus
}

### assign a code with an existing list of characters (indices used)
assignCharCodes = function(cleanTurn,charCode) {
	series = c()
	for (charct in unlist(strsplit(cleanTurn,''))) {
		series = c(series, which(charct==charCode))
	}
	return(series)
}

### assign a code with an existing list of characters (indices used)
### and zero out according to zeroCode
### targetCategory = list of T / F if category is satisfied, aligned with charCode
assignCharCodesDual = function(zeroCode,targetCategory,turnList,charCode) {
	series = c()
	for (i in 1:length(turnList)) { # NB: inefficient nested loop, but since transcripts are short...
		tc = targetCategory[i] # true or false if this fits with target category
		turn = turnList[i]
		for (charct in unlist(strsplit(turn,''))) {
				series = c(series, tc*which(charct==charCode)+abs(1-tc)*zeroCode)
		}
	}
	return(series)
}

### assign a code with an existing list of words (indices used); cleanTurn may be a list of turns
assignWordCodes = function(cleanTurn,wordCode) {
  series = c()
  if (length(cleanTurn)>1) {
    for (i in 1:length(cleanTurn)) { # NB: inefficient nested loop, but since transcripts are short...
      turn = cleanTurn[i]
      for (wd in unlist(strsplit(turn,' '))) {
        series = c(series, which(wd==wdCode))
      }
    }
  } else {
    for (word in unlist(strsplit(cleanTurn,' '))) {
      series = c(series, which(word==wordCode))
    }
  }
	return(series)
}

### assign a code with an existing list of words (indices used)
### and zero out according to zeroCode
### targetCategory = list of T / F if category is satisfied, aligned with wdCode
assignWordCodesDual = function(zeroCode,targetCategory,turnList,wdCode) {
  series = c()
  for (i in 1:length(turnList)) { # NB: inefficient nested loop, but since transcripts are short...
    tc = targetCategory[i] # true or false if this fits with target category
    turn = turnList[i]
    for (wd in unlist(strsplit(turn,' '))) {
      series = c(series, tc*which(wd==wdCode)+abs(1-tc)*zeroCode)
    }
  }
  return(series)
}

### let's use a look up table to clean special characters
cleanSpecialChars = function(text) {
	specialChars = c('\x92')
	substitution = c('\'')
	for (i in 1:length(specialChars)) {
		text = gsub(specialChars[i],substitution[i],text)
	}
	return(text)
}







