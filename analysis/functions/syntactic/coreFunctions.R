### core functions for performing syntactic-level analysis of transcripts structured in the manner described in the GitHub repository

# note that rJava may be tricky to install depending on Java installation settings on MacOS
require(rJava)
require(openNLP)
require(NLP)
# sent_token_annotator = Maxent_Sent_Token_Annotator()
word_token_annotator = Maxent_Word_Token_Annotator()
pos_tag_annotator = Maxent_POS_Tag_Annotator()

posAnnotation = function(s) {
  if (nchar(s)==0) {
    return('')
  }
  s = as.String(s)
  sw_annot = annotate(s, list(sent_token_annotator, word_token_annotator))
  pos_annot = annotate(s, pos_tag_annotator, sw_annot)
  ixes = pos_annot$features[[1]]$constituents
  return(unlist(lapply(ixes,function(x) { pos_annot$features[[x]]$POS } )))
  ## get tag probabilities, if desired (for later)
  # head(annotate(pos_annot, Maxent_POS_Tag_Annotator(probs = TRUE), sw_annot))
}

parseAnnotation = function(s) {
  parse_annotator <- Parse_Annotator()
  ## Compute the parse annotations only.
  p <- parse_annotator(s, a2)
  ## Extract the formatted parse trees.
  ptexts <- sapply(p$features, `[[`, "parse")
  ptexts
  ## Read into NLP Tree objects.
  ptrees <- lapply(ptexts, Tree_parse)
  ptrees  
}



