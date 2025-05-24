### core functions for performing lexical-level analysis of transcripts structured in the manner described in the GitHub repository

#source('lexical/coreFunctions.R') # make sure to include lexical core functions

# this requires a time series vector from two sources to determine the
# semantic similarity. this is demonstrated in the lexical example, and that
# should also be integrated in the core functions

# see description in angus et al. (Conc. RPs): https://s3.amazonaws.com/academia.edu.documents/6390899/conceptual_recurrence_plots.pdf?AWSAccessKeyId=AKIAIWOWYYGZ2Y53UL3A&Expires=1533756806&Signature=HftjOoonfphUBepjkwvQr4q0Tqs%3D&response-content-disposition=inline%3B%20filename%3DConceptual_Recurrence_Plots_Revealing_Pa.pdf
# wsz = number of turns
# t1 and t2 should have <0 entries in non-turn overlap
saltonModel = function(t1,t2,wsz=3,turnForce=NULL) {
  # pad:
  t1 = c(-10,t1) # so we get first turn
  t2 = c(-10,t2)
  t1_turns = which(diff(t1>0)>0)+1
  t2_turns = which(diff(t2>0)>0)+1
  if (!is.null(turnForce)) {
    # if we are running a FULL semantic RP without taking
    # the turns of interlocutors... then we should force a set of documents to define the salton
    # model, this is set by the researcher...
    tot_turns = turnForce
  } else {
    tot_turns = sort(unique(c(t1_turns,t2_turns))) 
  }
  
  # loop through turn windows and generate a semantic model
  sem.model = matrix(0,nrow=max(c(t1,t2)),ncol=max(c(t1,t2)))
  occ = 0*(1:nrow(sem.model))
  n.comps = 0
  for (i in (wsz+1):(length(tot_turns)-1)) {
    ixes = tot_turns[i-wsz]:tot_turns[i+1] # get up to NEXT turn, but then trim...
    sub_t = t1[ixes]
    sub_t = sub_t[-length(sub_t)] # trim it here...
    sub_t = unique(sub_t[sub_t>0]) # only count it once (a-la angus et al.)
    
    occ[sub_t] = occ[sub_t] + 1
    
    sub_coocc = expand.grid(sub_t,sub_t)
    sub_coocc = sub_coocc[sub_coocc[,1]!=sub_coocc[,2],]
    indices = (sub_coocc[,1]-1)*nrow(sem.model)+sub_coocc[,2]
    #indices_2 = (sub_t-1)*nrow(sem.model)+sub_t
    sem.model[indices]=sem.model[indices]+1
    n.comps = n.comps + 1
  }
  
  ixes.arr = which(sem.model>0,arr.ind=T)
  ixes.n = which(sem.model>0,arr.ind=F)
  p.t1.t2 = (sem.model / n.comps)[ixes.n]
  
  occ.sum = (occ[ixes.arr[,1]]+occ[ixes.arr[,2]])
  p.nt1.nt2 = (n.comps - occ.sum + sem.model[ixes.n])/n.comps # from angus et al.
  p.nt1.nt2[p.nt1.nt2==0]=1
  
  p.nt1.t2 = (occ-n.comps)/n.comps
  p.nt1.t2 = t(matrix(rep(p.nt1.t2,length(p.nt1.t2)),nrow=length(p.nt1.t2)))
  p.t1.nt2 = t(p.nt1.t2)
  
  sem.model[ixes.n] = (p.t1.t2 * p.nt1.nt2) / (p.nt1.t2[ixes.n] * p.t1.nt2[ixes.n])
  
  return(sem.model)
}

semanticRP = function(t1,t2,target.rr=.05,turnForce=NULL) {
  sem.model = saltonModel(t1,t2,wsz=3,turnForce=turnForce)
  ixes.1 = which(t1>0)
  ixes.2 = which(t2>0)
  rp.ixes = expand.grid(ixes.1,ixes.2) # columns are possible RP ij locations
  wd.ixes = cbind(t1[rp.ixes[,1]],t2[rp.ixes[,2]]) # columns are word IDs
  
  sem.ixes = (wd.ixes[,1]-1)*nrow(sem.model)+wd.ixes[,2] # which comparisons do we need?
  sem.vals = sem.model[sem.ixes] # let's get the semantic similarity (Salton, as in angus et al.)
  
  sem.rp = matrix(0,nrow=length(t1),ncol=length(t2)) # initialize RP
  rp.indices = (rp.ixes[,1]-1)*nrow(sem.rp)+rp.ixes[,2] 
  sem.rp[rp.indices] = sem.vals
  cutoff = sort(sem.rp,decreasing=T)[ceiling(target.rr*length(sem.rp))]
  RP = 1*(sem.rp>cutoff)
  return(RP)
}

semanticWindowed = function(rp,window.size,window.shift) {
  if (is.null(dim(rp))) {
    print('RP appears to be NA; populating with 0s');
    rp = matrix(0,nrow=window.size*6+1,ncol=window.size*6+1)
  }
  ixes = seq(from=1,to=nrow(rp),by=window.shift)
  res = c()
  for (i in ixes) {
    if (i+window.size>nrow(rp)) { break }
    r = i:(i+window.size-1)
    c = i:(i+window.size-1)
    sub.rp = rp[r,][,c]
    res.crqa = crqa(sub.rp,recpt=T,
                    embed=2,delay=1,radius=.001,
                    normalize=0,rescale=0,mindiagline=2,minvertline=2)
    #colnames(win.res) = c('RR','DET','NRLINE','maxL','L','ENTR','rENTR','LAM','TT','w')
    res = rbind(res,data.frame(RR=res.crqa$RR,DET=res.crqa$DET,
                               NRLINE=res.crqa$NRLINE,maxL=res.crqa$maxL,
                               L=res.crqa$L,ENTR=res.crqa$ENT,rENTR=res.crqa$rENTR,
                               LAM=res.crqa$LAM,TT=res.crqa$TT,w=which(ixes==i)))
  }
  return(res)
}

# eyeballing in simple, understood time series
# t1 = c(1,2,3,-1,1,2,3,-1,1,2,3,-1,1,2,3,-1,1,2,3,-1,1,2,3,-1,4,5,6,-1,4,5,6,-1,4,5,6,-1,4,5,6,-1,5,6,7,-1,5,6,7,-1)
# plot_rp(semanticRP(t1=t1,t2=t1,target.rr=.003),xlab='i',ylab='j',cex=0.25)





