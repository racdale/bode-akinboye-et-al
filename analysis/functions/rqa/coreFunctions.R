require(crqa)

# uses crqa
plot_rp = function(RP,xlab='i',ylab='j',cex=.1) { 
  if (!is.matrix(RP)) { RP = as.matrix(RP) }
  ij = which(RP==1,arr.ind=T)
  plot(ij[,1],ij[,2],cex=cex,xlab=xlab,ylab=ylab,pch=16,xlim=c(1,nrow(RP)),ylim=c(1,nrow(RP))) 
}
