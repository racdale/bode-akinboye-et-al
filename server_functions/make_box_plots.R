make_box_plots = function(output,session,interlocs,all.crqa.res,all.group1.res,all.group2.res,all.auto.res) {
  output$nlp1 = renderPlot({ plot.boxdot(all.crqa.res,aes(cond,length.group2),'red','Length (Number of Words)',interlocs$labels[2],session,15) })
  output$nlp2 = renderPlot({ plot.boxdot(all.crqa.res,aes(cond,length.group1),'red','Length (Number of Words)',interlocs$labels[1],session,25) })
  output$nlp3 = renderPlot({ plot.boxdot(all.crqa.res,aes(cond,word.types.group2),'red','Number of Unique Words',interlocs$labels[2],session,65) })
  output$nlp4 = renderPlot({ plot.boxdot(all.crqa.res,aes(cond,word.types.group1),'red','Number of Unique Words',interlocs$labels[1],session,100) })
  
  output$crqa1 = renderPlot({ plot.boxdot(all.crqa.res,aes(cond,RR),'green','Recurrence Rate','Dyad - CRQA',session,10) })
  output$crqa2 = renderPlot({ plot.boxdot(all.crqa.res,aes(cond,DET),'green','Determinism','',session,20) })
  output$crqa3 = renderPlot({ plot.boxdot(all.crqa.res,aes(cond,ENTR),'green','Entropy','',session,30) })
  output$crqa4 = renderPlot({ plot.boxdot(all.crqa.res,aes(cond,L),'green','Average Line Length','',session,40) })
  
  output$part1 = renderPlot({ plot.boxdot(all.group2.res,aes(cond,RR),'blue','Recurrence Rate',paste(interlocs$labels[2],'- RQA'),session,50) })
  output$part2 = renderPlot({ plot.boxdot(all.group2.res,aes(cond,DET),'blue','Determinism','',session,55) })
  output$part3 = renderPlot({ plot.boxdot(all.group2.res,aes(cond,ENTR),'blue','Entropy','',session,60) })
  output$part4 = renderPlot({ plot.boxdot(all.group2.res,aes(cond,L),'blue','Average Line Length','',session,65) })
  
  output$inv1 = renderPlot({ plot.boxdot(all.group1.res,aes(cond,RR),'pink','Recurrence Rate',paste(interlocs$labels[1],'- RQA'),session,75) })
  output$inv2 = renderPlot({ plot.boxdot(all.group1.res,aes(cond,DET),'pink','Determinism','',session,80) })
  output$inv3 = renderPlot({ plot.boxdot(all.group1.res,aes(cond,ENTR),'pink','Entropy','',session,85) })
  output$inv4 = renderPlot({ plot.boxdot(all.group1.res,aes(cond,L),'pink','Average Line Length','',session,86) })
  
  output$auto1 = renderPlot({ plot.boxdot(all.auto.res,aes(cond,RR),'orange','Recurrence Rate','Dyad - RQA',session,86) })
  output$auto2 = renderPlot({ plot.boxdot(all.auto.res,aes(cond,DET),'orange','Determinism','',session,87) })
  output$auto3 = renderPlot({ plot.boxdot(all.auto.res,aes(cond,ENTR),'orange','Entropy','',session,92) })
  output$auto4 = renderPlot({ plot.boxdot(all.auto.res,aes(cond,L),'orange','Average Line Length','',session,100) })
}