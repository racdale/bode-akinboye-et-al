ht = 300
stl = 'padding:0px;margin:0px;'
title.stl = 'font-size:14pt;'
row.stl = 'margin-left:10px;margin-right:10px;'
row.plotter = function(title,pref,n) {
  if (n==3) {
    the.row = fluidRow(div(title,style=title.stl),
                       column(4,
                              plotOutput(paste(pref,1,sep=''),height=ht),style=stl
                       ),
                       column(4,
                              plotOutput(paste(pref,2,sep=''),height=ht),style=stl
                       ),
                       column(4,
                              plotOutput(paste(pref,3,sep=''),height=ht),style=stl
                       )
                  ,style=row.stl)
  }
  else if (n==4) {
    the.row = fluidRow(div(title,style=title.stl),
                       column(3,
                              plotOutput(paste(pref,1,sep=''),height=ht),style=stl
                       ),
                       column(3,
                              plotOutput(paste(pref,2,sep=''),height=ht),style=stl
                       ),
                       column(3,
                              plotOutput(paste(pref,3,sep=''),height=ht),style=stl
                       ),
                       column(3,
                              plotOutput(paste(pref,4,sep=''),height=ht),style=stl
                       )
                       ,style=row.stl)
  }
  else if (n==2) {
    the.row = fluidRow(div(title,style=title.stl),
                       column(6,
                              plotOutput(paste(pref,1,sep=''),height=ht),style=stl
                       ),
                       column(6,
                              plotOutput(paste(pref,2,sep=''),height=ht),style=stl
                       )
                       ,style=row.stl)
  }
  
  return(the.row)
}
