tabPanel("Windowed Analyses",br(),
         selectInput('windowed.dv',"Source:",c("Recurrence Rate (RR)"="1","Determinism (DET)"="2","Average Line Length (L)"="5","Entropy (ENT)"="6"),selected=1),
         column(6,
                plotOutput('win.par.plots',height=ht),br(),
                div(id="win.par_lmer_table",div(DT::dataTableOutput("win.par.lmer"),style = "font-size:70%;padding:10px;")),
                style=stl
         ),
         column(6,
                plotOutput('win.inv.plots',height=ht),br(),
                div(id="win.inv_lmer_table",div(DT::dataTableOutput("win.inv.lmer"),style = "font-size:70%;padding:10px;")),
                style=stl
         ),
        column(6,
               plotOutput('win.crqa.plots',height=ht),br(),
               div(id="win.crqa_lmer_table",div(DT::dataTableOutput("win.crqa.lmer"),style = "font-size:70%;padding:10px;")),
               br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),
               style=stl
        ),
        column(6,
               plotOutput('win.auto.plots',height=ht),br(),
               div(id="win.auto_lmer_table",div(DT::dataTableOutput("win.auto.lmer"),style = "font-size:70%;padding:10px;")),
               style=stl
        )
    )
