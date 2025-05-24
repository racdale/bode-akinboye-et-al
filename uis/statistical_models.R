tabPanel("Statistical Models",br(),
      tabsetPanel(type="tabs",   
        tabPanel("Aggregate Models",br(), 
           selectInput('model.dv',"DV:",c("Recurrence Rate (RR)"="RR","Determinism (DET)"="DET","Average Line Length (L)"="L","Entropy (ENT)"="ENTR"),selected=1),
           selectInput('aggregate.source',"Source:",c("Participant - RQA"="all.group2.res","Investigator - RQA"="all.group1.res","Dyad - CRQA"="all.crqa.res","Dyad - RQA"="all.auto.res"),selected="all.crqa.res"),                                    
           column(3,uiOutput("model.ivs")),
           actionButton(inputId = "run_model_button", label = "Run GLM model"),
           column(9,
            #div(id="model.lmer_table",div(DT::dataTableOutput("model.lmer"),style = "font-size:70%;padding:10px;"))
            div(id="model.lmer_table",verbatimTextOutput("model.lmer"),style = "font-size:70%;padding:10px;")
           )
        ),
        tabPanel("Windowed Models",br(),
             selectInput('model.win.dv',"DV:",c("Recurrence Rate (RR)"="RR","Determinism (DET)"="DET","Average Line Length (L)"="L","Entropy (ENT)"="ENTR"),selected=1),
             selectInput('windowed.source',"Source:",c("Participant - RQA"="windowed.part","Investigator - RQA"="windowed.inv","Dyad - CRQA"="windowed.crqa","Dyad - RQA"="windowed.auto"),selected="windowed.crqa"),                 
             column(3,uiOutput("model.win.ivs")),
             actionButton(inputId = "run_win_model_button", label = "Run LMER model"),
             column(9,
                    #div(id="model.lmer_table",div(DT::dataTableOutput("model.lmer"),style = "font-size:70%;padding:10px;"))
                    div(id="model.win.lmer_table",verbatimTextOutput("model.win.lmer"),style = "font-size:70%;padding:10px;")
             )
          )
        )
      )