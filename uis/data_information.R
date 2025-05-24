tabPanel("Data Information",br(),
                 div(id="data_table",div(DT::dataTableOutput("data.table"),style = "font-size:90%")),
                 fluidRow(
                   column(12,
                          h3(textOutput('dyad.name')),
                          DT::dataTableOutput('dyad.view'),
                          style='font-size:90%;padding-top:20px;'
                   )
                 )
        )