fluidRow(
    column(12,
		selectInput('data.src',selected="Test","Corpus:",
		           c("REAL (pilot data)"="REAL","Traumatic Brain Injury"="Coelho","CHILDES Sample (Warren Corpus)"="Warren","Debugging (Test) Data Set"="Test")),
		selectInput('data.level',selected="lexical","Linguistic Level:",
		           c("Lexical"="lexical","Semantic"="semantic","Syntactic"="syntactic")),
		div(style="display:inline-block;",textInput('omit.fs',label='Files to Omit:')),
		div(style="display:inline-block;",actionButton('omit.button',label='Omit')),
		div(style='font-size:x-small;color:silver;','Use Filename values separated by commas, e.g.: n01.cha,n02.cha'),
		br()
))