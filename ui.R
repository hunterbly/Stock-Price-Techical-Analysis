source("helpers.R")



navbarPage("Signal",
           tabPanel("Summary",
                    
					textInput("sumTab_dateInput", "Date range:", value = "2017-01-01"),
					hr(),
					
					h4(textOutput("sumTab_dateDisplay")),
					fluidRow(
						
						box(formattableOutput("sumTab_summary"))
						
					),
					verbatimTextOutput("sumTab_test"),
					verbatimTextOutput("sumTab_debug")
					
					
					
					
           ),
           tabPanel("Plot",
                    sidebarLayout(
                      sidebarPanel(
                        
                        textInput("plotTab_code", "Stock Number (No description)", placeholder = "Stock Number"),
                        
                        # TODO: 
                        dateRangeInput("plotTab_daterange", "Date range:", format = "yyyy-mm-dd", start = "2017-01-01", end = "2018-12-31", min = "2010-01-01", max = "2018-12-31"),
                        
						# Radio button to choose signal for evalutaion
						radioButtons("plotTab_chooseSignal", 
									label = h3("Signal"),
									choices = signal_radioButton_list, 
									selected = "none"),
                        hr(),
                        
                        actionButton("plotTab_button", "Update View")
                        
                        
                      ),
                      
                      mainPanel(
                        h4("Plot"),
                        plotOutput("plotTab_mainplot"),
                        
						h4("Test"),
                        verbatimTextOutput("plotTab_test"),
						
                        h4("Summary"),
                        verbatimTextOutput("plotTab_summary"),
                        
						
						
                        h4("Debug"),
                        verbatimTextOutput("plotTab_debug")
                      )
                    )
           ),
		   tabPanel("About",

					# Test include html
					includeHTML("./aboutPage.html")
           )
           

)
