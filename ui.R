ui <- fluidPage(

    # Application title
    titlePanel("Simulate a Delirium Trial from the Competing Joint Model"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            sliderInput("alphaM",
                        "Alpha, Mortality",
                        min = -10,
                        max = 10,
                        value = 0),
            sliderInput("alphaD",
                        "Alpha, Discharge",
                        min = -10,
                        max = 10,
                        value = 0),
            sliderInput("betaR",
                        "Beta, Delirium",
                        min = -2,
                        max = 2,
                        value = 0),
            sliderInput("betaM",
                        "Beta, Mortality",
                        min = -2,
                        max = 2,
                        value = 0),
            sliderInput("betaD",
                        "Beta, Discharge",
                        min = -2,
                        max = 2,
                        value = 0),
            width = 2
        ),

        # Show a plot of the generated distribution
        mainPanel(width = 10,
        	tabsetPanel(id = "tabs",
        		tabPanel(title = "Plot",
        			plotOutput("trialPlot",height = "650px")
        		),
        		tabPanel(title = "Table",
        		    strong("Overall"),
        			dataTableOutput("outcomeTable1"),
        			strong("Among Patients who Died"),
        			dataTableOutput("outcomeTable2"),
        			strong("Among Patients who Discharged"),
        			dataTableOutput("outcomeTable3")
        		)
        	)
        )
    )
)
