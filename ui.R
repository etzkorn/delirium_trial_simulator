ui <- fluidPage(

    # Application title
    titlePanel("Simulate a Delirium Trial from the Competing Joint Model"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            sliderInput("alphaM",
                        "Delirium, Mortality Association (Alpha)",
                        min = -10,
                        max = 10,
                        value = 5),
            sliderInput("alphaD",
                        "Delirium, Discharge Association (Alpha)",
                        min = -10,
                        max = 10,
                        value = -5),
            sliderInput("betaR",
                        "Treatment Effect: Delirium (Beta)",
                        min = -2,
                        max = 2,
                        step = 0.25,
                        value = -0.5),
            sliderInput("betaM",
                        "Treatment Effect: Mortality (Beta)",
                        min = -2,
                        max = 2,
                        step = 0.25,
                        value = -0.5),
            sliderInput("betaD",
                        "Treatment Effect: Discharge (Beta)",
                        min = -2,
                        max = 2,
                        step = 0.25,
                        value = 0.5),
            actionButton("newData", "Generate New Data"),
            width = 2
        ),

        # Show a plot of the generated distribution
        mainPanel(width = 10,
        	tabsetPanel(id = "tabs",
        		tabPanel(title = "Plot",
        			plotOutput("trialPlot",height = "800px")
        		),
        		tabPanel(title = "Trial Summaries",
        		         fluidRow(
        		             column(width = 4, gt_output("outcomeTable1")),
        		             column(width = 4, gt_output("outcomeTable2")),
        		             column(width = 4, gt_output("outcomeTable3"))
        		         ),
        		         plotOutput("survPlot",height = "500px")
        		),
        		tabPanel(title = "Data",
        			dataTableOutput("fullData"),
        		),
        		tabPanel(title = "Other Parameters",
        		         sliderInput("sigma",
        		                     "Random Effect SD",
        		                     min = 0,
        		                     max = 0.5,
        		                     value = 0.25),
        		         sliderInput("shapeR",
        		                     label = "Delirium: Shape",
        		                     min = 0.25,
        		                     max = 2,
        		                     value = 0.25,
        		                     step = 0.05),
        		         sliderInput("scaleR",
        		                     label = "Delirium: Scale",
        		                     min = 1,
        		                     max = 20,
        		                     value = 6,
        		                     step = 1),
        		         sliderInput("shapeM",
        		                     label = "Mortality: Shape",
        		                     min = 0.5,
        		                     max = 5,
        		                     step = 0.25,
        		                     value = 2.5),
        		         sliderInput("scaleM",
        		                     label = "Mortality: Scale",
        		                     min = 5,
        		                     max = 40,
        		                     step = 1,
        		                     value = 35),
        		         sliderInput("shapeD",
        		                     label = "Discharge: Shape",
        		                     min = 0.5,
        		                     max = 5,
        		                     step = 0.25,
        		                     value = 2.5),
        		         sliderInput("scaleD",
        		                     label = "Discharge: Scale",
        		                     min = 5,
        		                     max = 40,
        		                     step = 1,
        		                     value = 15)
        		)
        	)
        )
    )
)
