tablemultUI	<-	function(id, SHOW = TRUE, ..., label = "Multi Table UI")	{
	ns	<-	NS(id)
	
	if (!SHOW)	return(NULL)
	
	tagList(
		tags$head(tags$style(HTML("hr {border-top: 1px solid #000000;}"))),
		p("CPU Temp"),
		textInput("multiTEMPperc",	label = "Percentiles (0 - 100)"),
		textInput("multiTEMPecdf",	label = "Specific Values (°C)"),
		hr(),
		p("Frequency"),
		textInput("multiFREQperc",	label = "Percentiles (0 - 100)"),
		textInput("multiFREQecdf",	label = "Specific Values (MHz)"),
		hr(),
		p("Socket Energy"),
		textInput("multiSOCKperc",	label = "Percentiles (0 - 100)"),
		textInput("multiSOCKecdf",	label = "Specific Values (W)"),
		hr(),
		p("Core Energy"),
		textInput("multiCOREperc",	label = "Percentiles (0 - 100)"),
		textInput("multiCOREecdf",	label = "Specific Values (W)"),
	)
}

tableUI	<-	function(id, SHOW = TRUE, SHOWmulti = TRUE, ..., label = "Table UI")	{
	ns	<-	NS(id)
	
	if (!SHOW)	return(NULL)
	
	out <-	tagList(
				numericInput(inputId	=	"roundTerm",
							label	=	"Decimals in Tables",	value	=	2,
							min	=	0,	step	=	1
						),
				checkboxInput("medOFFapply", "Subtract Median Warm-up Temp"),
				radioButtons(inputId	=	"engPOW",	label	=	"mJ or W",	inline	=	TRUE,
					choices	=	c("mJ", "W"),	selected = "W"),
				checkboxGroupInput(inputId	=	"listPERI",	label	=	"Period to show:"),
				checkboxGroupInput(inputId	=	"listMEAS",	label	=	"Measurement to show:"),
				radioButtons(inputId	=	"tabORDER",	label	=	"Group Order",	inline	=	FALSE,
					choiceNames		=	c("Period, Measurement",	"Measurement, Period"),
					choiceValues	=	c(FALSE,					TRUE),	selected	=	FALSE),
				checkboxGroupInput(inputId	=	"listSTAT",	label	=	"Statistics to show:"),
			)
	if (SHOWmulti)	return(
		tabPanel("Table", 
			tabsetPanel(
				tabPanel("Controls", out),
				tabPanel("Custom Stats", tablemultUI("tableMULTI"))
				)	)	)
	tabPanel("Table",	out)
}

tablemultoutUI	<-	function(id, SHOW = TRUE, ..., label = "Multi Table Outputs")	{
	ns	<-	NS(id)
	
	if (!SHOW)	return(NULL)
	
	tagList(
		fixedRow(
			column(6,	tableOutput("tableTEMPperc")),
			column(6,	tableOutput("tableTEMPecdf"))
		),
		fixedRow(
			column(6,	tableOutput("tableFREQperc")),
			column(6,	tableOutput("tableFREQecdf"))
		),
		fixedRow(
			column(6,	tableOutput("tableSOCKperc")),
			column(6,	tableOutput("tableSOCKecdf"))
		),
		fixedRow(
			column(6,	tableOutput("tableCOREperc")),
			column(6,	tableOutput("tableCOREecdf"))
		),
	)
}

GRAPHtabUI	<-	function(id, SHOW = TRUE, BRUSH = TRUE, ..., label = "Graphs UI")	{
	ns	<-	NS(id)

	ifBRUSH	=	function(IN)	{
		if (BRUSH)	return(IN)
		return(NULL)
	}
	if (!SHOW)	return(NULL)
	BRUSHexpl	<-	NULL
	BRUSHexpl	<-	strong("Click and Drag to Zoom Below")

	tabPanel("Course Graphs",
		tagList(
			tabsetPanel(
				tabPanel("Mean Frequency",
					plotOutput('graphMEAN',	height=720,	dblclick	=	ifBRUSH("brushMEANdbl"),
						brush	=	ifBRUSH(brushOpts(id	=	"brushMEAN", resetOnNew	=	TRUE, direction	=	"x"))),
					if (BRUSH)	tagList(	BRUSHexpl,
						fixedRow(
							column(3,	numericInput(inputId	=	"brushMEANstart",
								value = -300, label	=	"Zoom Start (s)",	step	=	1)
								),
							column(3,	numericInput(inputId	=	"brushMEANlength",
								value = 7500,	label	=	"Zoom Length (s)",	step	=	1)
								),
							column(3,	actionButton(inputId	=	"brushMEANupdate", label = "Update Zoom"))
						),
						plotOutput('brushMEANzoom',	height=720)
					)
				),
				tabPanel("Maximum Frequency",
					plotOutput('graphMAX',	height=720,	dblclick	=	ifBRUSH("brushMAXdbl"),
						brush	=	ifBRUSH(brushOpts(id	=	"brushMAX", resetOnNew	=	TRUE, direction	=	"x"))),
					if (BRUSH)	tagList(	BRUSHexpl,
						fixedRow(
							column(3,	numericInput(inputId	=	"brushMAXstart",
								value = -300, label	=	"Zoom Start (s)",	step	=	1)
								),
							column(3,	numericInput(inputId	=	"brushMAXlength",
								value = 7500,	label	=	"Zoom Length (s)",	step	=	1)
								),
							column(3,	actionButton(inputId	=	"brushMAXupdate", label = "Update Zoom"))
						),
						plotOutput('brushMAXzoom',	height=720),
					),
				),
				tabPanel("Per-Thread Frequency",
					plotOutput('graphFREQ',	height="auto",	dblclick	=	ifBRUSH("brushFREQdbl"),
						brush	=	ifBRUSH(brushOpts(id	=	"brushFREQ", resetOnNew	=	TRUE, direction	=	"x"))),
					if (BRUSH)	tagList(	BRUSHexpl,
						fixedRow(
							column(3,	numericInput(inputId	=	"brushFREQstart",
								value = -300, label	=	"Zoom Start (s)",	step	=	1)		),
							column(3,	numericInput(inputId	=	"brushFREQlength",
								value = 7500,	label	=	"Zoom Length (s)",	step	=	1)	),
							column(3,	actionButton(inputId	=	"brushFREQupdate", label = "Update Zoom")),
						),
						checkboxGroupInput('threadSEL',	label = "Threads Selected",	inline = TRUE),
						# plotOutput('brushFREQzoom',	height="auto"),
						uiOutput('brushFREQzoomFILT'),
					)
				),
				tabPanel("Per-Core Power",
					plotOutput('graphPOWER',	height="auto",	dblclick	=	ifBRUSH("brushPOWERdbl"),
						brush	=	ifBRUSH(brushOpts(id	=	"brushPOWER", resetOnNew	=	TRUE, direction	=	"x"))),
					if (BRUSH)	tagList(	BRUSHexpl,
						fixedRow(
							column(3,	numericInput(inputId	=	"brushPOWERstart",
								value = -300, label	=	"Zoom Start (s)",	step	=	1)		),
							column(3,	numericInput(inputId	=	"brushPOWERlength",
								value = 7500,	label	=	"Zoom Length (s)",	step	=	1)	),
							column(3,	actionButton(inputId	=	"brushPOWERupdate", label = "Update Zoom")),
						),
						checkboxGroupInput('coreSEL',	label = "Cores Selected",	inline = TRUE),
						# plotOutput('brushPOWERzoom',	height="auto"),
						uiOutput('brushPOWERzoomFILT'),
					)
				),
			)
		)
	)
}
HISTtabUI	<-	function(id, SHOW = TRUE, BRUSH = TRUE, ..., label = "Histograms UI")	{
	ns	<-	NS(id)

	ifBRUSH	=	function(IN)	{
		if (BRUSH)	return(IN)
		return(NULL)
	}
	if (!SHOW)	return(NULL)

	tabPanel("Histograms",
		tagList(
			tabsetPanel(
				tabPanel("Temperature",
					fixedRow(
						column(2, numericInput('graphHISTtempBIN',	label = "Bin Width",	value = 1,	min = 0)),
						column(3, numericInput('graphHISTtempMIN',	label = "X-Minimum",	value = 0,	min = 0)),
						column(3, actionButton(inputId	=	'graphHISTtempUPD',	label = "Update Histogram")),
					),
					plotOutput('graphHISTtemp',	height=720,
						brush	=	ifBRUSH(brushOpts(id	=	"brushHISTtemp", resetOnNew	=	TRUE, direction	=	"x"))),
					if (BRUSH)	tagList(
						fixedRow(
							column(3, numericInput('brushHISTtempMIN',	label = "Lower Limit (°C)",
								value = NULL,	step = 1,	min = 0,	max = 115)),
							column(3, numericInput('brushHISTtempMAX',	label = "Upper Limit (°C)",
								value = NULL,	step = 1,	min = 0,	max = 115))
						),
						tableOutput('graphHISTtempTAB')
					)
				),
				tabPanel("Frequency",
					fixedRow(
						column(2, numericInput('graphHISTfreqBIN',	label = "Bin Width",	value = 1,	min = 0)),
						column(3, numericInput('graphHISTfreqMIN',	label = "X-Minimum",	value = 2000,	min = 0)),
						column(3, actionButton(inputId	=	'graphHISTfreqUPD',	label = "Update Histogram")),
						column(4, textInput('graphHISTfreqSPEC',	label = "Frequency Specs (MHz)")),
					),
					plotOutput('graphHISTfreq',	height=720,
						brush	=	ifBRUSH(brushOpts(id	=	"brushHISTfreq", resetOnNew	=	TRUE, direction	=	"x"))),
					if (BRUSH)	tagList(
						fixedRow(
							column(3, numericInput('brushHISTfreqMIN',	label = "Lower Limit (MHz)",
								value = NULL,	step = 100,	min = 0,	max = 6000)),
							column(3, numericInput('brushHISTfreqMAX',	label = "Upper Limit (MHz)",
								value = NULL,	step = 100,	min = 0,	max = 6000))
						),
						tableOutput('graphHISTfreqTAB')
					)
				),
				tabPanel("Socket Power",
					fixedRow(
						column(2, numericInput('graphHISTsockBIN',	label = "Bin Width",	value = 0.1,	min = 0,	step = 0.1)),
						column(3, numericInput('graphHISTsockMIN',	label = "X-Minimum",	value = 0,	min = 0)),
						column(3, actionButton(inputId	=	'graphHISTsockUPD',	label = "Update Histogram")),
					),
					plotOutput('graphHISTsock',	height=720,
						brush	=	ifBRUSH(brushOpts(id	=	"brushHISTsock", resetOnNew	=	TRUE, direction	=	"x"))),
					if (BRUSH)	tagList(
						fixedRow(
							column(3, numericInput('brushHISTsockMIN',	label = "Lower Limit (W)",
								value = NULL,	step = 1,	min = 0,	max = 300)),
							column(3, numericInput('brushHISTsockMAX',	label = "Upper Limit (W)",
								value = NULL,	step = 1,	min = 0,	max = 300))
						),
						tableOutput('graphHISTsockTAB')
					)
				),
				tabPanel("Core Power",
					fixedRow(
						column(2, numericInput('graphHISTcoreBIN',	label = "Bin Width",	value = 0.1,	min = 0,	step = 0.1)),
						column(3, numericInput('graphHISTcoreMIN',	label = "X-Minimum",	value = 0,	min = 0)),
						column(3, actionButton(inputId	=	'graphHISTcoreUPD',	label = "Update Histogram")),
					),
					plotOutput('graphHISTcore',	height=720,
						brush	=	ifBRUSH(brushOpts(id	=	"brushHISTcore", resetOnNew	=	TRUE, direction	=	"x"))),
					if (BRUSH)	tagList(
						fixedRow(
							column(3, numericInput('brushHISTcoreMIN',	label = "Lower Limit (W)",
								value = NULL,	step = 0.1,	min = 0,	max = 300)),
							column(3, numericInput('brushHISTcoreMAX',	label = "Upper Limit (W)",
								value = NULL,	step = 0.1,	min = 0,	max = 300))
						),
						tableOutput('graphHISTcoreTAB')
					)
				),
				tabPanel("Uncore Power",
					fixedRow(
						column(2, numericInput('graphHISTuncoreBIN',	label = "Bin Width",	value = 0.1,	min = 0,	step = 0.1)),
						column(3, numericInput('graphHISTuncoreMIN',	label = "X-Minimum",	value = 0,	min = 0)),
						column(3, actionButton(inputId	=	'graphHISTuncoreUPD',	label = "Update Histogram")),
					),
					plotOutput('graphHISTuncore',	height=720,
						brush	=	ifBRUSH(brushOpts(id	=	"brushHISTuncore", resetOnNew	=	TRUE, direction	=	"x"))),
					if (BRUSH)	tagList(
						fixedRow(
							column(3, numericInput('brushHISTuncoreMIN',	label = "Lower Limit (W)",
								value = NULL,	step = 0.1,	min = 0,	max = 300)),
							column(3, numericInput('brushHISTuncoreMAX',	label = "Upper Limit (W)",
								value = NULL,	step = 0.1,	min = 0,	max = 300))
						),
						tableOutput('graphHISTuncoreTAB')
					)
				),
			)
		)
	)
}
TSERIEStabUI	<-	function(id, SHOW = TRUE, BRUSH = TRUE, ..., label = "Time Series UI")	{
	ns	<-	NS(id)

	ifBRUSH	=	function(IN)	{
		if (BRUSH)	return(IN)
		return(NULL)
	}
	if (!SHOW)	return(NULL)

	tabPanel("Time Series",
		tagList(
			tabsetPanel(
				tabPanel("Temperature",
					plotOutput('timeTEMPtrend',	height=480,	dblclick	=	ifBRUSH("brushTIMEtempTRENDdbl"),
						brush	=	ifBRUSH(brushOpts(id	=	"brushTIMEtempTREND", resetOnNew	=	TRUE, direction	=	"x"))),
					plotOutput('timeTEMPseas',	height=480,	dblclick	=	ifBRUSH("brushTIMEtempSEASdbl"),
						brush	=	ifBRUSH(brushOpts(id	=	"brushTIMEtempSEAS", resetOnNew	=	TRUE, direction	=	"x"))),
				),
				tabPanel("Frequency",
					plotOutput('timeFREQtrend',	height=480,	dblclick	=	ifBRUSH("brushTIMEfreqTRENDdbl"),
						brush	=	ifBRUSH(brushOpts(id	=	"brushTIMEfreqTREND", resetOnNew	=	TRUE, direction	=	"x"))),
					plotOutput('timeFREQseas',	height=480,	dblclick	=	ifBRUSH("brushTIMEfreqSEASdbl"),
						brush	=	ifBRUSH(brushOpts(id	=	"brushTIMEfreqSEAS", resetOnNew	=	TRUE, direction	=	"x"))),
				),
				tabPanel("Socket Power",
					plotOutput('timeSOCKtrend',	height=480,	dblclick	=	ifBRUSH("brushTIMEsockTRENDdbl"),
						brush	=	ifBRUSH(brushOpts(id	=	"brushTIMEsockTREND", resetOnNew	=	TRUE, direction	=	"x"))),
					plotOutput('timeSOCKseas',	height=480,	dblclick	=	ifBRUSH("brushTIMEsockSEASdbl"),
						brush	=	ifBRUSH(brushOpts(id	=	"brushTIMEsockSEAS", resetOnNew	=	TRUE, direction	=	"x"))),
				),
				tabPanel("Core Power",
					plotOutput('timeCOREtrend',	height=480,	dblclick	=	ifBRUSH("brushTIMEcoreTRENDdbl"),
						brush	=	ifBRUSH(brushOpts(id	=	"brushTIMEcoreTREND", resetOnNew	=	TRUE, direction	=	"x"))),
					plotOutput('timeCOREseas',	height=480,	dblclick	=	ifBRUSH("brushTIMEcoreSEASdbl"),
						brush	=	ifBRUSH(brushOpts(id	=	"brushTIMEcoreSEAS", resetOnNew	=	TRUE, direction	=	"x"))),
				),
			)
		)
	)
}

ui	<-	fluidPage(
	titlePanel("CPU Temperature Statistics and Graphs"),
	sidebarLayout(
		sidebarPanel(
			selectInput('dataSel',	label	=	"Data to Load",	selectize	=	FALSE,
				# choices	=	setNames(FILES, gsub(".env|.RData", "", names(FILES))), selected	=	FILES[1]
				choices	=	FILES, selected	=	FILES[1]
			),
			actionButton(inputId	=	"dataSelLOAD",	label	=	"Load Selected Data"),
			tabsetPanel(
				tableUI("tableTAB", TRUE, SHOWmulti = VIEW$MULTtab),
				tabPanel("Graphs",
					numericInput('FREQ.COEF',	label	=	"Power-Frequency Coefficient",
						value = 0.01,	min = 0.01,	max = 1,	step = 0.01),
				),
				id		=	"Table Controls",
				type	=	"pills"
			)
		),
		mainPanel(
			tableOutput('subTitle'),
			tabsetPanel(
				tabPanel("Table",
					tableOutput("tableSUMM"),
					tablemultoutUI("tableMULI", VIEW$MULTtab),
				),
			# textOutput("test"),
				GRAPHtabUI("graphTAB",	VIEW$GRAPHS,	VIEW$BRUSH),
				HISTtabUI("histTAB",	VIEW$HIST,		VIEW$BRUSH),
				TSERIEStabUI("histTAB",	VIEW$TSERIES,	VIEW$BRUSH),
			id	=	"Table",
			type	=	"pills"
			)
		)
	)
)