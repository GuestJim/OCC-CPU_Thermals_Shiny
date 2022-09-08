ifBRUSH	=	function(IN)	{
	if (VIEW$BRUSH)	return(IN)
	return(NULL)
}

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
		p("Socket Power"),
		textInput("multiSOCKperc",	label = "Percentiles (0 - 100)"),
		textInput("multiSOCKecdf",	label = "Specific Values (W)"),
		hr(),
		p("Core Power"),
		textInput("multiCOREperc",	label = "Percentiles (0 - 100)"),
		textInput("multiCOREecdf",	label = "Specific Values (W)"),
	)
}

tablecrossUI	<-	function(id, SHOW = TRUE, ..., label = "Temperature Cross Table UI")	{
	ns	<-	NS(id)
	
	if (!SHOW)	return(NULL)
	
	tagList(
		numericInput(inputId	=	"tableCROSSlim",	label	=	"Length of Lists",
			value	=	10,	min	=	0,	step	=	1),
		strong("Test Period Temperature Crosses"),
		numericInput(inputId	=	"tableCROSStest",
			label	=	"",	value	=	0,	step	=	1),
		strong("Cooldown Temperature Crosses"),
		numericInput(inputId	=	"tableCROSScool",	
			label	=	"",		value	=	0,	step	=	1),
	)
}

tableUI	<-	function(id, SHOW = TRUE, SHOWmulti = TRUE, SHOWcross = TRUE, ..., label = "Table UI")	{
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
	if (any(c(SHOWmulti, SHOWcross)))	return(
		tabPanel("Table", 
			tabsetPanel(
				tabPanel("Controls", out),
				if (SHOWmulti)	tabPanel("Custom Stats", tablemultUI("tableMULTI")),
				if (SHOWcross)	tabPanel("Temp. Crosses", tablecrossUI("tableCROSS")),
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

tablecrossoutUI	<-	function(id, SHOW = TRUE, ..., label = "Temperature Cross Table Outputs")	{
	ns	<-	NS(id)
	
	if (!SHOW)	return(NULL)
	
	tagList(
		fillRow(
			tagList(strong("Test Period"),	hr(),	tableOutput('tableCROSStest')),
			tagList(strong("Cooldown"),		hr(),	tableOutput('tableCROSScool')),
			flex = c(5, 7)
		)
	)
}

#	Graph and Brush Control, not zoomed graph
graphUI	<-	function(id, BRUSH, HEIGHT = 720, START = -300, LENGTH = 7500)	{
	ID	<-	id
	ns	<-	NS(id)
	
	pasteBRUSH	<-	function(IN = NULL)	paste0("brush", ID, IN)
	
	tagList(
		plotOutput(paste0("graph", ID),	height = HEIGHT,	dblclick	=	ifBRUSH(pasteBRUSH("dbl")),
			brush	=	ifBRUSH(brushOpts(id	=	pasteBRUSH(), resetOnNew	=	TRUE, direction	=	"x"))),
		if (BRUSH)	tagList(	strong("Click and Drag to Zoom Below"),
			fixedRow(
				column(3,	numericInput(inputId	=	pasteBRUSH("start"),
					value = START, 	label	=	"Zoom Start (s)",	step	=	1)
					),
				column(3,	numericInput(inputId	=	pasteBRUSH("length"),
					value = LENGTH,	label	=	"Zoom Length (s)",	step	=	1)
					),
				column(3,	actionButton(inputId	=	pasteBRUSH("update"), label = "Update Zoom"))
			)
		)
	)
}

GRAPHtabUI	<-	function(id, SHOW = TRUE, BRUSH = TRUE, ..., label = "Graphs UI", HEIGHT = 720)	{
	ns	<-	NS(id)

	if (!SHOW)	return(NULL)

	tabPanel("Course Graphs",
		tagList(
			tabsetPanel(
				tabPanel("Mean Frequency",
					graphUI('MEAN', BRUSH, HEIGHT),
					plotOutput('brushMEANzoom',	height = HEIGHT)
				),
				tabPanel("Maximum Frequency",
					graphUI('MAX', BRUSH, HEIGHT),
					plotOutput('brushMAXzoom',	height = HEIGHT),
				),
				tabPanel("Per-Thread Frequency",
					graphUI('FREQ', BRUSH, HEIGHT = "auto"),
					fluidRow(
						column(9,	checkboxGroupInput('threadSEL',	label = "Threads Selected",	inline = TRUE)),
						column(3,	actionButton('threadSELapply',	label = "Apply"),
									helpText('Must be pressed before zoom-graphs appear')),
					),
					uiOutput('brushFREQzoomFILT'),
				),
				tabPanel("Per-Core Power",
					graphUI('POWER', BRUSH, HEIGHT = "auto"),
					fluidRow(
						column(9,	checkboxGroupInput('coreSEL',	label = "Cores Selected",	inline = TRUE)),
						column(3,	actionButton('coreSELapply',	label = "Apply"),
									helpText('Must be pressed before zoom-graphs appear')),
					),
					uiOutput('brushPOWERzoomFILT'),
				),
			)
		)
	)
}

#	Histogram controls
histUI	<-	function(id, BIN.val, BIN.step, MIN.val = 0, ..., HEIGHT = 720, LAB = NULL)	{
	ID	<-	tolower(id)
	ns	<-	NS(id)
	
	# graphHIST	<-	function(IN = NULL)	paste0('graphHIST', ID, IN)
	
	tagList(
		fixedRow(
			column(2, numericInput(ns('BIN'),	label = "Bin Width",	min = 0,	value = BIN.val,	step = BIN.step)),
			column(3, numericInput(ns('MIN'),	label = "X-Minimum",	min = 0,	value = MIN.val)),
			column(3, actionButton(inputId	=	ns('UPD'),	label = "Update Histogram")),
			if (!is.null(LAB))	column(4, textInput(ns('SPEC'),	label = LAB))
		),
		plotOutput(ns('graphHIST'),	height = HEIGHT,
			brush	=	ifBRUSH(brushOpts(id	=	ns('brushHIST'), resetOnNew	=	TRUE, direction	=	"x"))),
	)
}

#	Modality controls and table
modalUI	<-	function(id, BIN.val, BIN.step, LOWER.val, UPPER.val, PER = "Test Period")	{
	ns	<-	NS(id)
	
	tagList(
		fluidRow(	strong(paste0(PER, " Modality")),	actionButton(inputId = ns("modeUPD"),	label = "Update Modes")	),
		fixedRow(
			column(2,	numericInput(ns("modeBIN"),	label = "Bin Width",		min = 0,	value = BIN.val,	step = BIN.step)),
			column(3,	numericInput(ns("modeLOW"),	label = "Lower Limit",		min = 0,	value = LOWER.val)),
			column(3,	numericInput(ns("modeUPP"),	label = "Upper Limit",		min = 0,	value = UPPER.val)),
			# column(3,	numericInput(pasteMODE("num"),	label = "Number of Modes",	min = 1,	value = 1)),
		),
		fluidRow(
			column(3, numericInput(ns("modeNUM"),	label = "Number of Modes",	min = 1,	value = 1)),
			tableOutput(ns("modeTAB")),
			# tableOutput(paste0("mode", as.character(id))),
		),
		HTML("<hr>")
	)
}

#	Histogram brush controls and table
brushUI	<-	function(id, UNIT, STEP, LOWER.max, UPPER.max)	{
	ID	<-	tolower(id)
	ns	<-	NS(id)
	
	brushHIST	<-	function(IN = NULL)	paste0('brushHIST', ID, IN)
	
	tagList(
		strong("Brush Stats"),
		fixedRow(
			column(3, numericInput(ns("brushMIN"),	label = paste0("Lower Limit (", UNIT, ")"),
				value = NULL,	step = STEP,	min = 0,	max = LOWER.max)),
			column(3, numericInput(ns("brushMAX"),	label = paste0("Upper Limit (", UNIT, ")"),
				value = NULL,	step = STEP,	min = 0,	max = UPPER.max))
		),
		tableOutput(ns('brushHISTtab'))
	)
}

HISTtabUI	<-	function(id, SHOW = TRUE, BRUSH = TRUE, MODES = TRUE, ..., label = "Histograms UI")	{
	ns	<-	NS(id)

	if (!SHOW)	return(NULL)

	tabPanel("Histograms",
		tagList(
			tabsetPanel(
				tabPanel("Temperature",
					histUI('TEMP',	BIN.val = 1,	BIN.step = 0.1),
					if (MODES)	modalUI('TEMP',	BIN.val = 1,	BIN.step = 0.1, LOWER.val = 0,		UPPER.val = 95),
					if (BRUSH)	brushUI('TEMP',	"°C",	STEP = 1,	LOWER.max = 115, UPPER.max = 115),
				),
				tabPanel("Frequency",
					histUI('FREQ',	BIN.val = 1,	BIN.step = 0.1, MIN.val = 2000,	LAB = "Frequency Specs (MHz)"),
						# column(4, textInput('SPEC',	label = "Frequency Specs (MHz)"))	),
					if (MODES)	modalUI('FREQ',	BIN.val = 100,	BIN.step = 1,	LOWER.val = 2000,	UPPER.val = 6000),
					if (BRUSH)	brushUI('FREQ',	"MHz",	STEP = 100,	LOWER.max = 6000, UPPER.max = 6000),
				),
				tabPanel("Socket Power",
					histUI('SOCK',	BIN.val = 0.1,	BIN.step = 0.1),
					if (MODES)	modalUI('SOCK',	BIN.val = 1,	BIN.step = 0.1, LOWER.val = 0,		UPPER.val = 200),
					if (BRUSH)	brushUI('SOCK',	"W",	STEP = 1,	LOWER.max = 300, UPPER.max = 300),
				),
				tabPanel("Core Power",
					histUI('CORE',	BIN.val = 0.1,	BIN.step = 0.1),
					if (MODES)	modalUI('CORE',	BIN.val = 1,	BIN.step = 0.1, LOWER.val = 0,		UPPER.val = 20),
					if (BRUSH)	brushUI('CORE',	"W",	STEP = 0.1,	LOWER.max = 300, UPPER.max = 300),
				),
				tabPanel("Uncore Power",
					histUI('UNCORE',	BIN.val = 0.1,	BIN.step = 0.1),
					if (MODES)	modalUI('UNCORE',	BIN.val = 1,	BIN.step = 0.1, LOWER.val = 0,		UPPER.val = 60),
					if (BRUSH)	brushUI('UNCORE',	"W",	STEP = 0.1,	LOWER.max = 300, UPPER.max = 300),
				),
			)
		)
	)
}

#	Time Series Graphs and brush
tseriesUI	<-	function(id, HEIGHT = 480)	{
	ns	<-	NS(id)
	
	# timeTEMP	<-	function(IN = NULL)	paste0('time', toupper(ID), IN)
	# brushTIME	<-	function(IN = NULL)	paste0('brushTIME', ID, IN)
	
	tagList(
		# plotOutput(ns('TStrend'),	height = HEIGHT),
		plotOutput(ns('TStrend'),	height = HEIGHT,	dblclick	=	ifBRUSH(ns('TStrendBRUSHdbl')),
			brush	=	ifBRUSH(brushOpts(id	=	ns('trendBRUSH'),	resetOnNew	=	TRUE, direction	=	"x"))),
		tableOutput(ns('TStrendTAB')),
		plotOutput(ns('TSseas'),	height = HEIGHT,	dblclick	=	ifBRUSH(ns('TSseasBRUSHdbl')),
			brush	=	ifBRUSH(brushOpts(id	=	ns('TSseasBRUSH'),	resetOnNew	=	TRUE, direction	=	"x"))),
		# plotOutput(ns('TSseasBRUSHgraph'),	height = HEIGHT)
		plotOutput(ns('TSseasBRUSHgraph'),	height = HEIGHT),
		actionButton(ns('TSseasBRUSHapp'), "Apply zoom to Other Seasonal Graphs"),
	)
}

TSERIEStabUI	<-	function(id, SHOW = TRUE, BRUSH = TRUE, ..., label = "Time Series UI")	{
	ns	<-	NS(id)

	if (!SHOW)	return(NULL)

	tabPanel("Time Series",
		tagList(
			tabsetPanel(
				tabPanel("Temperature",		tseriesUI('TEMP')	),
				tabPanel("Frequency",		tseriesUI('FREQ')	),
				tabPanel("Socket Power",	tseriesUI('SOCK')	),
				tabPanel("Core Power",		tseriesUI('CORE')	),
			)
		)
	)
}

ui	<-	ui <- function(request)	{fluidPage(
	titlePanel("CPU Temperature Statistics and Graphs"),
	sidebarLayout(
		sidebarPanel(
			# bookmarkButton(),
			selectInput('dataSel',	label	=	"Data to Load",	selectize	=	FALSE,
				# choices	=	setNames(FILES, gsub(".env|.RData", "", names(FILES))), selected	=	FILES[1]
				choices	=	FILES, selected	=	FILES[1]
			),
			actionButton(inputId	=	"dataSelLOAD",	label	=	"Load Selected Data"),
			tabsetPanel(
				tableUI("tableTAB", TRUE, SHOWmulti = VIEW$MULTtab, SHOWcross = VIEW$CROSStab),
				tabPanel("Graphs",
					numericInput('FREQ.COEF',	label	=	"Power-Frequency Coefficient",
						value = GRAPH$FREQ.COEF,	min = 0,	max = GRAPH$FREQ.COEF * 10,	step = GRAPH$FREQ.COEF / 4),
					actionButton('COEFupd',	label = "Apply Coefficient")
				),
				id		=	"Table Controls",
				type	=	"pills"
			)
		),
		mainPanel(
			fixedRow(
				column(8, tableOutput('subTitle')),
				if (is.character(VIEW$YTlink))	column(4, actionButton('tutorial', "Show YouTube Tutorial"))
				),
			tabsetPanel(
				tabPanel("Table",
					tabsetPanel(
						tabPanel("Summary Stats",
							tableOutput("tableSUMM"),
							tablemultoutUI("tableMULI", VIEW$MULTtab),
						),
						if (VIEW$CROSStab)	tabPanel("Temperature Crosses",
							tablecrossoutUI("tableCROS", VIEW$CROSStab)
						)
					)
				),
				GRAPHtabUI("graphTAB",	VIEW$GRAPHS,	VIEW$BRUSH),
				HISTtabUI("histTAB",	VIEW$HIST,		VIEW$BRUSH,	VIEW$MODES),
				TSERIEStabUI("histTAB",	VIEW$TSERIES,	VIEW$BRUSH),
			id	=	"Table",
			type	=	"pills"
			)
		)
	)
)
}