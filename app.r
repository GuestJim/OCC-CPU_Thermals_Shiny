library(shiny)
# runapp()
# options(shiny.error = browser)
# options(shiny.reactlog = TRUE)
# options(shiny.maxRequestSize = 100*1024^2)
options(encoding = 'UTF-8')
#	Shiny has a normal limit of 5 MB, so this raises it to 100 MB

DATA	=	new.env()
DATA$LOAD	=	FALSE	#used for tracking if data has been loaded automatically
GRAPH	=	new.env()
TABLE	=	new.env()
BRUSH	=	new.env()
VIEW	=	new.env()
app.BREAK	=	TRUE
#	rather than using super-assignment and pushing variables to Global, I'm putting them into this environment
#	this keeps DATA within the Shiny environment too, so when Shiny ends, the data is apparently removed, which I'm good with

VIEW$YTlink	=	"LJwyCXedkYY"	#	ID string for YouTube tutorial video
								#	if not a string, it will be disabled
VIEW$MULTtab	=	TRUE	#	control if the tables should be separated or not
VIEW$CROSStab	=	TRUE	#	control if Temperature Crossing tables are shown
VIEW$DOWN	=	FALSE	#	control if it should be possible to download tables
						#	usually it would be !exists("FILE", envir=DATA)
VIEW$GRAPHS		=	TRUE	#	control if Graphs should be included or not
VIEW$HIST		=	TRUE	#	control if Histograms should be included or not
VIEW$MODES		=	TRUE	#	control if the Modality controls should be included
VIEW$TSERIES	=	TRUE	#	control if Time Series graphs should be included or not
VIEW$BRUSH		=	TRUE	#	control if Zoomed/Single Graphs should be included or not

source("app_functions.r", local	=	TRUE)

GRAPH$FREQ.COEF	=	100		#	default value for the Power Frequency coefficient

FILES		=	list.files(pattern = "*.env$|*.RData$",	recursive = FALSE)
FILES.names	=	unlist(sapply(FILES, strsplit, "/"), use.names = FALSE)
# FILES		=	setNames(FILES, FILES.names[grepl(".env|.RData", FILES.names)])

FILES	=	as.data.frame(cbind(
	Test	=	sapply(FILES.names, function(IN) strsplit(IN, " ~ ")[[1]][1]),
	Config	=	gsub(".RData", "", sapply(FILES.names, function(IN) strsplit(IN, " ~ ")[[1]][2])),
	File	=	FILES.names
))
rownames(FILES) <- NULL
FILES	=	as.list(by(FILES[, c("File")], FILES$Config, identity))

mergeENV	=	function(env1, env2)	for (obj in ls(env2, all.names = TRUE))	assign(obj, get(obj, env2), envir = env1)

source("app_UI.r", local = TRUE)

# Define server logic to summarize and view selected dataset ----
server <- function(input, output, session) {
	setBookmarkExclude(c("COEFupd", "dataSelLOAD", "engPOW", "FREQ.COEF", "listMEAS", "listPERI", "listSTAT", "medOFFapply", "roundTerm", "Table", "Table Controls", "tableCROSScool", "tableCROSSlim", "tableCROSStest", "tabORDER", "tutorial", "MAX-length", "MAX-start", "MAX-update", "MEAN-length", "MEAN-start", "MEAN-update", "THREAD-length", "THREAD-SEL", "THREAD-SELapply", "THREAD-start", "THREAD-update", "POWER-length", "POWER-SEL", "POWER-SELapply", "POWER-start", "POWER-update", "CORE-BIN", "CORE-brushMAX", "CORE-brushMIN", "CORE-ECDF", "CORE-MIN", "CORE-modeBIN", "CORE-modeLOW", "CORE-modeNUM", "CORE-modeUPD", "CORE-modeUPP", "CORE-PERC", "CORE-TSseasBRUSHapp", "CORE-UPD", "FREQ-BIN", "FREQ-brushMAX", "FREQ-brushMIN", "FREQ-ECDF", "FREQ-MIN", "FREQ-modeBIN", "FREQ-modeLOW", "FREQ-modeNUM", "FREQ-modeUPD", "FREQ-modeUPP", "FREQ-PERC", "FREQ-SPEC", "FREQ-TSseasBRUSHapp", "FREQ-UPD", "SOCK-BIN", "SOCK-brushMAX", "SOCK-brushMIN", "SOCK-ECDF", "SOCK-MIN", "SOCK-modeBIN", "SOCK-modeLOW", "SOCK-modeNUM", "SOCK-modeUPD", "SOCK-modeUPP", "SOCK-PERC", "SOCK-TSseasBRUSHapp", "SOCK-UPD", "TEMP-BIN", "TEMP-brushMAX", "TEMP-brushMIN", "TEMP-ECDF", "TEMP-MIN", "TEMP-modeBIN", "TEMP-modeLOW", "TEMP-modeNUM", "TEMP-modeUPD", "TEMP-modeUPP", "TEMP-PERC", "TEMP-TSseasBRUSHapp", "TEMP-UPD", "UNCORE-BIN", "UNCORE-brushMAX", "UNCORE-brushMIN", "UNCORE-MIN", "UNCORE-modeBIN", "UNCORE-modeLOW", "UNCORE-modeNUM", "UNCORE-modeUPD", "UNCORE-modeUPP", "UNCORE-UPD"))
	TESTconfig	=	eventReactive(input$dataSelLOAD, {
		ifexist	=	function(IN, ENV = DATA)	{if (exists(IN, envir = ENV))	return(get(IN, envir = ENV))	;	return("")}
		config	=	c(
			"CPU"		=	ifexist("CPUname", DATA),
			"Cooler"	=	ifexist("COOLERname", DATA),
			"CPU Load"	=	ifexist("TESTname", DATA)
			)
		cbind(names(config), config)
		},	ignoreNULL = FALSE)
	
	observeEvent(input$tutorial,	{
		showModal(	modalDialog(
			HTML(paste0('<iframe width="560" height="315" src="https://www.youtube.com/embed/', VIEW$YTlink,'" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe><br><a href="https://www.youtube.com/watch?v=', VIEW$YTlink, '" target="_blank">YouTube Link</a> (Some features may appear different)')),
			title = "YouTube Tutorial Video",
			easyClose = TRUE,	footer = modalButton("Close")	)	)
	})
	
	output$subTitle	=	renderTable({TESTconfig()}, rownames = FALSE, colnames = FALSE, align = 'rl')
	observeEvent(input$dataSelLOAD,	{
		mergeENV(DATA, readRDS(input$dataSel))
		DATA$LOAD	=	TRUE

		DATA$maxPWR		=	nearCEIL(DATA$dataALL$Socket_Energy,	5000)
		DATA$maxCLK		=	nearCEIL(DATA$dataALL$Frequency,		500)
		if	(!is.numeric(DATA$FREQ.COEF))	DATA$FREQ.COEF	=	signif(exp(round(log(DATA$maxPWR/DATA$maxCLK / 1000), 0)), 1)
		
		if (DATA$FREQ.COEF < 1)	DATA$FREQ.COEF	<-	1 / DATA$FREQ.COEF
		
		DATA$warmMED	=	median(DATA$dataALL[DATA$dataALL$Period == "Warm-up", "CPU_Temp"])
		updateCheckboxInput(inputId = "medOFFapply", label = paste0("Subtract Median Warm-up Temp (", DATA$warmMED, " °C)"))

		GRAPH$CAPTION	=	c(DATA$CPUname, DATA$COOLERname,	ifelse(is.null(DATA$PULSE), DATA$TESTname, paste0(DATA$TESTname, " (", DATA$PULSE, " s)"))	)
		GRAPH$CAPTION	=	labs(caption = paste(GRAPH$CAPTION, collapse = "\n"))

		updateNumericInput(		inputId	=	"FREQ.COEF",
			value	=	DATA$FREQ.COEF)
		updateNumericInput(		inputId	=	"graphHISTfreqMIN",
			value	=	round(min(DATA$dataALL$Frequency)-500, -3))

		updateCheckboxGroupInput(	inputId	=	"listPERI",
			choices		=	DATA$levsPER,		selected	=	DATA$levsPER		)
		updateCheckboxGroupInput(	inputId	=	"listMEAS",
			choiceValues	=	names(DATA$DATAS),	choiceNames		=	rem_(names(DATA$DATAS)),
			selected		=	names(DATA$DATAS)[!grepl("Uncore_Energy", names(DATA$DATAS))]	)
		updateCheckboxGroupInput(	inputId	=	"listSTAT",
			choices		=	names(stats(0)),	selected	=	names(stats(0))		)
	},	label = "Data Loading",	priority	=	10)

	source("app_tables.r", local = TRUE)
	if (any(c(VIEW$GRAPHS, VIEW$HIST, VIEW$TSERIES)))	source("app_graphs_layers.r", local = TRUE)
	if (VIEW$GRAPHS)	source("app_graphs.r", local = TRUE)
	if (VIEW$HIST)		source("app_histograms.r", local = TRUE)
	if (VIEW$TSERIES)	source("app_time_series.r", local = TRUE)
}

# Create Shiny app ----
shinyApp(ui = ui, server = server, enableBookmarking = "url")