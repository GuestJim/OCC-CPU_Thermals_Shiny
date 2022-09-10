graphZOOMServer	<-	function(name, GRAPH)	{
	WARM	<-	DATA$warm	;	DURATION	<-	DATA$duration
	moduleServer(name, function(input, output, session)	{
		updateNumericInput(inputId	=	"start",	value = -WARM)
		updateNumericInput(inputId	=	"length",	value = 2 * DURATION + WARM)

		brush	<-	reactiveVal(NA)
		
		observeEvent(input$graphBRUSH,	{
			brush(	unlist(input$graphBRUSH[c("xmin", "xmax")])	)

			updateNumericInput(inputId	=	"start",	value = round(	brush()[[1]]				, 2)	)
			updateNumericInput(inputId	=	"length",	value = round(	brush()[[2]] - brush()[[1]]	, 2)	)
		},	ignoreInit = TRUE	)

		observeEvent(input$update,	{
			brush(	c(input$start, input$length + input$start)	)
		})
		
		output$graphZOOM	<-	renderPlot({
			if (!anyNA(brush()))	{
				GRAPH +
				coord_cartesian(xlim = brush(),	expand = FALSE) +
				labs(subtitle = paste0("X: ", paste(round(brush(), 2), collapse = " to "), " (s)")	)
			}
		}	)
		
		if (!anyNA(brush()))	return(brush())
})}

graphSELECTServer	<-	function(name, GRAPH, TYPE,	HEIGHT = 480)	{
	WARM <- DATA$warm	;	DURATION <- DATA$duration
	IN <- DATA$dataALL	;	THREAD <- unique(DATA$dataALL$Thread)	;	CORE <- unique(DATA$dataALL$Core)
	moduleServer(name, function(input, output, session)	{
		updateNumericInput(inputId	=	"start",	value = -WARM)
		updateNumericInput(inputId	=	"length",	value = 2 * DURATION + WARM)
		
		if (TYPE == "Thread")	SELECTION	=	setNames(THREAD,	paste0("Thread ", THREAD))
		if (TYPE == "Core")		SELECTION	=	setNames(CORE,		paste0("Core ", CORE))
		updateCheckboxGroupInput(inputId = "SEL",	choices = SELECTION,	selected = SELECTION,
			label = paste0(TYPE, " Selected"), inline = TRUE)
		

		brush	<-	reactiveVal(NA)
		
		observeEvent(input$graphBRUSH,	{
			brush(	unlist(input$graphBRUSH[c("xmin", "xmax")])	)

			updateNumericInput(inputId	=	"start",	value = round(	brush()[[1]]				, 2)	)
			updateNumericInput(inputId	=	"length",	value = round(	brush()[[2]] - brush()[[1]]	, 2)	)
		},	ignoreInit = TRUE	)

		observeEvent(input$update,	{
			brush(	c(input$start, input$length + input$start)	)
		})
		
		hold	<-	lapply(SELECTION, function(i)	{
			tagList(
				strong(paste0(TYPE, ": ", i)),
				renderPlot({
					GRAPH(IN[, TYPE] == i) +
					coord_cartesian(xlim = brush(),	expand = FALSE) +
					labs(subtitle = paste0("X: ", paste(round(brush(), 2), collapse = " to "), " (s)")	)
				},	height = HEIGHT)
			)
		})
		observeEvent(input$SELapply,	{
			output$graphZOOM	<-	renderUI(	hold[as.numeric(isolate(input$SEL)) + 1]	)
		})
		
		if (!anyNA(brush()))	return(brush())
})}

observeEvent(list(input$dataSelLOAD, input$COEFupd),	{
	graphZOOMServer("MEAN",		GRAPH$graphMEAN(GRAPH$FREQ.COEF))
	graphZOOMServer("MAX",		GRAPH$graphMAX(GRAPH$FREQ.COEF))
	
	graphSELECTServer("THREAD",		function(FILT)	{GRAPH$graphFREQ(GRAPH$FREQ.COEF, FILT)},	"Thread",	HEIGHT = 480)
	graphSELECTServer("POWER",		function(FILT)	{GRAPH$graphPOWER(GRAPH$FREQ.COEF, FILT)},	"Core",		HEIGHT = 480)
},	ignoreInit = TRUE	)
