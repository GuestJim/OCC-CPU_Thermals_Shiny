#	in theory this might be able to work, but simply nothing about it really did
#	originally I tried to have the zoomed graph included in this code, which would not be too difficult to replicate
#	eventually I abandoned that because it simply refused to work, but hoped I could get this different approach to work
#	alas, it proved even less reliable, even though I can see no reason for that
#	this is supposed to use the namespace stuff and the server function to get the X limits to then feed to normal calls for the graphs
#	it never wanted to update the brush values correctly or work properly with cropping the graphs
#	in other words, it is a failure and I could not hazard a guess for why beyond that reactives are too quirky
#		for example, you'll notice my having to use 

graphUI	<-	function(name, BRUSH, HEIGHT = 720, START = -300, LENGTH = 7500)	{
	ns	<-	NS(name)
	
	tagList(
		plotOutput(paste0("graph", as.character(name)),	height = HEIGHT,	dblclick	=	ifBRUSH(ns("graphBRUSHdbl")),
			brush	=	ifBRUSH(brushOpts(id	=	ns("graphBRUSH"), resetOnNew	=	TRUE, direction	=	"x"))),
		if (BRUSH)	tagList(	strong("Click and Drag to Zoom Below"),
			fixedRow(
				column(3,	numericInput(inputId	=	ns("start"),
					value = START, 	label	=	"Zoom Start (s)",	step	=	1)
					),
				column(3,	numericInput(inputId	=	ns("length"),
					value = LENGTH,	label	=	"Zoom Length (s)",	step	=	1)
					),
				column(3,	actionButton(inputId	=	ns("update"), label = "Update Zoom"))
			)
		)
	)
}

graphBRUSHServer	<-	function(name)	{
	WARM	<-	DATA$warm	;	DURATION	<-	DATA$duration	;	GRAPH <- GRAPH	;	moduleServer(name, function(input, output, session)	{
	updateNumericInput(inputId	=	"start",	value = -WARM)
	updateNumericInput(inputId	=	"length",	value = 2 * DURATION + WARM)

	brush	<-	reactiveVal(NA)
	
	observeEvent(input$graphBRUSH,	{
		brush(	unlist(input$graphBRUSH[c("xmin", "xmax")])	)
		SUB	<-	labs(subtitle = paste0("X: ", paste(round(isolate(brush()), 2), collapse = " to "), " (s)")	)

		updateNumericInput(inputId	=	"start",	value = round(	brush()[[1]]				, 2)	)
		updateNumericInput(inputId	=	"length",	value = round(	brush()[[2]] - brush()[[1]]	, 2)	)
	},	ignoreInit = TRUE	)

	observeEvent(input$update,	{
		brush(	c(input$start, input$length + input$start)	)
	})
	output$test	<-	renderText(	brush()	)
	
	# output$graphZOOM	<-	renderPlot({
		# if (!isTruthy(brush()))	{
			# GRAPH$graphMEAN(GRAPH$FREQ.COEF) +
			# coord_cartesian(xlim = brush(),	expand = FALSE) +
			# labs(subtitle = paste0("X: ", paste(round(brush(), 2), collapse = " to "), " (s)")	)
		# }
	# }	)
	if (!anyNA(	brush()	))	return(	reactive(	brush()	)	)
	return(NA)
})}

##	the plot within the module works, but getting it to work outside is proving stupidly impossible. The output of the module will change, but the render functions completely ignore that

observeEvent(input$dataSelLOAD,	{
	brushMEAN	<-	graphBRUSHServer("MEAN")
	
	output$test	<-	renderText(	brushMEAN	)
	# observeEvent(reactive(	brushMEAN	),	{
		# output$brushMEANzoom	<-	renderPlot({
			# if (!isTruthy(brushMEAN()))	{
				# GRAPH$graphMEAN(GRAPH$FREQ.COEF) +
				# coord_cartesian(xlim = brushMEAN,	expand = FALSE) +
				# labs(subtitle = paste0("X: ", paste(round(brushMEAN, 2), collapse = " to "), " (s)")	)
			# }
		# })
	# })
})