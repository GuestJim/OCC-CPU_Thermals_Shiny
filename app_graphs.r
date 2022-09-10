GRAPH$base	<-	function(COEF = DATA$FREQ.COEF, FILT = TRUE)	{
	ggplot(data = DATA$dataALL[FILT, ], aes(x=Time)) + GRAPH$CAPTION +
	GRAPH$themeSCALES(COEF)
}

GRAPH$graphMEAN	=	function(COEF = DATA$FREQ.COEF, FILT = TRUE)	{
	GRAPH$base(COEF, FILT) +
	ggtitle("Mean Frequency with Temperature and Power") + 
	GRAPH$TEMP_point() +
	GRAPH$SOCK_point() +
	GRAPH$FREQ_point(COEF = COEF, MEAN = TRUE) + 
	ylab("Temperature (°C) and Power (W)")
}

GRAPH$graphMAX	=	function(COEF = DATA$FREQ.COEF, FILT = TRUE)	{
	GRAPH$base(COEF, FILT) +
	ggtitle("Max Frequency with Temperature and Power") + GRAPH$CAPTION +
	GRAPH$TEMP_point() +
	GRAPH$SOCK_point() +
	GRAPH$FREQ_point(COEF = COEF, MAX = TRUE) +
	ylab("Temperature (°C) and Power (W)")
}

GRAPH$graphFREQ	=	function(COEF = FREQ.COEF, FILT = TRUE)	{
	GRAPH$base(COEF, FILT) +
	ggtitle("Frequency with Temperature and Core Power",
		subtitle = "Even Thread: Physical, Odd Thread: Logical") + 
	GRAPH$TEMP_point() +
	# GRAPH$SOCK_point() +
	GRAPH$CORE_point() +
	GRAPH$FREQ_point(COEF = COEF) +
	ylab("Temperature (°C) and Power (W)")
}
GRAPH$facetFREQ	=	facet_grid(rows = vars(Core, Thread),	switch = "y", labeller	=
		labeller(Core	=	function(IN) paste0("Core: ", IN), Thread	=	function(IN) paste0("Thread: ", IN))
		)

GRAPH$graphPOWER	=	function(COEF = FREQ.COEF, FILT = TRUE)	{
	GRAPH$base(COEF, FILT) +
	ggtitle("Frequency with Core and Uncore Power",	subtitle = "") + 
	GRAPH$FREQ_point(COEF = COEF) +
	GRAPH$unCORE_point() +
	GRAPH$CORE_point() +
	ylab("Power (W)") + expand_limits(y = c(0, 90))
}
GRAPH$facetPOWER	=	facet_grid(rows = vars(Core),	switch = "y", labeller	=
		labeller(Core	=	function(IN) paste0("Core: ", IN))
		)

observeEvent(input$COEFupd,	{
	if (input$FREQ.COEF != 0)	GRAPH$FREQ.COEF	<-	input$FREQ.COEF
	})

observeEvent(list(input$dataSelLOAD, input$COEFupd),	{
	output$graphMEAN	=	renderPlot({	GRAPH$graphMEAN(GRAPH$FREQ.COEF)	})
	output$graphMAX		=	renderPlot({	GRAPH$graphMAX(GRAPH$FREQ.COEF)		})
	output$graphTHREAD	=	renderPlot({	GRAPH$graphFREQ(GRAPH$FREQ.COEF) + GRAPH$facetFREQ		},
		height = 720/3 * length(unique(DATA$dataALL$Thread))	)
	output$graphPOWER	=	renderPlot({	GRAPH$graphPOWER(GRAPH$FREQ.COEF) + GRAPH$facetPOWER	},
		height = 720/3 * length(unique(DATA$dataALL$Core))	)
})

if (VIEW$BRUSH)	source("app_graphs_zoom.r", local = TRUE)