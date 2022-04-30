GRAPH$graphMEAN	=	function(COEF = DATA$FREQ.COEF, FILT = TRUE)	{
	ggplot(data = DATA$dataALL[FILT, ], aes(x=Time)) +
	ggtitle("Mean Frequency with Temperature and Power") + GRAPH$CAPTION +
	GRAPH$TEMP_point() +
	GRAPH$SOCK_point() +
	GRAPH$FREQ_point(COEF = COEF, MEAN = TRUE) +
	GRAPH$themeSCALES(COEF) + ylab("Temperature (°C) and Power (W)")
}

GRAPH$graphMAX	=	function(COEF = DATA$FREQ.COEF, FILT = TRUE)	{
	ggplot(data = DATA$dataALL[FILT, ], aes(x=Time)) +
	ggtitle("Max Frequency with Temperature and Power") + GRAPH$CAPTION +
	GRAPH$TEMP_point() +
	GRAPH$SOCK_point() +
	GRAPH$FREQ_point(COEF = COEF, MAX = TRUE) +
	GRAPH$themeSCALES(COEF) + ylab("Temperature (°C) and Power (W)")
}

GRAPH$graphFREQ	=	function(COEF = FREQ.COEF, FILT = TRUE)	{
	ggplot(data = DATA$dataALL[FILT, ], aes(x=Time)) +
	ggtitle("Frequency with Temperature and Core Power",
		subtitle = "Even Thread: Physical, Odd Thread: Logical") + GRAPH$CAPTION +
	GRAPH$TEMP_point() +
	# GRAPH$SOCK_point() +
	GRAPH$CORE_point() +
	GRAPH$FREQ_point(COEF = COEF) +
	GRAPH$themeSCALES(COEF) + ylab("Temperature (°C) and Power (W)") + 
	NULL
	# facet_grid(rows = vars(Core, Thread),	switch = "y", labeller	=
		# labeller(Core	=	function(IN) paste0("Core: ", IN), Thread	=	function(IN) paste0("Thread: ", IN))
		# )
}
GRAPH$facetFREQ	=	facet_grid(rows = vars(Core, Thread),	switch = "y", labeller	=
		labeller(Core	=	function(IN) paste0("Core: ", IN), Thread	=	function(IN) paste0("Thread: ", IN))
		)

GRAPH$graphPOWER	=	function(COEF = FREQ.COEF, FILT = TRUE)	{
	ggplot(data = DATA$dataALL[FILT, ], aes(x=Time)) +
	ggtitle("Frequency with Core and Uncore Power",	subtitle = "") + GRAPH$CAPTION +
	GRAPH$FREQ_point(COEF = COEF) +
	GRAPH$unCORE_point() +
	GRAPH$CORE_point() +
	GRAPH$themeSCALES(COEF) + ylab("Power (W)") + expand_limits(y = c(0, 90)) +
	NULL
	# facet_grid(rows = vars(Core),	switch = "y", labeller	=
		# labeller(Core	=	function(IN) paste0("Core: ", IN))
		# )
}
GRAPH$facetPOWER	=	facet_grid(rows = vars(Core),	switch = "y", labeller	=
		labeller(Core	=	function(IN) paste0("Core: ", IN))
		)

observeEvent(list(input$dataSelLOAD, DATA$LOAD, input$FREQ.COEF) ,{
	output$graphMEAN	=	renderPlot({
		req(DATA$dataALL)
		GRAPH$graphMEAN(input$FREQ.COEF)
	})
	output$graphMAX	=	renderPlot({
		req(DATA$dataALL)
		GRAPH$graphMAX(input$FREQ.COEF)
	})
	output$graphFREQ	=	renderPlot({
		req(DATA$dataALL)
		GRAPH$graphFREQ(input$FREQ.COEF) + GRAPH$facetFREQ
	},	height = 720/3 * length(unique(DATA$dataALL$Thread))	)
	output$graphPOWER	=	renderPlot({
		req(DATA$dataALL)
		GRAPH$graphPOWER(input$FREQ.COEF) + GRAPH$facetPOWER
	},	height = 720/3 * length(unique(DATA$dataALL$Core))	)
})

if (VIEW$BRUSH)	source("app_graphs_zoom.r", local = TRUE)