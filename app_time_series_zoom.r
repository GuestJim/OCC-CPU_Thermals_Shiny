# brushTEMPzoomTREND	=	reactiveValues(x = NULL,	FILTER	=	TRUE,	CHANGE	=	FALSE)
# observeEvent(input$brushTIMEtempTREND, {	req(DATA$dataALL)
	# brush 		<- input$brushTIMEtempTREND
	# brushFILT	<-	setNames(brush[grep("panelvar", names(brush))], brush$mapping[grep("panelvar", names(brush$mapping))]	)

	# brushTEMPzoomTREND$x		<-	NULL
	# brushTEMPzoomTREND$FILTER	<-	TRUE

	# brushTEMPzoomTREND$x	<-	c(brush$xmin, brush$xmax)

	# brushTEMPzoomTREND$CHANGE	<-	TRUE
# })

# observeEvent(list(input$brushTIMEtempTRENDdbl, input$dataSelLOAD), {
	# brushTEMPzoomTREND$x		<-	NULL
	# brushTEMPzoomTREND$FILTER	<-	TRUE

	# brushTEMPzoomSEAS$CHANGE	<-	TRUE
# })

# observeEvent(list(input$brushTIMEtempTREND, input$brushTIMEtempTRENDdbl),	{
	# req(DATA$dataALL, brushTEMPzoomTREND$CHANGE)
	# output$brushTIMEtempTREND	=	renderPlot({
		# TIME$timeTREND("CPU_Temp", TS.df = TS.df) + 
		# coord_cartesian(xlim = brushTEMPzoomTREND$x,	expand = FALSE)
	# })
# })

#	The above can be used for zooming into a TREND graph, but I do not think this is as necessary, so it is disabled
#	the above is also out of date

#	Wipe zooms when new data loaded
observeEvent(input$dataSelLOAD,	{
	output$brushTIMEtempSEAS	=	NULL
	output$brushTIMEfreqSEAS	=	NULL
	output$brushTIMEsockSEAS	=	NULL
	output$brushTIMEcoreSEAS	=	NULL
	
	output$timeTEMPtrendTAB		<-	NULL
	output$timeFREQtrendTAB		<-	NULL
	output$timeSOCKtrendTAB		<-	NULL
	output$timeCOREtrendTAB		<-	NULL
})

#	holder for all brushes
brushSEAS	<-	reactiveValues(
	TEMP	=	c(xmin = -Inf, xmax = Inf),
	FREQ	=	c(xmin = -Inf, xmax = Inf),
	SOCK	=	c(xmin = -Inf, xmax = Inf),
	CORE	=	c(xmin = -Inf, xmax = Inf)
)

#	observers to update brushSEAS values
observeEvent(input$brushTIMEtempSEAS,	brushSEAS$TEMP	<-	input$brushTIMEtempSEAS[c("xmin", "xmax")])
observeEvent(input$brushTIMEfreqSEAS,	brushSEAS$FREQ	<-	input$brushTIMEfreqSEAS[c("xmin", "xmax")])
observeEvent(input$brushTIMEsockSEAS,	brushSEAS$SOCK	<-	input$brushTIMEsockSEAS[c("xmin", "xmax")])
observeEvent(input$brushTIMEcoreSEAS,	brushSEAS$CORE	<-	input$brushTIMEcoreSEAS[c("xmin", "xmax")])

#	observers to change brushSEAS values when buttons are pressed
observeEvent(input$brushTIMEtempSEASapp,	{	VAL	<-	brushSEAS$TEMP
	brushSEAS$TEMP	<-	VAL	;	brushSEAS$FREQ	<-	VAL	;	brushSEAS$SOCK	<-	VAL	;	brushSEAS$CORE	<-	VAL
	})
observeEvent(input$brushTIMEfreqSEASapp,	{	VAL	<-	brushSEAS$FREQ
	brushSEAS$TEMP	<-	VAL	;	brushSEAS$FREQ	<-	VAL	;	brushSEAS$SOCK	<-	VAL	;	brushSEAS$CORE	<-	VAL
	})
observeEvent(input$brushTIMEsockSEASapp,	{	VAL	<-	brushSEAS$SOCK
	brushSEAS$TEMP	<-	VAL	;	brushSEAS$FREQ	<-	VAL	;	brushSEAS$SOCK	<-	VAL	;	brushSEAS$CORE	<-	VAL
	})
observeEvent(input$brushTIMEcoreSEASapp,	{	VAL	<-	brushSEAS$CORE
	brushSEAS$TEMP	<-	VAL	;	brushSEAS$FREQ	<-	VAL	;	brushSEAS$SOCK	<-	VAL	;	brushSEAS$CORE	<-	VAL
	})

#	module for the Trend Min/Max tables
trendTABServer	<-	function(id, BRUSH, UNIT = NULL)	{moduleServer(id, function(input, output, session)	{
	brushTREND		<-	list(
		x	=	NULL,	FILTER	=	TRUE,
		TAB	=	as.data.frame(matrix(c("", ""), nrow = 1, ncol = 2, dimnames = list(c(UNIT), c("Minimum", "Maximum"))))
		)
	if (is.null(BRUSH$xmin))	return(	brushTREND$TAB	)
	
	TS.df	<-	TIME$DF(as.character(id))
	
	brushTREND$x	<-	c(BRUSH$xmin, BRUSH$xmax)
	brushTREND$FILT	<-	!is.na(cut(TS.df$Index, brushTREND$x, labels = FALSE, include.lowest = TRUE))
	brushTREND$TAB$Minimum	<-	min(TS.df[brushTREND$FILT, "Trend"],	na.rm = TRUE)
	brushTREND$TAB$Maximum	<-	max(TS.df[brushTREND$FILT, "Trend"],	na.rm = TRUE)
	
	return(brushTREND$TAB)
})}

#	module for the Zoomed Seasonal graphs
seasPLOTServer	<-	function(id, BRUSH)	{moduleServer(id, function(input, output, session)	{
	brushSEAS	<-	list(x = c(-Inf, Inf),	FILTER = TRUE)
	if (is.null(BRUSH$xmin))	return(	TIME$timeSEAS(as.character(id), TS.df = TS.df)	)
	
	TS.df	<-	TIME$DF(as.character(id))
	
	brushSEAS$x	<-	c(BRUSH$xmin, BRUSH$xmax)
	
	TIME$timeSEAS(as.character(id), TS.df = TS.df) + coord_cartesian(xlim = brushSEAS$x,	expand = TRUE)
})}


#	CPU_Temp
observeEvent(list(input$roundTerm, input$brushTIMEtempTREND),	{
	output$timeTEMPtrendTAB	<-	renderTable(
		trendTABServer("CPU_Temp", input$brushTIMEtempTREND, "°C"),
		digits = input$roundTerm,	rownames = TRUE	)
})

observeEvent(brushSEAS$TEMP,	{	output$brushTIMEtempSEAS	=	renderPlot(	seasPLOTServer("CPU_Temp", brushSEAS$TEMP)	)	})

#	Frequency
observeEvent(list(input$roundTerm, input$brushTIMEfreqTREND),	{
	output$timeFREQtrendTAB	<-	renderTable(
		trendTABServer("Frequency", input$brushTIMEfreqTREND, "MHz"),
		digits = input$roundTerm,	rownames = TRUE	)
})

observeEvent(brushSEAS$FREQ,	{	output$brushTIMEfreqSEAS	=	renderPlot(	seasPLOTServer("Frequency", brushSEAS$FREQ)	)	})

#	Socket_Energy
observeEvent(list(input$roundTerm, input$brushTIMEsockTREND),	{
	output$timeSOCKtrendTAB	<-	renderTable(
		trendTABServer("Socket_Energy", input$brushTIMEsockTREND, "W"),
		digits = input$roundTerm,	rownames = TRUE	)
})

observeEvent(brushSEAS$SOCK,	{	output$brushTIMEsockSEAS	=	renderPlot(	seasPLOTServer("Socket_Energy", brushSEAS$SOCK)	)	})

#	Core_Energy
observeEvent(list(input$roundTerm, input$brushTIMEcoreTREND),	{
	output$timeCOREtrendTAB	<-	renderTable(
		trendTABServer("Core_Energy", input$brushTIMEcoreTREND, "W"),
		digits = input$roundTerm,	rownames = TRUE	)
})

observeEvent(brushSEAS$CORE,	{	output$brushTIMEcoreSEAS	=	renderPlot(	seasPLOTServer("Core_Energy", brushSEAS$CORE)	)	})
