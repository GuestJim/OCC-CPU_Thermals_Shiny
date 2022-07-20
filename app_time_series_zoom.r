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

#	Apply zoom from one graph to the others
observeEvent(input$brushTIMEtempSEASapp,	{
	brushFREQzoomSEAS$x	<-	brushTEMPzoomSEAS$x
	brushSOCKzoomSEAS$x	<-	brushTEMPzoomSEAS$x
	brushCOREzoomSEAS$x	<-	brushTEMPzoomSEAS$x
	
	brushFREQzoomSEAS$CHANGE	<-	TRUE
	brushSOCKzoomSEAS$CHANGE	<-	TRUE
	brushCOREzoomSEAS$CHANGE	<-	TRUE
})
observeEvent(input$brushTIMEfreqSEASapp,	{
	brushTEMPzoomSEAS$x	<-	brushFREQzoomSEAS$x
	brushSOCKzoomSEAS$x	<-	brushFREQzoomSEAS$x
	brushCOREzoomSEAS$x	<-	brushFREQzoomSEAS$x
	
	brushTEMPzoomSEAS$CHANGE	<-	TRUE
	brushSOCKzoomSEAS$CHANGE	<-	TRUE
	brushCOREzoomSEAS$CHANGE	<-	TRUE
})
observeEvent(input$brushTIMEsockSEASapp,	{
	brushTEMPzoomSEAS$x	<-	brushSOCKzoomSEAS$x
	brushFREQzoomSEAS$x	<-	brushSOCKzoomSEAS$x
	brushCOREzoomSEAS$x	<-	brushSOCKzoomSEAS$x
	
	brushTEMPzoomSEAS$CHANGE	<-	TRUE
	brushFREQzoomSEAS$CHANGE	<-	TRUE
	brushCOREzoomSEAS$CHANGE	<-	TRUE
})
observeEvent(input$brushTIMEcoreSEASapp,	{
	brushTEMPzoomSEAS$x	<-	brushCOREzoomSEAS$x
	brushFREQzoomSEAS$x	<-	brushCOREzoomSEAS$x
	brushSOCKzoomSEAS$x	<-	brushCOREzoomSEAS$x
	
	brushTEMPzoomSEAS$CHANGE	<-	TRUE
	brushFREQzoomSEAS$CHANGE	<-	TRUE
	brushSOCKzoomSEAS$CHANGE	<-	TRUE
})

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

#	CPU_Temp
observeEvent(list(input$roundTerm, input$brushTIMEtempTREND),	{
	output$timeTEMPtrendTAB	<-	renderTable(
		trendTABServer("CPU_Temp", input$brushTIMEtempTREND, "°C"),
		digits = input$roundTerm,	rownames = TRUE	)
})

brushTEMPzoomSEAS	=	reactiveValues(x = NULL,	FILTER	=	TRUE,	CHANGE	=	FALSE)
observeEvent(input$brushTIMEtempSEAS, {
	brush 		<- input$brushTIMEtempSEAS

	brushTEMPzoomSEAS$x			<-	NULL
	brushTEMPzoomSEAS$FILTER	<-	TRUE

	brushTEMPzoomSEAS$x			<-	c(brush$xmin, brush$xmax)

	brushTEMPzoomSEAS$CHANGE	<-	TRUE
})

observeEvent(input$brushTIMEtempSEASdbl, {
	brushTEMPzoomSEAS$x			<-	NULL
	brushTEMPzoomSEAS$FILTER	<-	TRUE

	brushTEMPzoomSEAS$CHANGE	<-	TRUE
})

observeEvent(brushTEMPzoomSEAS$x,	{
	req(brushTEMPzoomSEAS$CHANGE)
	
	TS.df	<-	TIME$DF("CPU_Temp")
	output$brushTIMEtempSEAS	=	renderPlot({
		TIME$timeSEAS("CPU_Temp", TS.df = TS.df) + 
		coord_cartesian(xlim = brushTEMPzoomSEAS$x,	expand = TRUE)
	})
})


#	Frequency
observeEvent(list(input$roundTerm, input$brushTIMEfreqTREND),	{
	output$timeFREQtrendTAB	<-	renderTable(
		trendTABServer("Frequency", input$brushTIMEfreqTREND, "MHz"),
		digits = input$roundTerm,	rownames = TRUE	)
})

brushFREQzoomSEAS	=	reactiveValues(x = NULL,	FILTER	=	TRUE,	CHANGE	=	FALSE)
observeEvent(input$brushTIMEfreqSEAS, {
	brush 		<- input$brushTIMEfreqSEAS

	brushFREQzoomSEAS$x			<-	NULL
	brushFREQzoomSEAS$FILTER	<-	TRUE

	brushFREQzoomSEAS$x			<-	c(brush$xmin, brush$xmax)

	brushFREQzoomSEAS$CHANGE	<-	TRUE
})

observeEvent(input$brushTIMEFREQSEASdbl, {
	brushFREQzoomSEAS$x			<-	NULL
	brushFREQzoomSEAS$FILTER	<-	TRUE

	brushFREQzoomSEAS$CHANGE	<-	TRUE
})

observeEvent(brushFREQzoomSEAS$x,	{
	req(brushFREQzoomSEAS$CHANGE)
	
	TS.df	<-	TIME$DF("Frequency")
	output$brushTIMEfreqSEAS	=	renderPlot({
		TIME$timeSEAS("Frequency", TS.df = TS.df) + 
		coord_cartesian(xlim = brushFREQzoomSEAS$x,	expand = TRUE)
	})
})


#	Socket_Energy
observeEvent(list(input$roundTerm, input$brushTIMEsockTREND),	{
	output$timeSOCKtrendTAB	<-	renderTable(
		trendTABServer("Socket_Energy", input$brushTIMEsockTREND, "W"),
		digits = input$roundTerm,	rownames = TRUE	)
})

brushSOCKzoomSEAS	=	reactiveValues(x = NULL,	FILTER	=	TRUE,	CHANGE	=	FALSE)
observeEvent(input$brushTIMEsockSEAS, {
	brush 		<- input$brushTIMEsockSEAS

	brushSOCKzoomSEAS$x			<-	NULL
	brushSOCKzoomSEAS$FILTER	<-	TRUE

	brushSOCKzoomSEAS$x			<-	c(brush$xmin, brush$xmax)

	brushSOCKzoomSEAS$CHANGE	<-	TRUE
})

observeEvent(input$brushTIMESOCKSEASdbl, {
	brushSOCKzoomSEAS$x			<-	NULL
	brushSOCKzoomSEAS$FILTER	<-	TRUE

	brushSOCKzoomSEAS$CHANGE	<-	TRUE
})

observeEvent(brushSOCKzoomSEAS$x,	{
	req(brushSOCKzoomSEAS$CHANGE)
	
	TS.df	<-	TIME$DF("Socket_Energy")
	output$brushTIMEsockSEAS	=	renderPlot({
		TIME$timeSEAS("Socket_Energy", TS.df = TS.df) + 
		coord_cartesian(xlim = brushSOCKzoomSEAS$x,	expand = TRUE)
	})
})


#	Core_Energy
observeEvent(list(input$roundTerm, input$brushTIMEcoreTREND),	{
	output$timeCOREtrendTAB	<-	renderTable(
		trendTABServer("Core_Energy", input$brushTIMEcoreTREND, "W"),
		digits = input$roundTerm,	rownames = TRUE	)
})

brushCOREzoomSEAS	=	reactiveValues(x = NULL,	FILTER	=	TRUE,	CHANGE	=	FALSE)
observeEvent(input$brushTIMEcoreSEAS, {
	brush 		<- input$brushTIMEcoreSEAS

	brushCOREzoomSEAS$x			<-	NULL
	brushCOREzoomSEAS$FILTER	<-	TRUE

	brushCOREzoomSEAS$x			<-	c(brush$xmin, brush$xmax)

	brushCOREzoomSEAS$CHANGE	<-	TRUE
})

observeEvent(input$brushTIMECORESEASdbl, {
	brushCOREzoomSEAS$x			<-	NULL
	brushCOREzoomSEAS$FILTER	<-	TRUE

	brushCOREzoomSEAS$CHANGE	<-	TRUE
})

observeEvent(brushCOREzoomSEAS$x,	{
	req(brushCOREzoomSEAS$CHANGE)
	
	TS.df	<-	TIME$DF("Core_Energy")
	output$brushTIMEcoreSEAS	=	renderPlot({
		TIME$timeSEAS("Core_Energy", TS.df = TS.df) + 
		coord_cartesian(xlim = brushCOREzoomSEAS$x,	expand = TRUE)
	})
})