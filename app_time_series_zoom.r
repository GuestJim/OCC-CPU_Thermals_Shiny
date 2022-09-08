# module for the Trend Min/Max tables
TSbrushTABServer	<-	function(id, TS.df,	UNIT = NULL)	{moduleServer(id, function(input, output, session)	{
	brushTREND		<-	list(
		x	=	NULL,	FILTER	=	TRUE,
		TAB	=	as.data.frame(matrix(c("", ""), nrow = 1, ncol = 2, dimnames = list(c(UNIT), c("Minimum", "Maximum"))))
		)
	if (is.null(input$trendBRUSH$xmin))	return(	brushTREND$TAB	)
	
	brushTREND$x	<-	c(input$trendBRUSH$xmin, input$trendBRUSH$xmax)
	brushTREND$FILT	<-	cutWithin(TS.df$Index, brushTREND$x)
	brushTREND$TAB$Minimum	<-	min(TS.df[brushTREND$FILT, "Trend"],	na.rm = TRUE)
	brushTREND$TAB$Maximum	<-	max(TS.df[brushTREND$FILT, "Trend"],	na.rm = TRUE)
	
	return(brushTREND$TAB)
})}

output$"TEMP-TStrendTAB"	<-	renderTable(	TSbrushTABServer('TEMP', TS.list()$TEMP, "°C"),	digits = reactive(input$roundTerm),	rownames = TRUE)
output$"FREQ-TStrendTAB"	<-	renderTable(	TSbrushTABServer('FREQ', TS.list()$FREQ, "MHz"),	digits = reactive(input$roundTerm),	rownames = TRUE)
output$"SOCK-TStrendTAB"	<-	renderTable(	TSbrushTABServer('SOCK', TS.list()$SOCK, "W"),	digits = reactive(input$roundTerm),	rownames = TRUE)
output$"CORE-TStrendTAB"	<-	renderTable(	TSbrushTABServer('CORE', TS.list()$CORE, "W"),	digits = reactive(input$roundTerm),	rownames = TRUE)


#	creating holder for Seasonal brushes
brushSEAS	<-	reactiveValues(	TEMP = NULL,	FREQ = NULL,	SOCK = NULL,	CORE = NULL,	UNCORE = NULL	)

#	observers to update brushSEAS values
observeEvent(input$"TEMP-TSseasBRUSH",	brushSEAS$TEMP	<-	input$"TEMP-TSseasBRUSH"[c("xmin", "xmax")])
observeEvent(input$"FREQ-TSseasBRUSH",	brushSEAS$FREQ	<-	input$"FREQ-TSseasBRUSH"[c("xmin", "xmax")])
observeEvent(input$"SOCK-TSseasBRUSH",	brushSEAS$SOCK	<-	input$"SOCK-TSseasBRUSH"[c("xmin", "xmax")])
observeEvent(input$"CORE-TSseasBRUSH",	brushSEAS$CORE	<-	input$"CORE-TSseasBRUSH"[c("xmin", "xmax")])

#	watches buttons and applies the correct limits to appropriate graphs
observeEvent(input$"TEMP-TSseasBRUSHapp",	{	VAL	<-	brushSEAS$TEMP
	brushSEAS$TEMP	<-	VAL	;	brushSEAS$FREQ	<-	VAL	;	brushSEAS$SOCK	<-	VAL	;	brushSEAS$CORE	<-	VAL
	})
observeEvent(input$"FREQ-TSseasBRUSHapp",	{	VAL	<-	brushSEAS$FREQ
	brushSEAS$TEMP	<-	VAL	;	brushSEAS$FREQ	<-	VAL	;	brushSEAS$SOCK	<-	VAL	;	brushSEAS$CORE	<-	VAL
	})
observeEvent(input$"SOCK-TSseasBRUSHapp",	{	VAL	<-	brushSEAS$SOCK
	brushSEAS$TEMP	<-	VAL	;	brushSEAS$FREQ	<-	VAL	;	brushSEAS$SOCK	<-	VAL	;	brushSEAS$CORE	<-	VAL
	})
observeEvent(input$"CORE-TSseasBRUSHapp",	{	VAL	<-	brushSEAS$CORE
	brushSEAS$TEMP	<-	VAL	;	brushSEAS$FREQ	<-	VAL	;	brushSEAS$SOCK	<-	VAL	;	brushSEAS$CORE	<-	VAL
	})


TSbrushZOOMServer	<-	function(id, TYPE, TS.df, LIMS = NULL)	{moduleServer(id, function(input, output, session)	{
	output$TSseasBRUSHgraph	<-	renderPlot(	
		graphSEAS(TYPE, TS.df) + coord_cartesian(xlim = unlist(	LIMS()[c("xmin", "xmax")]	))
	)
})}

TSbrushZOOMServer('TEMP',	"CPU_Temp",			TS.list()$TEMP,	reactive(brushSEAS$TEMP))
TSbrushZOOMServer('FREQ',	"Frequency",		TS.list()$FREQ,	reactive(brushSEAS$FREQ))
TSbrushZOOMServer('SOCK',	"Socket_Energy",	TS.list()$SOCK,	reactive(brushSEAS$SOCK))
TSbrushZOOMServer('CORE',	"Core_Energy",		TS.list()$CORE,	reactive(brushSEAS$CORE))
