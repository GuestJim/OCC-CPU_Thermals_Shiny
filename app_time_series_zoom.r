# module for the Trend Min/Max tables
TSbrushTABServer	<-	function(name, TS.df,	UNIT = NULL)	{
	roundTerm	<-	reactive(input$roundTerm)	;	dataSelLOAD	<-	reactive(input$dataSelLOAD)
	moduleServer(name, function(input, output, session)	{
		CLEAN	=	as.data.frame(matrix(c("", ""), nrow = 1, ncol = 2, dimnames = list(c(UNIT), c("Minimum", "Maximum"))))
		brushTREND	<-	list(	x = NULL,	FILTER = TRUE,	TAB = CLEAN	)

		observeEvent(dataSelLOAD(),	output$TStrendTAB	<-	renderTable(	CLEAN,	rownames = TRUE	)	)

		observeEvent(input$trendBRUSH,	{
			brushTREND$x	<-	c(input$trendBRUSH$xmin, input$trendBRUSH$xmax)
			brushTREND$FILT	<-	cutWithin(TS.df$Index, brushTREND$x)
			brushTREND$TAB$Minimum	<-	min(TS.df[brushTREND$FILT, "Trend"],	na.rm = TRUE)
			brushTREND$TAB$Maximum	<-	max(TS.df[brushTREND$FILT, "Trend"],	na.rm = TRUE)

			output$TStrendTAB	<-	renderTable(	brushTREND$TAB,	rownames = TRUE,	digits = reactive(roundTerm()))
		})
})}

TSbrushTABServer('TEMP', TS.list()$TEMP, "°C")
TSbrushTABServer('FREQ', TS.list()$FREQ, "MHz")
TSbrushTABServer('SOCK', TS.list()$SOCK, "W")
TSbrushTABServer('CORE', TS.list()$CORE, "W")

#	creating holder for Seasonal brushes
brushSEAS	<-	reactiveValues(	TEMP = NULL,	FREQ = NULL,	SOCK = NULL,	CORE = NULL,	UNCORE = NULL	)

#	observers to update brushSEAS values
observeEvent(input[[NS("TEMP", "TSseasBRUSH")]],	brushSEAS$TEMP	<-	input[[NS("TEMP", "TSseasBRUSH")]][c("xmin", "xmax")])
observeEvent(input[[NS("FREQ", "TSseasBRUSH")]],	brushSEAS$FREQ	<-	input[[NS("FREQ", "TSseasBRUSH")]][c("xmin", "xmax")])
observeEvent(input[[NS("SOCK", "TSseasBRUSH")]],	brushSEAS$SOCK	<-	input[[NS("SOCK", "TSseasBRUSH")]][c("xmin", "xmax")])
observeEvent(input[[NS("CORE", "TSseasBRUSH")]],	brushSEAS$CORE	<-	input[[NS("CORE", "TSseasBRUSH")]][c("xmin", "xmax")])

#	watches buttons and applies the correct limits to appropriate graphs
observeEvent(input[[NS("TEMP", "TSseasBRUSHapp")]],	{	VAL	<-	brushSEAS$TEMP
	brushSEAS$TEMP	<-	VAL	;	brushSEAS$FREQ	<-	VAL	;	brushSEAS$SOCK	<-	VAL	;	brushSEAS$CORE	<-	VAL
	})
observeEvent(input[[NS("FREQ", "TSseasBRUSHapp")]],	{	VAL	<-	brushSEAS$FREQ
	brushSEAS$TEMP	<-	VAL	;	brushSEAS$FREQ	<-	VAL	;	brushSEAS$SOCK	<-	VAL	;	brushSEAS$CORE	<-	VAL
	})
observeEvent(input[[NS("SOCK", "TSseasBRUSHapp")]],	{	VAL	<-	brushSEAS$SOCK
	brushSEAS$TEMP	<-	VAL	;	brushSEAS$FREQ	<-	VAL	;	brushSEAS$SOCK	<-	VAL	;	brushSEAS$CORE	<-	VAL
	})
observeEvent(input[[NS("CORE", "TSseasBRUSHapp")]],	{	VAL	<-	brushSEAS$CORE
	brushSEAS$TEMP	<-	VAL	;	brushSEAS$FREQ	<-	VAL	;	brushSEAS$SOCK	<-	VAL	;	brushSEAS$CORE	<-	VAL
	})


TSbrushZOOMServer	<-	function(name, TYPE, TS.df, LIMS = NULL)	{moduleServer(name, function(input, output, session)	{
	output$TSseasBRUSHgraph	<-	renderPlot(
		graphSEAS(TYPE, TS.df) + coord_cartesian(xlim = unlist(	LIMS()[c("xmin", "xmax")]	))
	)
})}

TSbrushZOOMServer('TEMP',	"CPU_Temp",			TS.list()$TEMP,	reactive(brushSEAS$TEMP))
TSbrushZOOMServer('FREQ',	"Frequency",		TS.list()$FREQ,	reactive(brushSEAS$FREQ))
TSbrushZOOMServer('SOCK',	"Socket_Energy",	TS.list()$SOCK,	reactive(brushSEAS$SOCK))
TSbrushZOOMServer('CORE',	"Core_Energy",		TS.list()$CORE,	reactive(brushSEAS$CORE))
