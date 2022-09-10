tableCUSTServer	<-	function(name, TYPE, OFF = 0, UNIT = NULL)	{
	IN	<-	DATA$dataALL	;	roundTerm	<- reactive(input$roundTerm)	;	moduleServer(name, function(input, output, session)	{
	if (!is.null(UNIT))	updateTextInput(inputId = "ECDF",	label = paste0("Specific Values (", UNIT, ")"))

	PERCs	<-	reactive(	tablePERC(IN, TYPE, to.NUM(input$PERC), UNIT, OFF)	)	%>%	bindCache(IN, input$PERC)
	ECDFs	<-	reactive(	tableECDF(IN, TYPE, to.NUM(input$ECDF), UNIT, OFF)	)	%>%	bindCache(IN, input$ECDF)

	output$PERCtab	<-	renderTable(	PERCs(),	striped = TRUE,	digits = reactive(roundTerm())	)
	output$ECDFtab	<-	renderTable(	ECDFs(),	striped = TRUE,	digits = reactive(roundTerm())	)
})}

observeEvent(list(input$dataSelLOAD, input$engPOW),	{
	tableCUSTServer("TEMP",	"CPU_Temp",			OFF = TABLE$warmOFF())
	tableCUSTServer("FREQ",	"Frequency")
	tableCUSTServer("SOCK",	"Socket_Energy",	UNIT = input$engPOW)
	tableCUSTServer("CORE",	"Core_Energy",		UNIT = input$engPOW)
})