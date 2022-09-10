tableCUSTServer	<-	function(name, IN, TYPE, roundTerm, OFF = 0, UNIT = NULL)	{	moduleServer(name, function(input, output, session)	{
	if (!is.null(UNIT))	updateTextInput(inputId = "ECDF",	label = paste0("Specific Values (", UNIT, ")"))

	PERCs	<-	reactive(	tablePERC(IN, TYPE, to.NUM(input$PERC), UNIT, OFF)	)	%>%	bindCache(IN, input$PERC)
	ECDFs	<-	reactive(	tableECDF(IN, TYPE, to.NUM(input$ECDF), UNIT, OFF)	)	%>%	bindCache(IN, input$ECDF)

	output$PERCtab	<-	renderTable(	PERCs(),	striped = TRUE,	digits = reactive(roundTerm)	)
	output$ECDFtab	<-	renderTable(	ECDFs(),	striped = TRUE,	digits = reactive(roundTerm)	)
})}

observeEvent(list(input$dataSelLOAD, input$roundTerm, input$engPOW),	{
	tableCUSTServer("TEMP",	DATA$dataALL,	"CPU_Temp",			input$roundTerm,	OFF = TABLE$warmOFF())
	tableCUSTServer("FREQ",	DATA$dataALL,	"Frequency",		input$roundTerm)
	tableCUSTServer("SOCK",	DATA$dataALL,	"Socket_Energy",	input$roundTerm,	UNIT = input$engPOW)
	tableCUSTServer("CORE",	DATA$dataALL,	"Core_Energy",		input$roundTerm,	UNIT = input$engPOW)
})