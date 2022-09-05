brushHISTclearServer	<-	function(id)	{moduleServer(id, function(input, output, session)	{
	updateNumericInput(inputId = "brushMIN",	value = NA)	;	updateNumericInput(inputId = "brushMAX",	value = NA)
	output$brushHISTtab	<-	renderTable({	TAB	},	striped = TRUE)
})}

brushHISTtabServer	<-	function(id, TYPE, r = 0, roundTerm)	{moduleServer(id, function(input, output, session)	{
	BRUSH	<-	reactiveValues(xMIN = NULL,	xMAX = NULL)
	TAB		=	data.frame(cbind(
		Type			=	c("Warm-up", "Test", "Cooldown"),
		Unit			=	"%",
		"Lower Limit"	=	"",
		"Upper Limit"	=	"",
		Difference		=	""
		),	check.names = FALSE	)
	if	(isTruthy(DATA$levsPER))	TAB$Type	=	DATA$levsPER

	observeEvent(input$brushHIST,	{
		BRUSH$xMIN	<-	input$brushHIST$xmin	;	BRUSH$xMAX	<-	input$brushHIST$xmax
	},	priority = 10)
	observeEvent(list(input$brushMIN, input$brushMAX),	{
		BRUSH$xMIN	<-	min(c(input$brushMIN, input$brushMAX),	na.rm = TRUE)
		BRUSH$xMAX	<-	max(c(input$brushMIN, input$brushMAX),	na.rm = TRUE)
	},	ignoreInit	=	TRUE)

	observeEvent(list(BRUSH$xMIN, BRUSH$xMAX),	{
		if (BRUSH$xMIN <= 0)	BRUSH$xMIN	<-	input$BIN
		if (any(is.na(BRUSH)))	BRUSH	<-	rep(remNA(BRUSH), 2)

		if (!is.na(	BRUSH$xMIN	))	updateNumericInput(inputId = "brushMIN",	value = round(	BRUSH$xMIN	, r))
		if (!is.na(	BRUSH$xMAX	))	updateNumericInput(inputId = "brushMAX",	value = round(	BRUSH$xMAX	, r))
	},	ignoreInit	=	TRUE)

	output$brushHISTtab	<-	renderTable({
		if (!isTruthy(c(BRUSH$xMIN, BRUSH$xMAX)))	return(TAB)
		if (diff(c(BRUSH$xMIN, BRUSH$xMAX)) == 0 | any(is.infinite(c(BRUSH$xMIN, BRUSH$xMAX))))	return(TAB)

		out				<-	tableECDF(DATA$dataALL, TYPE, round(	c(BRUSH$xMIN, BRUSH$xMAX)	))
		out$Difference	<-	out[, 3] - out[, 2]	;	out$Unit	=	"%"

		numCOL	=	sapply(out, is.numeric)

		out[, c(which(!numCOL), which(numCOL))]
	},	digits = roundTerm,	striped = TRUE)
})}


observeEvent(list(input$dataSelLOAD, input$roundTerm),	{
	brushHISTtabServer('TEMP',		"CPU_Temp",			0,	input$roundTerm)
	brushHISTtabServer('FREQ',		"Frequency",		0,	input$roundTerm)
	brushHISTtabServer('SOCK',		"Socket_Energy",	1,	input$roundTerm)
	brushHISTtabServer('CORE',		"Core_Energy",		1,	input$roundTerm)
	brushHISTtabServer('UNCORE',	"Uncore_Energy",	1,	input$roundTerm)
},	ignoreInit = FALSE)

observeEvent(input$dataSelLOAD,	{
	brushHISTclearServer('TEMP')
	brushHISTclearServer('FREQ')
	brushHISTclearServer('SOCK')
	brushHISTclearServer('CORE')
	brushHISTclearServer('UNCORE')
},	priority = 10,	ignoreInit = FALSE)