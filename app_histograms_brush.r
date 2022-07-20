brushHIST	<-	reactiveValues(
	TEMP	=	NULL,	FREQ	=	NULL,	SOCK	=	NULL,	CORE	=	NULL,	UNCORE	=	NULL,
	TAB		=	data.frame(cbind(
		Type			=	c("Warm-up", "Test", "Cooldown"),
		Unit			=	"%",
		"Lower Limit"	=	"",
		"Upper Limit"	=	"",
		Difference		=	""
		),	check.names = FALSE	)
)

observeEvent(input$dataSelLOAD,	{
	brushHIST$TAB$Type	<-	DATA$levsPER
	brushHIST$TEMP			<-	NULL	;	output$graphHISTtempTAB		<-	renderTable(brushHIST$TAB, striped = TRUE)
	brushHIST$FREQ			<-	NULL	;	output$graphHISTfreqTAB		<-	renderTable(brushHIST$TAB, striped = TRUE)
		if (exists("FREQspec", envir = DATA))	brushHIST$FREQ	<-	DATA$FREQspec
	brushHIST$SOCK			<-	NULL	;	output$graphHISTsockTAB		<-	renderTable(brushHIST$TAB, striped = TRUE)
	brushHIST$CORE			<-	NULL	;	output$graphHISTcoreTAB		<-	renderTable(brushHIST$TAB, striped = TRUE)
	brushHIST$UNCORE		<-	NULL	;	output$graphHISTuncoreTAB	<-	renderTable(brushHIST$TAB, striped = TRUE)
})

brushHISTtabServer	<-	function(id, BRUSH)	{moduleServer(id, function(input, output, session)	{
	if (!isTruthy(BRUSH))	return(brushHIST$TAB)
	if (diff(BRUSH) == 0 | any(is.infinite(BRUSH)))	return(brushHIST$TAB)

	TAB				<-	tableECDF(DATA$dataALL, as.character(id), round(BRUSH))
	TAB$Difference	<-	TAB[, 3] - TAB[, 2]	;	TAB$Unit	=	"%"

	numCOL	=	sapply(TAB, is.numeric)

	return(TAB[, c(which(!numCOL), which(numCOL))])
})}

#	CPU_Temp
observeEvent(input$graphHISTtempBIN,	{
	if (input$graphHISTtempBIN == 0)	updateNumericInput(inputId = "graphHISTtempBIN",	value = 1)
})
observeEvent(input$brushHISTtemp,	{
	brushHIST$TEMP		<-	c(input$brushHISTtemp$xmin, input$brushHISTtemp$xmax)
},	priority = 10)
observeEvent(list(input$brushHISTtempMIN, input$brushHISTtempMAX),	{
	brushHIST$TEMP[1]		<-	min(c(input$brushHISTtempMIN, input$brushHISTtempMAX),	na.rm = TRUE)
	brushHIST$TEMP[2]		<-	max(c(input$brushHISTtempMIN, input$brushHISTtempMAX),	na.rm = TRUE)
},	ignoreInit = TRUE)
observeEvent(brushHIST$TEMP,	{
	if (brushHIST$TEMP[1] <= 0)	brushHIST$TEMP[1]	<-	input$graphHISTtempBIN
	if (any(is.na(brushHIST$TEMP)))	brushHIST$TEMP	<-	rep(remNA(brushHIST$TEMP), 2)

	if (!is.na(brushHIST$TEMP[1]))	updateNumericInput(inputId = "brushHISTtempMIN",	value = round(brushHIST$TEMP[1]))
	if (!is.na(brushHIST$TEMP[2]))	updateNumericInput(inputId = "brushHISTtempMAX",	value = round(brushHIST$TEMP[2]))
})

observeEvent(list(input$roundTerm, brushHIST$TEMP), {
	output$graphHISTtempTAB	<-	renderTable({	brushHISTtabServer("CPU_Temp", brushHIST$TEMP)	},
		digits	=	input$roundTerm,	striped	=	TRUE)
})

#	Frequency
observeEvent(input$graphHISTfreqBIN,	{
	if (input$graphHISTfreqBIN == 0)	updateNumericInput(inputId = "graphHISTfreqBIN",	value = 1)
})
observeEvent(input$brushHISTfreq,	{
	brushHIST$FREQ		<-	c(input$brushHISTfreq$xmin, input$brushHISTfreq$xmax)
},	priority = 10)
observeEvent(list(input$brushHISTfreqMIN, input$brushHISTfreqMAX),	{
	brushHIST$FREQ[1]		<-	min(c(input$brushHISTfreqMIN, input$brushHISTfreqMAX),	na.rm = TRUE)
	brushHIST$FREQ[2]		<-	max(c(input$brushHISTfreqMIN, input$brushHISTfreqMAX),	na.rm = TRUE)
},	ignoreInit = TRUE)
observeEvent(brushHIST$FREQ,	{
	if (brushHIST$FREQ[1] <= 0)	brushHIST$FREQ[1]	<-	input$graphHISTfreqBIN
	if (any(is.na(brushHIST$FREQ)))	brushHIST$FREQ	<-	rep(remNA(brushHIST$FREQ), 2)

	if (!is.na(brushHIST$FREQ[1]))	updateNumericInput(inputId = "brushHISTfreqMIN",	value = round(brushHIST$FREQ[1]))
	if (!is.na(brushHIST$FREQ[2]))	updateNumericInput(inputId = "brushHISTfreqMAX",	value = round(brushHIST$FREQ[2]))
})

observeEvent(list(input$roundTerm, brushHIST$FREQ), {
	output$graphHISTfreqTAB	<-	renderTable({	brushHISTtabServer("Frequency", brushHIST$FREQ)	},
		digits	=	input$roundTerm,	striped	=	TRUE)
})

#	Socket_Energy
observeEvent(input$graphHISTsockBIN,	{
	if (input$graphHISTsockBIN == 0)	updateNumericInput(inputId = "graphHISTsockBIN",	value = 0.1)
})
observeEvent(input$brushHISTsock,	{
	brushHIST$SOCK		<-	c(input$brushHISTsock$xmin, input$brushHISTsock$xmax)
},	priority = 10)
observeEvent(list(input$brushHISTsockMIN, input$brushHISTsockMAX),	{
	brushHIST$SOCK[1]		<-	min(c(input$brushHISTsockMIN, input$brushHISTsockMAX),	na.rm = TRUE)
	brushHIST$SOCK[2]		<-	max(c(input$brushHISTsockMIN, input$brushHISTsockMAX),	na.rm = TRUE)
},	ignoreInit = TRUE)
observeEvent(brushHIST$SOCK,	{
	if (brushHIST$SOCK[1] <= 0)	brushHIST$SOCK[1]	<-	input$graphHISTsockBIN
	if (any(is.na(brushHIST$SOCK)))	brushHIST$SOCK	<-	rep(remNA(brushHIST$SOCK), 2)

	if (!is.na(brushHIST$SOCK[1]))	updateNumericInput(inputId = "brushHISTsockMIN",	value = round(brushHIST$SOCK[1], 1))
	if (!is.na(brushHIST$SOCK[2]))	updateNumericInput(inputId = "brushHISTsockMAX",	value = round(brushHIST$SOCK[2], 1))
})

observeEvent(list(input$roundTerm, brushHIST$SOCK), {
	output$graphHISTsockTAB	<-	renderTable({	brushHISTtabServer("Socket_Energy", brushHIST$SOCK)	},
		digits	=	input$roundTerm,	striped	=	TRUE)
})

#	Core_Energy
observeEvent(input$graphHISTcoreBIN,	{
	if (input$graphHISTcoreBIN == 0)	updateNumericInput(inputId = "graphHISTcoreBIN",	value = 0.1)
})
observeEvent(input$brushHISTcore,	{
	brushHIST$CORE		<-	c(input$brushHISTcore$xmin, input$brushHISTcore$xmax)
},	priority = 10)
observeEvent(list(input$brushHISTcoreMIN, input$brushHISTcoreMAX),	{
	brushHIST$CORE[1]		<-	min(c(input$brushHISTcoreMIN, input$brushHISTcoreMAX),	na.rm = TRUE)
	brushHIST$CORE[2]		<-	max(c(input$brushHISTcoreMIN, input$brushHISTcoreMAX),	na.rm = TRUE)
},	ignoreInit = TRUE)
observeEvent(brushHIST$CORE,	{
	if (brushHIST$CORE[1] <= 0)	brushHIST$CORE[1]	<-	input$graphHISTcoreBIN
	if (any(is.na(brushHIST$CORE)))	brushHIST$CORE	<-	rep(remNA(brushHIST$CORE), 2)

	if (!is.na(brushHIST$CORE[1]))	updateNumericInput(inputId = "brushHISTcoreMIN",	value = round(brushHIST$CORE[1], 1))
	if (!is.na(brushHIST$CORE[2]))	updateNumericInput(inputId = "brushHISTcoreMAX",	value = round(brushHIST$CORE[2], 1))
})

observeEvent(list(input$roundTerm, brushHIST$CORE), {
	output$graphHISTcoreTAB	<-	renderTable({	brushHISTtabServer("Core_Energy", brushHIST$CORE)	},
		digits	=	input$roundTerm,	striped	=	TRUE)
})

#	Uncore_Energy
observeEvent(input$graphHISTuncoreBIN,	{
	if (input$graphHISTuncoreBIN == 0)	updateNumericInput(inputId = "graphHISTuncoreBIN",	value = 0.1)
})
observeEvent(input$brushHISTuncore,	{
	brushHIST$UNCORE		<-	c(input$brushHISTuncore$xmin, input$brushHISTuncore$xmax)
},	priority = 10)
observeEvent(list(input$brushHISTuncoreMIN, input$brushHISTuncoreMAX),	{
	brushHIST$UNCORE[1]		<-	min(c(input$brushHISTuncoreMIN, input$brushHISTuncoreMAX),	na.rm = TRUE)
	brushHIST$UNCORE[2]		<-	max(c(input$brushHISTuncoreMIN, input$brushHISTuncoreMAX),	na.rm = TRUE)
},	ignoreInit = TRUE)
observeEvent(brushHIST$UNCORE,	{
	if (brushHIST$UNCORE[1] <= 0)	brushHIST$UNCORE[1]	<-	input$graphHISTuncoreBIN
	if (any(is.na(brushHIST$UNCORE)))	brushHIST$UNCORE	<-	rep(remNA(brushHIST$UNCORE), 2)

	if (!is.na(brushHIST$UNCORE[1]))	updateNumericInput(inputId = "brushHISTuncoreMIN",	value = round(brushHIST$UNCORE[1], 1))
	if (!is.na(brushHIST$UNCORE[2]))	updateNumericInput(inputId = "brushHISTuncoreMAX",	value = round(brushHIST$UNCORE[2], 1))
})

observeEvent(list(input$roundTerm, brushHIST$UNCORE), {
	output$graphHISTuncoreTAB	<-	renderTable({	brushHISTtabServer("Uncore_Energy", brushHIST$UNCORE)	},
		digits	=	input$roundTerm,	striped	=	TRUE)
})
