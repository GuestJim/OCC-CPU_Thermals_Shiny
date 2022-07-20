#	CPU_Temp
brushHISTtemp	<-		reactiveValues(x = c(-Inf, Inf),	FILTER	=	FALSE)

observeEvent(input$graphHISTtempBIN,	{
	if (input$graphHISTtempBIN == 0)	updateNumericInput(inputId = "graphHISTtempBIN",	value = 1)
})
observeEvent(input$brushHISTtemp,	{
	brushHISTtemp$x		<-	c(input$brushHISTtemp$xmin, input$brushHISTtemp$xmax)
	if (brushHISTtemp$x[1] <= 0)	brushHISTtemp$x[1]	<-	input$graphHISTtempBIN
},	priority = 10)
observeEvent(list(input$brushHISTtempMIN, input$brushHISTtempMAX),	{
	brushHISTtemp$x[1]		<-	min(c(input$brushHISTtempMIN, input$brushHISTtempMAX),	na.rm = TRUE)
	brushHISTtemp$x[2]		<-	max(c(input$brushHISTtempMIN, input$brushHISTtempMAX),	na.rm = TRUE)
	if (brushHISTtemp$x[1] <= 0)	brushHISTtemp$x[1]	<-	input$graphHISTtempBIN
	if (any(is.na(brushHISTtemp$x)))	brushHISTtemp$x	<-	rep(remNA(brushHISTtemp$x), 2)
},	ignoreInit = TRUE)
observeEvent(brushHISTtemp$x,	{
	if (!is.na(brushHISTtemp$x[1]))	updateNumericInput(inputId = "brushHISTtempMIN",	value = round(brushHISTtemp$x[1]))
	if (!is.na(brushHISTtemp$x[2]))	updateNumericInput(inputId = "brushHISTtempMAX",	value = round(brushHISTtemp$x[2]))
})

GRAPH$graphHISTtempTAB	<-	reactive({
	if (diff(brushHISTtemp$x) == 0 | any(is.infinite(brushHISTtemp$x)))	return(NULL)
	hold	<-	tableECDF(DATA$dataALL, "CPU_Temp", round(brushHISTtemp$x))
	hold$Difference	=	hold[, 3] - hold[, 2]	;	hold$Unit	=	"%"

	numCOL	=	sapply(hold, is.numeric)
	hold[, c(which(!numCOL), which(numCOL))]
	})

observeEvent(input$roundTerm, {
	output$graphHISTtempTAB	<-	renderTable({	GRAPH$graphHISTtempTAB()	},
		digits	=	input$roundTerm,	striped	=	TRUE)
})

#	Frequency
brushHISTfreq	<-		reactiveValues(x = c(-Inf, Inf),	FILTER	=	FALSE)
observeEvent(input$dataSelLOAD, {
	if (exists("FREQspec", envir = DATA))	brushHISTfreq$x	<-	DATA$FREQspec
})

observeEvent(input$graphHISTfreqBIN,	{
	if (input$graphHISTfreqBIN == 0)	updateNumericInput(inputId = "graphHISTfreqBIN",	value = 1)
})
observeEvent(input$brushHISTfreq,	{
	brushHISTfreq$x		<-	c(input$brushHISTfreq$xmin, input$brushHISTfreq$xmax)
	if (brushHISTfreq$x[1] <= 0)	brushHISTfreq$x[1]	<-	input$graphHISTfreqBIN
},	priority = 10)
observeEvent(list(input$brushHISTfreqMIN, input$brushHISTfreqMAX),	{
	brushHISTfreq$x[1]		<-	min(c(input$brushHISTfreqMIN, input$brushHISTfreqMAX),	na.rm = TRUE)
	brushHISTfreq$x[2]		<-	max(c(input$brushHISTfreqMIN, input$brushHISTfreqMAX),	na.rm = TRUE)
	if (brushHISTfreq$x[1] <= 0)	brushHISTfreq$x[1]	<-	input$graphHISTfreqBIN
	if (any(is.na(brushHISTfreq$x)))	brushHISTfreq$x	<-	rep(remNA(brushHISTfreq$x), 2)
},	ignoreInit = TRUE)
observeEvent(brushHISTfreq$x,	{
	if (!is.na(brushHISTfreq$x[1]))	updateNumericInput(inputId = "brushHISTfreqMIN",	value = round(brushHISTfreq$x[1]))
	if (!is.na(brushHISTfreq$x[2]))	updateNumericInput(inputId = "brushHISTfreqMAX",	value = round(brushHISTfreq$x[2]))
})

GRAPH$graphHISTfreqTAB	<-	reactive({
	if (diff(brushHISTfreq$x) == 0 | any(is.infinite(brushHISTfreq$x)))	return(NULL)
	hold	<-	tableECDF(DATA$dataALL, "Frequency", round(brushHISTfreq$x))
	hold$Difference	=	hold[, 3] - hold[, 2]	;	hold$Unit	=	"%"

	numCOL	=	sapply(hold, is.numeric)
	hold[, c(which(!numCOL), which(numCOL))]
	})

observeEvent(input$roundTerm, {
	output$graphHISTfreqTAB	<-	renderTable({	GRAPH$graphHISTfreqTAB()	},
		digits	=	input$roundTerm,	striped	=	TRUE)
})

#	Socket_Energy
brushHISTsock	<-		reactiveValues(x = c(-Inf, Inf),	FILTER	=	FALSE)

observeEvent(input$graphHISTsockBIN,	{
	if (input$graphHISTsockBIN == 0)	updateNumericInput(inputId = "graphHISTsockBIN",	value = 0.1)
})
observeEvent(input$brushHISTsock,	{
	brushHISTsock$x		<-	c(input$brushHISTsock$xmin, input$brushHISTsock$xmax)
	if (brushHISTsock$x[1] <= 0)	brushHISTsock$x[1]	<-	input$graphHISTsockBIN
},	priority = 10)
observeEvent(list(input$brushHISTsockMIN, input$brushHISTsockMAX),	{
	brushHISTsock$x[1]		<-	min(c(input$brushHISTsockMIN, input$brushHISTsockMAX),	na.rm = TRUE)
	brushHISTsock$x[2]		<-	max(c(input$brushHISTsockMIN, input$brushHISTsockMAX),	na.rm = TRUE)
},	ignoreInit = TRUE)
observeEvent(brushHISTsock$x,	{
	if (!is.na(brushHISTsock$x[1]))	updateNumericInput(inputId = "brushHISTsockMIN",	value = round(brushHISTsock$x[1], 1))
	if (!is.na(brushHISTsock$x[2]))	updateNumericInput(inputId = "brushHISTsockMAX",	value = round(brushHISTsock$x[2], 1))
})

GRAPH$graphHISTsockTAB	<-	reactive({
	if (diff(brushHISTsock$x) == 0 | any(is.infinite(brushHISTsock$x)))	return(NULL)
	hold	<-	tableECDF(DATA$dataALL, "Socket_Energy", round(brushHISTsock$x, 1))
	hold$Difference	=	hold[, 3] - hold[, 2]	;	hold$Unit	=	"%"

	numCOL	=	sapply(hold, is.numeric)
	hold[, c(which(!numCOL), which(numCOL))]
	})

observeEvent(input$roundTerm, {
	output$graphHISTsockTAB	<-	renderTable({	GRAPH$graphHISTsockTAB()	},
		digits	=	input$roundTerm,	striped	=	TRUE)
})

#	Core_Energy
brushHISTcore	<-		reactiveValues(x = c(-Inf, Inf),	FILTER	=	FALSE)

observeEvent(input$graphHISTcoreBIN,	{
	if (input$graphHISTcoreBIN == 0)	updateNumericInput(inputId = "graphHISTcoreBIN",	value = 0.1)
})
observeEvent(input$brushHISTcore,	{
	brushHISTcore$x		<-	c(input$brushHISTcore$xmin, input$brushHISTcore$xmax)
	if (brushHISTcore$x[1] <= 0)	brushHISTcore$x[1]	<-	input$graphHISTcoreBIN
},	priority = 10)
observeEvent(list(input$brushHISTcoreMIN, input$brushHISTcoreMAX),	{
	brushHISTcore$x[1]		<-	min(c(input$brushHISTcoreMIN, input$brushHISTcoreMAX),	na.rm = TRUE)
	brushHISTcore$x[2]		<-	max(c(input$brushHISTcoreMIN, input$brushHISTcoreMAX),	na.rm = TRUE)
	if (brushHISTcore$x[1] <= 0)	brushHISTcore$x[1]	<-	input$graphHISTcoreBIN
	if (any(is.na(brushHISTcore$x)))	brushHISTcore$x	<-	rep(remNA(brushHISTcore$x), 2)
},	ignoreInit = TRUE)
observeEvent(brushHISTcore$x,	{
	if (!is.na(brushHISTcore$x[1]))	updateNumericInput(inputId = "brushHISTcoreMIN",	value = round(brushHISTcore$x[1], 1))
	if (!is.na(brushHISTcore$x[2]))	updateNumericInput(inputId = "brushHISTcoreMAX",	value = round(brushHISTcore$x[2], 1))
})

GRAPH$graphHISTcoreTAB	<-	reactive({
	if (diff(brushHISTcore$x) == 0 | any(is.infinite(brushHISTcore$x)))	return(NULL)
	hold	<-	tableECDF(DATA$dataALL, "Core_Energy", round(brushHISTcore$x, 1))
	hold$Difference	=	hold[, 3] - hold[, 2]	;	hold$Unit	=	"%"

	numCOL	=	sapply(hold, is.numeric)
	hold[, c(which(!numCOL), which(numCOL))]
	})

observeEvent(input$roundTerm, {
	output$graphHISTcoreTAB	<-	renderTable({	GRAPH$graphHISTcoreTAB()	},
		digits	=	input$roundTerm,	striped	=	TRUE)
})

#	Uncore_Energy
brushHISTuncore	<-		reactiveValues(x = c(-Inf, Inf),	FILTER	=	FALSE)

observeEvent(input$graphHISTuncoreBIN,	{
	if (input$graphHISTuncoreBIN == 0)	updateNumericInput(inputId = "graphHISTuncoreBIN",	value = 0.1)
})
observeEvent(input$brushHISTuncore,	{
	brushHISTuncore$x		<-	c(input$brushHISTuncore$xmin, input$brushHISTuncore$xmax)
	if (brushHISTuncore$x[1] <= 0)	brushHISTuncore$x[1]	<-	input$graphHISTuncoreBIN
},	priority = 10)
observeEvent(list(input$brushHISTuncoreMIN, input$brushHISTuncoreMAX),	{
	brushHISTuncore$x[1]		<-	min(c(input$brushHISTuncoreMIN, input$brushHISTuncoreMAX),	na.rm = TRUE)
	brushHISTuncore$x[2]		<-	max(c(input$brushHISTuncoreMIN, input$brushHISTuncoreMAX),	na.rm = TRUE)
	if (brushHISTuncore$x[1] <= 0)	brushHISTuncore$x[1]	<-	input$graphHISTuncoreBIN
	if (any(is.na(brushHISTuncore$x)))	brushHISTuncore$x	<-	rep(remNA(brushHISTuncore$x), 2)
},	ignoreInit = TRUE)
observeEvent(brushHISTuncore$x,	{
	if (!is.na(brushHISTuncore$x[1]))	updateNumericInput(inputId = "brushHISTuncoreMIN",	value = round(brushHISTuncore$x[1], 1))
	if (!is.na(brushHISTuncore$x[2]))	updateNumericInput(inputId = "brushHISTuncoreMAX",	value = round(brushHISTuncore$x[2], 1))
})

GRAPH$graphHISTuncoreTAB	<-	reactive({
	if (diff(brushHISTuncore$x) == 0 | any(is.infinite(brushHISTuncore$x)))	return(NULL)
	hold	<-	tableECDF(DATA$dataALL, "Uncore_Energy", round(brushHISTuncore$x, 1))
	hold$Difference	=	hold[, 3] - hold[, 2]	;	hold$Unit	=	"%"

	numCOL	=	sapply(hold, is.numeric)
	hold[, c(which(!numCOL), which(numCOL))]
	})

observeEvent(input$roundTerm, {
	output$graphHISTuncoreTAB	<-	renderTable({	GRAPH$graphHISTuncoreTAB()	},
		digits	=	input$roundTerm,	striped	=	TRUE)
})
