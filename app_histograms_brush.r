#	CPU_Temp
brushHISTtemp	<-		reactiveValues(x = c(-Inf, Inf),	FILTER	=	FALSE)

observeEvent(input$brushHISTtemp,	{
	brushHISTtemp$x		<-	c(input$brushHISTtemp$xmin, input$brushHISTtemp$xmax)
	updateNumericInput(inputId = "brushHISTtempMIN",	value = round(brushHISTtemp$x[1]))
	updateNumericInput(inputId = "brushHISTtempMAX",	value = round(brushHISTtemp$x[2]))
})
observeEvent(list(input$brushHISTtempMIN, input$brushHISTtempMAX),	{
	brushHISTtemp$x		<-	c(input$brushHISTtempMIN, input$brushHISTtempMAX)
	if (any(is.null(c(input$brushHISTtempMIN, input$brushHISTtempMAX))))	brushHISTtemp$x	<-	rep(brushHISTtemp$x, 2)
},	ignoreInit = TRUE)

GRAPH$graphHISTtempTAB	<-	reactive({	
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

observeEvent(input$brushHISTfreq,	{
	brushHISTfreq$x		<-	c(input$brushHISTfreq$xmin, input$brushHISTfreq$xmax)
	updateNumericInput(inputId = "brushHISTfreqMIN",	value = round(brushHISTfreq$x[1]))
	updateNumericInput(inputId = "brushHISTfreqMAX",	value = round(brushHISTfreq$x[2]))
})
observeEvent(list(input$brushHISTfreqMIN, input$brushHISTfreqMAX),	{
	brushHISTfreq$x		<-	c(input$brushHISTfreqMIN, input$brushHISTfreqMAX)
	if (any(is.null(c(input$brushHISTfreqMIN, input$brushHISTfreqMAX))))	brushHISTfreq$x	<-	rep(brushHISTfreq$x, 2)
},	ignoreInit = TRUE)

observeEvent(input$dataSelLOAD, {
	if (exists("FREQspec", envir = DATA))	{
		updateNumericInput(inputId = "brushHISTfreqMIN",	value = DATA$FREQspec[1])
		updateNumericInput(inputId = "brushHISTfreqMAX",	value = DATA$FREQspec[2])
	}
})

GRAPH$graphHISTfreqTAB	<-	reactive({	
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

observeEvent(input$brushHISTsock,	{
	brushHISTsock$x		<-	c(input$brushHISTsock$xmin, input$brushHISTsock$xmax)
	updateNumericInput(inputId = "brushHISTsockMIN",	value = round(brushHISTsock$x[1]))
	updateNumericInput(inputId = "brushHISTsockMAX",	value = round(brushHISTsock$x[2]))
})
observeEvent(list(input$brushHISTsockMIN, input$brushHISTsockMAX),	{
	brushHISTsock$x		<-	c(input$brushHISTsockMIN, input$brushHISTsockMAX)
	if (any(is.null(c(input$brushHISTsockMIN, input$brushHISTsockMAX))))	brushHISTsock$x	<-	rep(brushHISTsock$x, 2)
},	ignoreInit = TRUE)

GRAPH$graphHISTsockTAB	<-	reactive({	
	hold	<-	tableECDF(DATA$dataALL, "Socket_Energy", round(brushHISTsock$x))
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

observeEvent(input$brushHISTcore,	{
	brushHISTcore$x		<-	c(input$brushHISTcore$xmin, input$brushHISTcore$xmax)
	updateNumericInput(inputId = "brushHISTcoreMIN",	value = round(brushHISTcore$x[1], 1))
	updateNumericInput(inputId = "brushHISTcoreMAX",	value = round(brushHISTcore$x[2], 1))
})
observeEvent(list(input$brushHISTcoreMIN, input$brushHISTcoreMAX),	{
	brushHISTcore$x		<-	c(input$brushHISTcoreMIN, input$brushHISTcoreMAX)
	if (any(is.null(c(input$brushHISTcoreMIN, input$brushHISTcoreMAX))))	brushHISTcore$x	<-	rep(brushHISTcore$x, 2)
},	ignoreInit = TRUE)

GRAPH$graphHISTcoreTAB	<-	reactive({	
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

observeEvent(input$brushHISTuncore,	{
	brushHISTuncore$x		<-	c(input$brushHISTuncore$xmin, input$brushHISTuncore$xmax)
	updateNumericInput(inputId = "brushHISTuncoreMIN",	value = round(brushHISTuncore$x[1], 1))
	updateNumericInput(inputId = "brushHISTuncoreMAX",	value = round(brushHISTuncore$x[2], 1))
})
observeEvent(list(input$brushHISTuncoreMIN, input$brushHISTuncoreMAX),	{
	brushHISTuncore$x		<-	c(input$brushHISTuncoreMIN, input$brushHISTuncoreMAX)
	if (any(is.null(c(input$brushHISTuncoreMIN, input$brushHISTuncoreMAX))))	brushHISTuncore$x	<-	rep(brushHISTuncore$x, 2)
},	ignoreInit = TRUE)

GRAPH$graphHISTuncoreTAB	<-	reactive({	
	hold	<-	tableECDF(DATA$dataALL, "Uncore_Energy", round(brushHISTuncore$x, 1))
	hold$Difference	=	hold[, 3] - hold[, 2]	;	hold$Unit	=	"%"
	
	numCOL	=	sapply(hold, is.numeric)
	hold[, c(which(!numCOL), which(numCOL))]
	})

observeEvent(input$roundTerm, {
	output$graphHISTuncoreTAB	<-	renderTable({	GRAPH$graphHISTuncoreTAB()	},
		digits	=	input$roundTerm,	striped	=	TRUE)
})
