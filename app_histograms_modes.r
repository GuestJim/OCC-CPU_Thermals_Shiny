if (!require(multimode))	install.packages('multimode')
library(multimode)

modesTAB	<-	function(IN, UNIT, NUM, LOW, UPP)	{
	modesLOC	=	locmodes(IN, mod0 = NUM, lowsup = LOW, uppsup = UPP)$location
	modesLOCtab	<-	t(data.frame(Modes = modesLOC[seq(1, length(modesLOC), by = 2)]))

	colnames(modesLOCtab)	<-	paste0("Mode ", 1:ncol(modesLOCtab))
	rownames(modesLOCtab)	<-	paste0("Unit: ", UNIT)

	return(modesLOCtab)
}

modesNUMServer	<-	function(id, TYPE, COEF = 1)	{moduleServer(id, function(input, output, session)	{
	FILT	<-	DATA$dataALL[DATA$dataALL$Period == DATA$TESTname, TYPE] * COEF

	observeEvent(list(input$modeBIN, input$modeLOW, input$modeUPP),	{
		if	(any(is.na(c(input$modeBIN, input$modeLOW, input$modeUPP))))	return(NULL)
		if	(input$modeBIN < 0)								return(NULL)
		if	(!isTruthy(FILT))								return(NULL)
		NUM	<-	nmodes(FILT,	bw = input$modeBIN,	lowsup = input$modeLOW,	uppsup = input$modeUPP)

		if (is.numeric(NUM))	updateNumericInput(inputId = "modeNUM",	value = NUM)
	})
})}

modesLOCServer	<-	function(id, TYPE, UNIT, COEF = 1)	{moduleServer(id, function(input, output, session)	{
	FILT	<-	DATA$dataALL[DATA$dataALL$Period == DATA$TESTname, TYPE] * COEF

	TAB	<-	eventReactive(input$modeUPD,	{
		NULL
		if	(isTruthy(FILT))	modesTAB(FILT, UNIT, input$modeNUM, input$modeLOW, input$modeUPP)
	})
	
	# output$modeTAB	=	renderTable({	as.data.frame(TAB())	},	digits = roundTerm, rownames = TRUE)
	as.data.frame(TAB())
})}

#	calling the functions to calculate number of modes whenever new data is loaded, and clearing the tables
observeEvent(input$dataSelLOAD,	{
	modesNUMServer('TEMP',		"CPU_Temp")
	modesNUMServer('FREQ',		"Frequency")
	modesNUMServer('SOCK',		"Socket_Energy",	1/1000)
	modesNUMServer('CORE',		"Core_Energy",		1/1000)
	modesNUMServer('UNCORE',	"Uncore_Energy",	1/1000)
	
	output$"TEMP-modeTAB"		<-	NULL
	output$"FREQ-modeTAB"		<-	NULL
	output$"SOCK-modeTAB"		<-	NULL
	output$"CORE-modeTAB"		<-	NULL
	output$"UNCORE-modeTAB"		<-	NULL
})

#	Mode tables updated by their respective buttons, and tables react to roundTerm
observeEvent(input$"TEMP-modeUPD",		{	modesLOC	<-	modesLOCServer('TEMP',	"CPU_Temp",	"°C")
	observeEvent(input$roundTerm,	{
		output$"TEMP-modeTAB"	=	renderTable({	modesLOC	},	digits = input$roundTerm, rownames = TRUE)
	})
},	ignoreInit = TRUE)

observeEvent(input$"FREQ-modeUPD",		{	modesLOC	<-	modesLOCServer('FREQ',	"Frequency",	"MHz")
	observeEvent(input$roundTerm,	{
		output$"FREQ-modeTAB"	=	renderTable({	modesLOC	},	digits = input$roundTerm, rownames = TRUE)
	})
},	ignoreInit = TRUE)

observeEvent(input$"SOCK-modeUPD",		{	modesLOC	<-	modesLOCServer('SOCK',	"Socket_Energy",	"W",	1/1000)
	observeEvent(input$roundTerm,	{
		output$"SOCK-modeTAB"	=	renderTable({	modesLOC	},	digits = input$roundTerm, rownames = TRUE)
	})
},	ignoreInit = TRUE)

observeEvent(input$"CORE-modeUPD",		{	modesLOC	<-	modesLOCServer('CORE',	"Core_Energy",	"W",	1/1000)
	observeEvent(input$roundTerm,	{
		output$"CORE-modeTAB"	=	renderTable({	modesLOC	},	digits = input$roundTerm, rownames = TRUE)
	})
},	ignoreInit = TRUE)

observeEvent(input$"UNCORE-modeUPD",	{	modesLOC	<-	modesLOCServer('UNCORE',	"Uncore_Energy",	"W",	1/1000)
	observeEvent(input$roundTerm,	{
		output$"UNCORE-modeTAB"	=	renderTable({	modesLOC	},	digits = input$roundTerm, rownames = TRUE)
	})
},	ignoreInit = TRUE)
