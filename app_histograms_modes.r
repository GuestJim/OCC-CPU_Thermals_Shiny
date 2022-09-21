if (!require(multimode))	install.packages('multimode')
library(multimode)

modesTAB	<-	function(IN, UNIT, NUM, LOW, UPP)	{
	modesLOC	=	locmodes(IN, mod0 = NUM, lowsup = LOW, uppsup = UPP)$location
	modesLOCtab	<-	t(data.frame(Modes = modesLOC[seq(1, length(modesLOC), by = 2)]))

	colnames(modesLOCtab)	<-	paste0("Mode ", 1:ncol(modesLOCtab))
	rownames(modesLOCtab)	<-	paste0("Unit: ", UNIT)

	return(modesLOCtab)
}

modesNUMServer	<-	function(name, TYPE, COEF = 1)	{moduleServer(name, function(input, output, session)	{
	FILT	<-	DATA$dataALL[DATA$dataALL$Period == DATA$TESTname, TYPE] * COEF

	observeEvent(list(input$modeBIN, input$modeLOW, input$modeUPP),	{
		if	(any(is.na(c(input$modeBIN, input$modeLOW, input$modeUPP))))	return(NULL)
		if	(input$modeBIN < 0)								return(NULL)
		if	(!isTruthy(FILT))								return(NULL)
		NUM	<-	nmodes(FILT,	bw = input$modeBIN,	lowsup = input$modeLOW,	uppsup = input$modeUPP)

		if (is.numeric(NUM))	updateNumericInput(inputId = "modeNUM",	value = NUM)
	})
})}

modesLOCServer	<-	function(name, TYPE, UNIT, COEF = 1)	{roundTerm <- reactive(input$roundTerm)
	moduleServer(name, function(input, output, session)	{
		FILT	<-	DATA$dataALL[DATA$dataALL$Period == DATA$TESTname, TYPE] * COEF

		TAB	<-	eventReactive(input$modeUPD,	{
			NULL
			if	(isTruthy(FILT))	modesTAB(FILT, UNIT, input$modeNUM, input$modeLOW, input$modeUPP)
		})
		
		output$modeTAB	=	renderTable({	as.data.frame(TAB())	},	digits = reactive(roundTerm()), rownames = TRUE)
})}

modesCLEARServer	<-	function(name)	{	dataSelLOAD	<-	reactive(input$dataSelLOAD)
	moduleServer(name, function(input, output, session){
		observeEvent(dataSelLOAD(),	output$modeTAB	<-	NULL	)
})}

#	calling the functions to calculate number of modes whenever new data is loaded, and clearing the tables
observeEvent(input$dataSelLOAD,	{
	modesNUMServer('TEMP',		"CPU_Temp")
	modesNUMServer('FREQ',		"Frequency")
	modesNUMServer('SOCK',		"Socket_Energy",	1/1000)
	modesNUMServer('CORE',		"Core_Energy",		1/1000)
	modesNUMServer('UNCORE',	"Uncore_Energy",	1/1000)
})
lapply(c('TEMP', 'FREQ', 'SOCK', 'CORE', 'UNCORE'), modesCLEARServer)

#	Mode tables updated by their respective buttons
observeEvent(input[[NS("TEMP", "modeUPD")]],	{modesLOCServer('TEMP',		"CPU_Temp",			"°C")	},			ignoreInit = TRUE)
observeEvent(input[[NS("FREQ", "modeUPD")]],	{modesLOCServer('FREQ',		"Frequency",		"MHz")	},			ignoreInit = TRUE)
observeEvent(input[[NS("SOCK", "modeUPD")]],	{modesLOCServer('SOCK',		"Socket_Energy",	"W",	1/1000)},	ignoreInit = TRUE)
observeEvent(input[[NS("CORE", "modeUPD")]],	{modesLOCServer('CORE',		"Core_Energy",		"W",	1/1000)},	ignoreInit = TRUE)
observeEvent(input[[NS("UNCORE", "modeUPD")]],	{modesLOCServer('UNCORE',	"Uncore_Energy",	"W",	1/1000)},	ignoreInit = TRUE)
