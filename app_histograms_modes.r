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

modesLOCServer	<-	function(id, TYPE, UNIT, roundTerm = 2, COEF = 1)	{moduleServer(id, function(input, output, session)	{
	FILT	<-	DATA$dataALL[DATA$dataALL$Period == DATA$TESTname, TYPE] * COEF

	TAB	<-	eventReactive(input$modeUPD,	{
		NULL
		if	(isTruthy(FILT))	modesTAB(FILT, UNIT, input$modeNUM, input$modeLOW, input$modeUPP)
	})
	
	output$modeTAB	=	renderTable({	as.data.frame(TAB())	},	digits = roundTerm, rownames = TRUE)
})}

observeEvent(input$dataSelLOAD,	{
	modesNUMServer('TEMP',		"CPU_Temp")
	modesNUMServer('FREQ',		"Frequency")
	modesNUMServer('SOCK',		"Socket_Energy",	1/1000)
	modesNUMServer('CORE',		"Core_Energy",		1/1000)
	modesNUMServer('UNCORE',	"Uncore_Energy",	1/1000)
	observeEvent(input$roundTerm,	{
		modesLOCServer('TEMP',		"CPU_Temp",			"°C",	input$roundTerm)
		modesLOCServer('FREQ',		"Frequency",		"MHz",	input$roundTerm)
		modesLOCServer('SOCK',		"Socket_Energy",	"W",	input$roundTerm,	1/1000)
		modesLOCServer('CORE',		"Core_Energy",		"W",	input$roundTerm,	1/1000)
		modesLOCServer('UNCORE',	"Uncore_Energy",	"W",	input$roundTerm,	1/1000)
	# },	ignoreInit = TRUE)
	})
})

# FILT		<-	reactiveValues()
# observeEvent(input$dataSelLOAD,	{
	# req(DATA$dataALL)
	# FILT$CPU_Temp		<-	DATA$dataALL[DATA$dataALL$Period == DATA$TESTname, "CPU_Temp"]
	# FILT$Frequency		<-	DATA$dataALL[DATA$dataALL$Period == DATA$TESTname, "Frequency"]
	# FILT$Socket_Energy	<-	DATA$dataALL[DATA$dataALL$Period == DATA$TESTname, "Socket_Energy"]/1000
	# FILT$Core_Energy	<-	DATA$dataALL[DATA$dataALL$Period == DATA$TESTname, "Core_Energy"]/1000
	# FILT$Uncore_Energy	<-	DATA$dataALL[DATA$dataALL$Period == DATA$TESTname, "Uncore_Energy"]/1000

	# output$modesTEMP	<-	NULL
	# output$modesFREQ	<-	NULL
	# output$modesSOCK	<-	NULL
	# output$modesCORE	<-	NULL
	# output$modesUNCORE	<-	NULL

	# observe(	modesNUMserver("TEMP",		FILT$CPU_Temp,		input$modesTEMPbin, input$modesTEMPlow, input$modesTEMPupp)	)
	# observe(	modesNUMserver("FREQ",		FILT$Frequency,		input$modesFREQbin, input$modesFREQlow, input$modesFREQupp)	)
	# observe(	modesNUMserver("SOCK",		FILT$Socket_Energy,	input$modesSOCKbin, input$modesSOCKlow, input$modesSOCKupp)	)
	# observe(	modesNUMserver("CORE",		FILT$Core_Energy,	input$modesCOREbin, input$modesCORElow, input$modesCOREupp)	)
	# observe(	modesNUMserver("UNCORE",	FILT$Uncore_Energy,	input$modesUNCOREbin, input$modesUNCORElow, input$modesUNCOREupp)	)
# },	priority	=	5	)
# })

#	Temperature
# observeEvent(input$modesTEMPupd,	{
	# modesLOCtab	<-	modesTAB(FILT$CPU_Temp, "°C", input$modesTEMPnum, input$modesTEMPlow, input$modesTEMPupp)

	# observeEvent(input$roundTerm,	{
		# output$modesTEMP	=	renderTable({	modesLOCtab	},	digits = input$roundTerm, rownames = TRUE)
	# })
# },	ignoreInit = TRUE)
