if (!require(multimode))	install.packages('multimode')
library(multimode)

modesTAB	<-	function(IN, UNIT, NUM, LOW, UPP)	{
	modesLOC	=	locmodes(IN, mod0 = NUM, lowsup = LOW, uppsup = UPP)$location
	modesLOCtab	<-	t(data.frame(Modes = modesLOC[seq(1, length(modesLOC), by = 2)]))
	
	colnames(modesLOCtab)	<-	paste0("Mode ", 1:ncol(modesLOCtab))
	rownames(modesLOCtab)	<-	paste0("Unit: ", UNIT)
	
	return(modesLOCtab)
}

modesNUMserver	<-	function(id, IN, BIN, LOW, UPP)	{
	if	(any(is.na(c(BIN, LOW, UPP))))	return(NULL)
	if	(BIN < 0)						return(NULL)
	if	(is.null(IN))					return(NULL)
	modesNUM	<-	nmodes(IN,	bw = BIN,	lowsup = LOW,	uppsup = UPP)
	
	if (is.numeric(modesNUM))	updateNumericInput(inputId = paste0("modes", as.character(id), "num"),	value = modesNUM)
}
#	with moduleServer I cannot get this to work, but as a normal function I can
FILT		<-	reactiveValues()
observeEvent(input$dataSelLOAD,	{
	# req(DATA$dataALL)
	FILT$CPU_Temp		<-	DATA$dataALL[DATA$dataALL$Period == DATA$TESTname, "CPU_Temp"]
	FILT$Frequency		<-	DATA$dataALL[DATA$dataALL$Period == DATA$TESTname, "Frequency"]
	FILT$Socket_Energy	<-	DATA$dataALL[DATA$dataALL$Period == DATA$TESTname, "Socket_Energy"]/1000
	FILT$Core_Energy	<-	DATA$dataALL[DATA$dataALL$Period == DATA$TESTname, "Core_Energy"]/1000
	FILT$Uncore_Energy	<-	DATA$dataALL[DATA$dataALL$Period == DATA$TESTname, "Uncore_Energy"]/1000
	
	output$modesTEMP	<-	NULL
	output$modesFREQ	<-	NULL
	output$modesSOCK	<-	NULL
	output$modesCORE	<-	NULL
	output$modesUNCORE	<-	NULL

	observe(	modesNUMserver("TEMP",		FILT$CPU_Temp,		input$modesTEMPbin, input$modesTEMPlow, input$modesTEMPupp)	)
	observe(	modesNUMserver("FREQ",		FILT$Frequency,		input$modesFREQbin, input$modesFREQlow, input$modesFREQupp)	)
	observe(	modesNUMserver("SOCK",		FILT$Socket_Energy,	input$modesSOCKbin, input$modesSOCKlow, input$modesSOCKupp)	)
	observe(	modesNUMserver("CORE",		FILT$Core_Energy,	input$modesCOREbin, input$modesCORElow, input$modesCOREupp)	)
	observe(	modesNUMserver("UNCORE",	FILT$Uncore_Energy,	input$modesUNCOREbin, input$modesUNCORElow, input$modesUNCOREupp)	)
# },	priority	=	5	)
})

#	Temperature
observeEvent(input$modesTEMPupd,	{
	modesLOCtab	<-	modesTAB(FILT$CPU_Temp, "°C", input$modesTEMPnum, input$modesTEMPlow, input$modesTEMPupp)
	
	observeEvent(input$roundTerm,	{
		output$modesTEMP	=	renderTable({	modesLOCtab	},	digits = input$roundTerm, rownames = TRUE)
	})
},	ignoreInit = TRUE)

#	Frequency
observeEvent(input$modesFREQupd,	{
	modesLOCtab	<-	modesTAB(FILT$Frequncy, "MHz", input$modesFREQnum, input$modesFREQlow, input$modesFREQupp)
	
	observeEvent(input$roundTerm,	{
		output$modesFREQ	=	renderTable({	modesLOCtab	},	digits = input$roundTerm, rownames = TRUE)
	})
},	ignoreInit = TRUE)

#	Socket Energy
observeEvent(input$modesSOCKupd,	{
	modesLOCtab	<-	modesTAB(FILT$Socket_Energy, "W", input$modesSOCKnum, input$modesSOCKlow, input$modesSOCKupp)
	
	observeEvent(input$roundTerm,	{
		output$modesSOCK	=	renderTable({	modesLOCtab	},	digits = input$roundTerm, rownames = TRUE)
	})
},	ignoreInit = TRUE)

#	Core Energy
observeEvent(input$modesCOREupd,	{
	modesLOCtab	<-	modesTAB(FILT$Core_Energy, "W", input$modesCOREnum, input$modesCORElow, input$modesCOREupp)
	
	observeEvent(input$roundTerm,	{
		output$modesCORE	=	renderTable({	modesLOCtab	},	digits = input$roundTerm, rownames = TRUE)
	})
},	ignoreInit = TRUE)

#	Uncore Energy
observeEvent(input$modesUNCOREupd,	{
	modesLOCtab	<-	modesTAB(FILT$Uncore_Energy, "W", input$modesUNCOREnum, input$modesUNCORElow, input$modesUNCOREupp)
	
	observeEvent(input$roundTerm,	{
		output$modesUNCORE	=	renderTable({	modesLOCtab	},	digits = input$roundTerm, rownames = TRUE)
	})
},	ignoreInit = TRUE)
