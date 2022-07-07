if (!require(multimode))	install.packages('multimode')
library(multimode)

#	Temperature
observeEvent(list(input$dataSelLOAD, DATA$LOAD, input$modesTEMPbin, input$modesTEMPlow, input$modesTEMPupp),	{
	req(DATA$dataALL$CPU_Temp)
	FILT	=	DATA$dataALL[DATA$dataALL$Period == DATA$TESTname, "CPU_Temp"]
	
	modesNUM	=	nmodes(FILT, bw = input$modesTEMPbin, lowsup = input$modesTEMPlow, uppsup = input$modesTEMPupp)
	updateNumericInput(inputId	=	"modesTEMPnum",	value	=	modesNUM)
	
	observeEvent(input$modesTEMPupd,	{
		modesLOC	=	locmodes(FILT, mod0 = input$modesTEMPnum, lowsup = input$modesTEMPlow, uppsup = input$modesTEMPupp)$location
		
		modesLOCtab	<-	t(data.frame(Modes = modesLOC[seq(1, length(modesLOC), by = 2)]))
		
		output$modesTEMP	=	renderTable({
			colnames(modesLOCtab)	<-	paste0("Loc ", 1:ncol(modesLOCtab))
			rownames(modesLOCtab)	<-	"Modes (°C)"
			modesLOCtab
		},	digits = input$roundTerm, rownames = TRUE)
	})
},	ignoreInit = TRUE)

#	Frequency
observeEvent(list(input$dataSelLOAD, DATA$LOAD, input$modesFREQbin, input$modesFREQlow, input$modesFREQupp),	{
	req(DATA$dataALL$Frequency)
	FILT	=	DATA$dataALL[DATA$dataALL$Period == DATA$TESTname, "Frequency"]
	
	modesNUM	=	nmodes(FILT, bw = input$modesFREQbin, lowsup = input$modesFREQlow, uppsup = input$modesFREQupp)
	updateNumericInput(inputId	=	"modesFREQnum",	value	=	modesNUM)
	
	observeEvent(input$modesFREQupd,	{
		modesLOC	=	locmodes(FILT, mod0 = input$modesFREQnum, lowsup = input$modesFREQlow, uppsup = input$modesFREQupp)$location
		
		modesLOCtab	<-	t(data.frame(Modes = modesLOC[seq(1, length(modesLOC), by = 2)]))
		
		output$modesFREQ	=	renderTable({
			colnames(modesLOCtab)	<-	paste0("Loc ", 1:ncol(modesLOCtab))
			rownames(modesLOCtab)	<-	"Modes (MHz)"
			modesLOCtab
		},	digits = input$roundTerm, rownames = TRUE)
	})
},	ignoreInit = TRUE)

#	Socket Power
observeEvent(list(input$dataSelLOAD, DATA$LOAD, input$modesSOCKbin, input$modesSOCKlow, input$modesSOCKupp),	{
	req(DATA$dataALL$Socket_Energy)
	FILT	=	DATA$dataALL[DATA$dataALL$Period == DATA$TESTname, "Socket_Energy"]
	
	modesNUM	=	nmodes(FILT/1000, bw = input$modesSOCKbin, lowsup = input$modesSOCKlow, uppsup = input$modesSOCKupp)
	updateNumericInput(inputId	=	"modesSOCKnum",	value	=	modesNUM)
	
	observeEvent(input$modesSOCKupd,	{
		modesLOC	=	locmodes(FILT/1000, mod0 = input$modesSOCKnum, lowsup = input$modesSOCKlow, uppsup = input$modesSOCKupp)$location
		
		modesLOCtab	<-	t(data.frame(Modes = modesLOC[seq(1, length(modesLOC), by = 2)]))
		
		output$modesSOCK	=	renderTable({
			colnames(modesLOCtab)	<-	paste0("Loc ", 1:ncol(modesLOCtab))
			rownames(modesLOCtab)	<-	"Modes (W)"
			modesLOCtab
		},	digits = input$roundTerm, rownames = TRUE)
	})
},	ignoreInit = TRUE)

#	Core Power
observeEvent(list(input$dataSelLOAD, DATA$LOAD, input$modesCOREbin, input$modesCORElow, input$modesCOREupp),	{
	req(DATA$dataALL$Core_Energy)
	FILT	=	DATA$dataALL[DATA$dataALL$Period == DATA$TESTname, "Core_Energy"]
	
	modesNUM	=	nmodes(FILT/1000, bw = input$modesCOREbin, lowsup = input$modesCORElow, uppsup = input$modesCOREupp)
	updateNumericInput(inputId	=	"modesCOREnum",	value	=	modesNUM)
	
	observeEvent(input$modesCOREupd,	{
		modesLOC	=	locmodes(FILT/1000, mod0 = input$modesCOREnum, lowsup = input$modesCORElow, uppsup = input$modesCOREupp)$location
		
		modesLOCtab	<-	t(data.frame(Modes = modesLOC[seq(1, length(modesLOC), by = 2)]))
		
		output$modesCORE	=	renderTable({
			colnames(modesLOCtab)	<-	paste0("Loc ", 1:ncol(modesLOCtab))
			rownames(modesLOCtab)	<-	"Modes (W)"
			modesLOCtab
		},	digits = input$roundTerm, rownames = TRUE)
	})
},	ignoreInit = TRUE)

#	Uncore Power
observeEvent(list(input$dataSelLOAD, DATA$LOADLOAD, input$modesUNCOREbin, input$modesUNCORElow, input$modesUNCOREupp),	{
	req(DATA$dataALL$Uncore_Energy)
	FILT	=	DATA$dataALL[DATA$dataALL$Period == DATA$TESTname, "Uncore_Energy"]
	
	modesNUM	=	nmodes(FILT/1000, bw = input$modesUNCOREbin, lowsup = input$modesUNCORElow, uppsup = input$modesUNCOREupp)
	updateNumericInput(inputId	=	"modesUNCOREnum",	value	=	modesNUM)
	
	observeEvent(input$modesUNCOREupd,	{
		modesLOC	=	locmodes(FILT/1000, mod0 = input$modesUNCOREnum, lowsup = input$modesUNCORElow, uppsup = input$modesUNCOREupp)$location
		
		modesLOCtab	<-	t(data.frame(Modes = modesLOC[seq(1, length(modesLOC), by = 2)]))
		
		output$modesUNCORE	=	renderTable({
			colnames(modesLOCtab)	<-	paste0("Loc ", 1:ncol(modesLOCtab))
			rownames(modesLOCtab)	<-	"Modes (W)"
			modesLOCtab
		},	digits = input$roundTerm, rownames = TRUE)
	})
},	ignoreInit = TRUE)