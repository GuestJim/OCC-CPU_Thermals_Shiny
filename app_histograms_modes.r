if (!require(multimode))	install.packages('multimode')
library(multimode)

#	Temperature
observeEvent(list(input$dataSelLOAD, DATA$LOAD),	{
	req(DATA$dataALL$CPU_Temp)
	FILT	=	DATA$dataALL[DATA$dataALL$Period == DATA$TESTname, "CPU_Temp"]
	
	observeEvent(list(input$modesTEMPbin, input$modesTEMPlow, input$modesTEMPupp),	{
		modesNUM	=	nmodes(FILT, bw = input$modesTEMPbin, lowsup = input$modesTEMPlow, uppsup = input$modesTEMPupp)
		updateNumericInput(inputId	=	"modesTEMPnum",	value	=	modesNUM)
	})
	
	observeEvent(list(input$modesTEMPupd, input$roundTerm),	{
		modesLOC	=	locmodes(FILT, mod0 = input$modesTEMPnum, lowsup = input$modesTEMPlow, uppsup = input$modesTEMPupp)$location
		modesLOCtab	<-	t(data.frame(Modes = modesLOC[seq(1, length(modesLOC), by = 2)]))
		
		output$modesTEMP	=	renderTable({
			colnames(modesLOCtab)	<-	paste0("Loc ", 1:ncol(modesLOCtab))
			rownames(modesLOCtab)	<-	"Modes (°C)"
			modesLOCtab
		},	digits = input$roundTerm, rownames = TRUE)
	},	ignoreInit = TRUE)
},	ignoreInit = TRUE)

#	Frequency
observeEvent(list(input$dataSelLOAD, DATA$LOAD),	{
	req(DATA$dataALL$Frequency)
	FILT	=	DATA$dataALL[DATA$dataALL$Period == DATA$TESTname, "Frequency"]
	
	observeEvent(list(input$modesFREQbin, input$modesFREQlow, input$modesFREQupp),	{
		modesNUM	=	nmodes(FILT, bw = input$modesFREQbin, lowsup = input$modesFREQlow, uppsup = input$modesFREQupp)
		updateNumericInput(inputId	=	"modesFREQnum",	value	=	modesNUM)
	})
	
	observeEvent(list(input$modesFREQupd, input$roundTerm),	{
		modesLOC	=	locmodes(FILT, mod0 = input$modesFREQnum, lowsup = input$modesFREQlow, uppsup = input$modesFREQupp)$location
		
		modesLOCtab	<-	t(data.frame(Modes = modesLOC[seq(1, length(modesLOC), by = 2)]))
		
		output$modesFREQ	=	renderTable({
			colnames(modesLOCtab)	<-	paste0("Loc ", 1:ncol(modesLOCtab))
			rownames(modesLOCtab)	<-	"Modes (MHz)"
			modesLOCtab
		},	digits = input$roundTerm, rownames = TRUE)
	},	ignoreInit = TRUE)
},	ignoreInit = TRUE)

#	Socket Power
observeEvent(list(input$dataSelLOAD, DATA$LOAD),	{
	req(DATA$dataALL$Socket_Energy)
	FILT	=	DATA$dataALL[DATA$dataALL$Period == DATA$TESTname, "Socket_Energy"]
	
	observeEvent(list(input$modesSOCKbin, input$modesSOCKlow, input$modesSOCKupp), {
		modesNUM	=	nmodes(FILT/1000, bw = input$modesSOCKbin, lowsup = input$modesSOCKlow, uppsup = input$modesSOCKupp)
		updateNumericInput(inputId	=	"modesSOCKnum",	value	=	modesNUM)
	})
	
	observeEvent(list(input$modesSOCKupd, input$roundTerm),	{
		modesLOC	=	locmodes(FILT/1000, mod0 = input$modesSOCKnum, lowsup = input$modesSOCKlow, uppsup = input$modesSOCKupp)$location
		
		modesLOCtab	<-	t(data.frame(Modes = modesLOC[seq(1, length(modesLOC), by = 2)]))
		
		output$modesSOCK	=	renderTable({
			colnames(modesLOCtab)	<-	paste0("Loc ", 1:ncol(modesLOCtab))
			rownames(modesLOCtab)	<-	"Modes (W)"
			modesLOCtab
		},	digits = input$roundTerm, rownames = TRUE)
	},	ignoreInit = TRUE)
},	ignoreInit = TRUE)

#	Core Power
observeEvent(list(input$dataSelLOAD, DATA$LOAD),	{
	req(DATA$dataALL$Core_Energy)
	FILT	=	DATA$dataALL[DATA$dataALL$Period == DATA$TESTname, "Core_Energy"]
	
	observeEvent(list(input$modesCOREbin, input$modesCORElow, input$modesCOREupp),	{
		modesNUM	=	nmodes(FILT/1000, bw = input$modesCOREbin, lowsup = input$modesCORElow, uppsup = input$modesCOREupp)
		updateNumericInput(inputId	=	"modesCOREnum",	value	=	modesNUM)
	})
	
	observeEvent(list(input$modesCOREupd, input$roundTerm),	{
		modesLOC	=	locmodes(FILT/1000, mod0 = input$modesCOREnum, lowsup = input$modesCORElow, uppsup = input$modesCOREupp)$location
		
		modesLOCtab	<-	t(data.frame(Modes = modesLOC[seq(1, length(modesLOC), by = 2)]))
		
		output$modesCORE	=	renderTable({
			colnames(modesLOCtab)	<-	paste0("Loc ", 1:ncol(modesLOCtab))
			rownames(modesLOCtab)	<-	"Modes (W)"
			modesLOCtab
		},	digits = input$roundTerm, rownames = TRUE)
	},	ignoreInit = TRUE)
},	ignoreInit = TRUE)

#	Uncore Power
observeEvent(list(input$dataSelLOAD, DATA$LOADLOAD),	{
	req(DATA$dataALL$Uncore_Energy)
	FILT	=	DATA$dataALL[DATA$dataALL$Period == DATA$TESTname, "Uncore_Energy"]
	
	observeEvent(list(input$modesUNCOREbin, input$modesUNCORElow, input$modesUNCOREupp),	{
		modesNUM	=	nmodes(FILT/1000, bw = input$modesUNCOREbin, lowsup = input$modesUNCORElow, uppsup = input$modesUNCOREupp)
		updateNumericInput(inputId	=	"modesUNCOREnum",	value	=	modesNUM)
	})
	
	observeEvent(list(input$modesUNCOREupd, input$roundTerm),	{
		modesLOC	=	locmodes(FILT/1000, mod0 = input$modesUNCOREnum, lowsup = input$modesUNCORElow, uppsup = input$modesUNCOREupp)$location
		
		modesLOCtab	<-	t(data.frame(Modes = modesLOC[seq(1, length(modesLOC), by = 2)]))
		
		output$modesUNCORE	=	renderTable({
			colnames(modesLOCtab)	<-	paste0("Loc ", 1:ncol(modesLOCtab))
			rownames(modesLOCtab)	<-	"Modes (W)"
			modesLOCtab
		},	digits = input$roundTerm, rownames = TRUE)
	},	ignoreInit = TRUE)
},	ignoreInit = TRUE)