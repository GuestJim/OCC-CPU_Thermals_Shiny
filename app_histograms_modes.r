if (!require(multimode))	install.packages('multimode')
library(multimode)

modesTAB	<-	function(IN, UNIT, NUM, LOW, UPP)	{
	modesLOC	=	locmodes(IN, mod0 = NUM, lowsup = LOW, uppsup = UPP)$location
	modesLOCtab	<-	t(data.frame(Modes = modesLOC[seq(1, length(modesLOC), by = 2)]))
	
	colnames(modesLOCtab)	<-	paste0("Mode ", 1:ncol(modesLOCtab))
	rownames(modesLOCtab)	<-	paste0("Unit: ", UNIT)
	
	return(modesLOCtab)
}

#	Temperature
observeEvent(list(input$dataSelLOAD, DATA$LOAD),	{
	req(DATA$dataALL$CPU_Temp)
	FILT	=	DATA$dataALL[DATA$dataALL$Period == DATA$TESTname, "CPU_Temp"]
	modesNUM	<-	1
	
	observeEvent(list(input$modesTEMPbin, input$modesTEMPlow, input$modesTEMPupp),	{
		if (is.numeric(c(input$modesTEMPbin, input$modesTEMPlow, input$modesTEMPupp)))	if (input$modesTEMPbin > 0)	{
			modesNUM	=	nmodes(FILT,
				bw = input$modesTEMPbin,	lowsup = input$modesTEMPlow,	uppsup = input$modesTEMPupp)	}
			
		if (is.numeric(modesNUM)) updateNumericInput(inputId	=	"modesTEMPnum",	value	=	modesNUM)
	})

	observeEvent(input$modesTEMPupd,	{
		modesLOCtab	<-	modesTAB(FILT, "°C", input$modesTEMPnum, input$modesTEMPlow, input$modesTEMPupp)
		
		observeEvent(input$roundTerm,	{
			output$modesTEMP	=	renderTable({
				modesLOCtab
			},	digits = input$roundTerm, rownames = TRUE)
		})
	},	ignoreInit = TRUE)
},	ignoreInit = TRUE)

#	Frequency
observeEvent(list(input$dataSelLOAD, DATA$LOAD),	{
	req(DATA$dataALL$Frequency)
	FILT	=	DATA$dataALL[DATA$dataALL$Period == DATA$TESTname, "Frequency"]
	modesNUM	<-	1
	
	observeEvent(list(input$modesFREQbin, input$modesFREQlow, input$modesFREQupp),	{
		if (is.numeric(c(input$modesFREQbin, input$modesFREQlow, input$modesFREQupp)))	if (input$modesFREQbin > 0)	{
			modesNUM	=	nmodes(FILT,
				bw = input$modesFREQbin,	lowsup = input$modesFREQlow,	uppsup = input$modesFREQupp)	}
			
		if (is.numeric(modesNUM))	updateNumericInput(inputId	=	"modesFREQnum",	value	=	modesNUM)
	})

	observeEvent(input$modesFREQupd,	{
		modesLOCtab	<-	modesTAB(FILT, "MHz", input$modesFREQnum, input$modesFREQlow, input$modesFREQupp)
		
		observeEvent(input$roundTerm,	{
			output$modesFREQ	=	renderTable({
				modesLOCtab
			},	digits = input$roundTerm, rownames = TRUE)
		})
	},	ignoreInit = TRUE)
},	ignoreInit = TRUE)

#	Socket 
observeEvent(list(input$dataSelLOAD, DATA$LOAD),	{
	req(DATA$dataALL$Socket_Energy)
	FILT	=	DATA$dataALL[DATA$dataALL$Period == DATA$TESTname, "Socket_Energy"]/1000
	modesNUM	<-	1
	
	observeEvent(list(input$modesSOCKbin, input$modesSOCKlow, input$modesSOCKupp),	{
		if (is.numeric(c(input$modesSOCKbin, input$modesSOCKlow, input$modesSOCKupp)))	if (input$modesSOCKbin > 0)	{
			modesNUM	=	nmodes(FILT,
				bw = input$modesSOCKbin,	lowsup = input$modesSOCKlow,	uppsup = input$modesSOCKupp)	}
			
		if (is.numeric(modesNUM))	updateNumericInput(inputId	=	"modesSOCKnum",	value	=	modesNUM)
	})

	observeEvent(input$modesSOCKupd,	{
		modesLOCtab	<-	modesTAB(FILT, "W", input$modesSOCKnum, input$modesSOCKlow, input$modesSOCKupp)
		
		observeEvent(input$roundTerm,	{
			output$modesSOCK	=	renderTable({
				modesLOCtab
			},	digits = input$roundTerm, rownames = TRUE)
		})
	},	ignoreInit = TRUE)
},	ignoreInit = TRUE)

#	Core Power
observeEvent(list(input$dataSelLOAD, DATA$LOAD),	{
	req(DATA$dataALL$Core_Energy)
	FILT	=	DATA$dataALL[DATA$dataALL$Period == DATA$TESTname, "Core_Energy"]/1000
	modesNUM	<-	1
	
	observeEvent(list(input$modesCOREbin, input$modesCORElow, input$modesCOREupp),	{
		if (is.numeric(c(input$modesCOREbin, input$modesCORElow, input$modesCOREupp)))	if (input$modesCOREbin > 0)	{
			modesNUM	=	nmodes(FILT,
				bw = input$modesCOREbin,	lowsup = input$modesCORElow,	uppsup = input$modesCOREupp)	}
			
		if (is.numeric(modesNUM))	updateNumericInput(inputId	=	"modesCOREnum",	value	=	modesNUM)
	})

	observeEvent(input$modesCOREupd,	{
		modesLOCtab	<-	modesTAB(FILT, "W", input$modesCOREnum, input$modesCORElow, input$modesCOREupp)
		
		observeEvent(input$roundTerm,	{
			output$modesCORE	=	renderTable({
				modesLOCtab
			},	digits = input$roundTerm, rownames = TRUE)
		})
	},	ignoreInit = TRUE)
},	ignoreInit = TRUE)

#	Uncore Power
observeEvent(list(input$dataSelLOAD, DATA$LOAD),	{
	req(DATA$dataALL$Uncore_Energy)
	FILT	=	DATA$dataALL[DATA$dataALL$Period == DATA$TESTname, "Uncore_Energy"]/1000
	modesNUM	<-	1
	
	observeEvent(list(input$modesUNCOREbin, input$modesUNCORElow, input$modesUNCOREupp),	{
		if (is.numeric(c(input$modesUNCOREbin, input$modesUNCORElow, input$modesUNCOREupp)))	if (input$modesUNCOREbin > 0)	{
			modesNUM	=	nmodes(FILT,
				bw = input$modesUNCOREbin,	lowsup = input$modesUNCORElow,	uppsup = input$modesUNCOREupp)	}
			
		if (is.numeric(modesNUM))	updateNumericInput(inputId	=	"modesUNCOREnum",	value	=	modesNUM)
	})

	observeEvent(input$modesUNCOREupd,	{
		modesLOCtab	<-	modesTAB(FILT, "°C", input$modesUNCOREnum, input$modesUNCORElow, input$modesUNCOREupp)
		
		observeEvent(input$roundTerm,	{
			output$modesUNCORE	=	renderTable({
				modesLOCtab
			},	digits = input$roundTerm, rownames = TRUE)
		})
	},	ignoreInit = TRUE)
},	ignoreInit = TRUE)