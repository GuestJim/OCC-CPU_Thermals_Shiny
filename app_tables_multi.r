observeEvent(input$engPOW,	{
	updateTextInput(inputId = "multiSOCKecdf", label = paste0("Specific Values (", input$engPOW, ")"))
	updateTextInput(inputId = "multiCOREecdf", label = paste0("Specific Values (", input$engPOW, ")"))
})

#	CPU_Temp
TABLE$multiTEMPperc	<-	eventReactive(list(input$dataSelLOAD, input$multiTEMPperc),	{to.NUM(input$multiTEMPperc)}	)
TABLE$multiTEMPecdf	<-	eventReactive(list(input$dataSelLOAD, input$multiTEMPecdf),	{to.NUM(input$multiTEMPecdf)}	)

#	Frequency
TABLE$multiFREQperc	<-	eventReactive(list(input$dataSelLOAD, input$multiFREQperc),	{to.NUM(input$multiFREQperc)}	)
TABLE$multiFREQecdf	<-	eventReactive(list(input$dataSelLOAD, input$multiFREQecdf),	{to.NUM(input$multiFREQecdf)}	)

#	Socket_Energy
TABLE$multiSOCKperc	<-	eventReactive(list(input$dataSelLOAD, input$multiSOCKperc),	{to.NUM(input$multiSOCKperc)}	)
TABLE$multiSOCKecdf	<-	eventReactive(list(input$dataSelLOAD, input$multiSOCKecdf),	{to.NUM(input$multiSOCKecdf)}	)

#	Core_Energy
TABLE$multiCOREperc	<-	eventReactive(list(input$dataSelLOAD, input$multiCOREperc),	{to.NUM(input$multiCOREperc)}	)
TABLE$multiCOREecdf	<-	eventReactive(list(input$dataSelLOAD, input$multiCOREecdf),	{to.NUM(input$multiCOREecdf)}	)

observeEvent(input$roundTerm,	{
	#	CPU_Temp
	output$tableTEMPperc	=	renderTable({	tablePERC(DATA$dataALL, "CPU_Temp", TABLE$multiTEMPperc())
		},	digits	=	input$roundTerm,	striped	=	TRUE)
	
	output$tableTEMPecdf	=	renderTable({	tableECDF(DATA$dataALL, "CPU_Temp", TABLE$multiTEMPecdf())
		},	digits	=	input$roundTerm,	striped	=	TRUE)
	
	#	Frequency
	output$tableFREQperc	=	renderTable({	tablePERC(DATA$dataALL, "Frequency", TABLE$multiFREQperc())
		},	digits	=	input$roundTerm,	striped	=	TRUE)

	output$tableFREQecdf	=	renderTable({	tableECDF(DATA$dataALL, "Frequency", TABLE$multiFREQecdf())
		},	digits	=	input$roundTerm,	striped	=	TRUE)
	
	observeEvent(input$engPOW,	{
	#	Socket_Energy
	output$tableSOCKperc	=	renderTable({	tablePERC(DATA$dataALL, "Socket_Energy", TABLE$multiSOCKperc(), input$engPOW)
		},	digits	=	input$roundTerm,	striped	=	TRUE)
	
	output$tableSOCKecdf	=	renderTable({	tableECDF(DATA$dataALL, "Socket_Energy", TABLE$multiSOCKecdf(), input$engPOW)
		},	digits	=	input$roundTerm,	striped	=	TRUE)
	
	#	Core_Energy
	output$tableCOREperc	=	renderTable({	tablePERC(DATA$dataALL, "Core_Energy", TABLE$multiCOREperc(), input$engPOW)
		},	digits	=	input$roundTerm,	striped	=	TRUE)
	
	output$tableCOREecdf	=	renderTable({	tableECDF(DATA$dataALL, "Core_Energy", TABLE$multiCOREecdf(), input$engPOW)
		},	digits	=	input$roundTerm,	striped	=	TRUE)
	})
},	label = "Table - Summary")