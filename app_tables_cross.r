observeEvent(input$dataSelLOAD,	{
	tempTEST	=	DATA$dataALL[DATA$dataALL$Period == DATA$TESTname, "CPU_Temp"]
	mmTEST	=	c(	min(tempTEST),	max(tempTEST)	)
	updateNumericInput(inputId	=	"tableCROSStest",
		min	=	floor(mmTEST[1]),	max	=	ceiling(mmTEST[2]),
		value	=	quantile(tempTEST, 0.25, names = FALSE),
		label	=	paste0(c("Min: ", "Max: "), mmTEST, " °C", collapse = ", ")
	)
	
	tempCOOL	=	DATA$dataALL[DATA$dataALL$Period == "Cooldown", "CPU_Temp"]
	mmCOOL	=	c(	min(tempCOOL),	max(tempCOOL)	)
	updateNumericInput(inputId	=	"tableCROSScool",
		min	=	floor(mmCOOL[1]),	max	=	ceiling(mmCOOL[2]),
		value	=	quantile(tempCOOL, 0.75, names = FALSE),
		label	=	paste0(c("Min: ", "Max: "), mmCOOL, " °C", collapse = ", ")
	)
})

observeEvent(input$tableCROSSlim,	{
	observeEvent(input$tableCROSStest,	{
		output$tableCROSStest	=	renderTable({
			tempCROSS(DATA$dataALL, DATA$TESTname, to.NUM(input$tableCROSStest), LIST = input$tableCROSSlim)
			},	striped	=	TRUE)
	},	ignoreInit	=	TRUE)

	observeEvent(input$tableCROSScool,	{
		output$tableCROSScool	=	renderTable({
			tempCROSS(DATA$dataALL, "Cooldown", to.NUM(input$tableCROSScool), LIST = input$tableCROSSlim)
			},	striped	=	TRUE)
	},	ignoreInit	=	TRUE)
})