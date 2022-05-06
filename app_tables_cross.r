
observeEvent(input$dataSelLOAD,	{
	mmTEST	=	c(
		min(DATA$dataALL[DATA$dataALL$Period == DATA$TESTname, "CPU_Temp"]),
		max(DATA$dataALL[DATA$dataALL$Period == DATA$TESTname, "CPU_Temp"])
	)
	updateNumericInput(inputId	=	"tableCROSStest",
		min	=	floor(mmTEST[1]),	max	=	ceiling(mmTEST[2]),
		value	=	mmTEST[2],
		label	=	paste0(c("Min: ", "Max: "), mmTEST, " °C", collapse = ", ")
	)
	
	mmCOOL	=	c(
		min(DATA$dataALL[DATA$dataALL$Period == "Cooldown", "CPU_Temp"]),
		max(DATA$dataALL[DATA$dataALL$Period == "Cooldown", "CPU_Temp"])
	)
	updateNumericInput(inputId	=	"tableCROSScool",
		min	=	floor(mmCOOL[1]),	max	=	ceiling(mmCOOL[2]),
		value	=	mmCOOL[1],
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