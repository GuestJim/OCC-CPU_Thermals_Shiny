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
		CROSStest	<-	tempCROSS(DATA$dataALL, DATA$TESTname, to.NUM(input$tableCROSStest))
		output$tableCROSStest	=	renderTable({
			CROSStest[1:input$tableCROSSlim, ]
			},	striped	=	TRUE)
		output$tableINTERtest	<-	renderTable({
			out	<-	seqToIntervals(CROSStest$Time)
			interFORMdiff(out)[1:min(nrow(out), input$tableCROSSlim), ]
		},	striped	=	TRUE)
	},	ignoreInit	=	TRUE)

	observeEvent(input$tableCROSScool,	{
		CROSScool	<-	tempCROSS(DATA$dataALL, "Cooldown", to.NUM(input$tableCROSScool))
		output$tableCROSScool	=	renderTable({
			CROSScool[1:input$tableCROSSlim, ]
			},	striped	=	TRUE)
		output$tableINTERcool	<-	renderTable({
			out	<-	seqToIntervals(CROSScool$Time)
			interFORMdiff(out)[1:min(nrow(out), input$tableCROSSlim), ]
		},	striped	=	TRUE)
	},	ignoreInit	=	TRUE)
})

if (!require(R.utils))	install.package("R.utils")
library(R.utils)

# output$aboveTABL	<-	renderTable({
	# hold	<-	PART()$"Time in Video"[PART()$PULSE >= input$aboveTHRS]
	# out	<-	seqToIntervals(hold)
	
	# data.frame("Intervals above Threshold" = apply(out, 1, function(IN)	paste(num2time(IN), collapse = " - ")), check.names = FALSE)
# },	striped = TRUE)
