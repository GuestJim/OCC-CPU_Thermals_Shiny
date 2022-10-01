if (!require(R.utils))	install.package("R.utils")
library(R.utils)

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

CROSStest	<-	reactive(	tempCROSS(DATA$dataALL,	DATA$TESTname,	to.NUM(input$tableCROSStest))	)	%>%	bindEvent(input$dataSelLOAD, input$tableCROSStest)
CROSScool	<-	reactive(	tempCROSS(DATA$dataALL,	"Cooldown",		to.NUM(input$tableCROSScool))	)	%>%	bindEvent(input$dataSelLOAD, input$tableCROSScool)

output$tableCROSStest	<-	renderTable({	
	CROSStest()[1:min(nrow(CROSStest()), input$tableCROSSlim), ]
},	striped	=	TRUE)
output$tableINTERtest	<-	renderTable({
	out	<-	seqToIntervals(CROSStest()$Time)
	interFORMdiff(out)[1:min(nrow(out), input$tableCROSSlim), ]
},	striped	=	TRUE)

output$tableCROSScool	=	renderTable({
	CROSScool()[1:min(nrow(CROSScool()), input$tableCROSSlim), ]
	},	striped	=	TRUE)
output$tableINTERcool	<-	renderTable({
	out	<-	seqToIntervals(CROSScool()$Time)
	interFORMdiff(out)[1:min(nrow(out), input$tableCROSSlim), ]
},	striped	=	TRUE)