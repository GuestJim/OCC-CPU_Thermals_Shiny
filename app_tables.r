if (!require(tidyr))	install.packages('tidyr')
library(tidyr)

TABLE$warmOFF	=	eventReactive(input$medOFFapply,	{
	ifelse(input$medOFFapply, DATA$warmMED, 0)
})

TABLE$longSUM	<-	eventReactive(list(input$dataSelLOAD, DATA$LOAD, input$medOFFapply),	{
	req(DATA$DATAS, DATA$GROUPS)
	
	holdDATAS	=	DATA$DATAS
	holdDATAS$CPU_Temp	=	holdDATAS$CPU_Temp - TABLE$warmOFF()
	
	dataSUM	=	sepCOL(aggregate(holdDATAS, DATA$GROUPS, stats))
	holdSUM	=	pivot_longer(dataSUM,
		cols			=	-c(1:length(DATA$GROUPS)),
		names_to		=	c("Measurement", ".value"),
		names_sep		=	' - ',
		names_ptypes	=	list(Measurement = factor(ordered = TRUE))
	)

	levels(holdSUM$Measurement)	=	unitCOL(levels(holdSUM$Measurement))
	mJ2W(holdSUM)
},	ignoreInit	=	FALSE)

tableFILT	=	function(TAB)	{
	filtCOL	=	TRUE
	if (unique(TAB$Socket) == 0)	TAB$Socket	=	NULL
	#	to remove Socket when there is only one CPU
	filtCOL	=	c(which(names(TAB) %in% c("Period", "Socket", "Measurement")),	which(names(TAB) %in% c(input$listSTAT)))

	filtROW	=	TRUE
	MEAS	=	input$listMEAS
	if (input$engPOW == "W") MEAS	=	gsub("Energy", "Power", MEAS)
	filtROW	=	intersect(which(TAB$Period %in% input$listPERI), which(TAB$Measurement %in% unitCOL(MEAS)))

	return(TAB[filtROW, filtCOL])
}

TABLE$ORDER	<-	eventReactive(input$tabORDER,	{
	hold	<-	TABLE$longSUM()
	ORD		<-	order(hold$Period, hold$Measurement)
	if (input$tabORDER)	ORD	<-	order(hold$Measurement, hold$Period)
	ORD
})

observeEvent(list(input$roundTerm),	{
	output$tableSUMM	=	renderTable({
		tableFILT(TABLE$longSUM()[TABLE$ORDER(), ])
	},	digits	=	input$roundTerm,	striped	=	TRUE)
},	label = "Table - Summary")

if (VIEW$MULTtab)	source("app_tables_multi.r", local = TRUE)
source("app_tables_cross.r", local = TRUE)