observeEvent(list(input$dataSelLOAD, DATA$LOAD),	{
	req(DATA$dataALL)
	updateNumericInput(inputId	=	"brushMEANstart",	value	=	-DATA$warm						)
	updateNumericInput(inputId	=	"brushMEANlength",	value	=	2 * DATA$duration + DATA$warm	)
	
	updateNumericInput(inputId	=	"brushMAXstart",	value	=	-DATA$warm						)
	updateNumericInput(inputId	=	"brushMAXlength",	value	=	2 * DATA$duration + DATA$warm	)
	
	updateNumericInput(inputId	=	"brushFREQstart",	value	=	-DATA$warm						)
	updateNumericInput(inputId	=	"brushFREQlength",	value	=	2 * DATA$duration + DATA$warm	)
	
	updateNumericInput(inputId	=	"brushPOWERstart",	value	=	-DATA$warm						)
	updateNumericInput(inputId	=	"brushPOWERlength",	value	=	2 * DATA$duration + DATA$warm	)
})

#	Mean
graphMEANzoom	=	reactive({
	output$brushMEANzoom	=	renderPlot({
		GRAPH$graphMEAN(GRAPH$FREQ.COEF) + 
		coord_cartesian(xlim = brushMEANzoom$x,	expand = FALSE) +
		labs(subtitle = paste0("X: ", paste(round(brushMEANzoom$x, 2), collapse = " to "), " (s)")	)
	})
})

brushMEANzoom	=	reactiveValues(x = c(-Inf, Inf),	y = c(-Inf, Inf),	FILTER	=	TRUE)
observeEvent(input$COEFupd,	{
	observeEvent(input$brushMEAN, {
		req(DATA$dataALL)
		brush 		<- input$brushMEAN
		
		brushMEANzoom$x			<-	NULL
		if (!is.null(brush)) 	brushMEANzoom$x			<-	c(brush$xmin, brush$xmax)

		updateNumericInput(inputId	=	"brushMEANstart",	value	=	round(	brushMEANzoom$x[1], 		2)	)
		updateNumericInput(inputId	=	"brushMEANlength",	value	=	round(	abs(diff(brushMEANzoom$x)),	2)	)
		
		graphMEANzoom()
	})

	observeEvent(input$brushMEANupdate,	{
		req(DATA$dataALL)
		brushMEANzoom$x	=	c(input$brushMEANstart, input$brushMEANstart + input$brushMEANlength)
		
		graphMEANzoom()
	})

	observeEvent(input$brushMEANdbl,	{
		brush 		<-	input$brushMEANdbl
		brushMEANzoom$x			<-	c(min(DATA$dataALL$Time), max(DATA$dataALL$Time))
		
		updateNumericInput(inputId	=	"brushMEANstart",	value	=	brushMEANzoom$x[1])
		updateNumericInput(inputId	=	"brushMEANlength",	value	=	brushMEANzoom$x[2] - brushMEANzoom$x[1])
			
		graphMEANzoom()
	})
},	ignoreNULL = FALSE)


#	Max
graphMAXzoom	=	reactive({
	output$brushMAXzoom	=	renderPlot({
		GRAPH$graphMAX(GRAPH$FREQ.COEF) + 
		coord_cartesian(xlim = brushMAXzoom$x,	expand = FALSE) +
		labs(subtitle = paste0("X: ", paste(round(brushMAXzoom$x, 2), collapse = " to "), " (s)")	)
	})
})

brushMAXzoom	=	reactiveValues(x = c(-Inf, Inf),	y = c(-Inf, Inf),	FILTER	=	TRUE)
observeEvent(input$COEFupd,	{
	observeEvent(input$brushMAX, {
		req(DATA$dataALL)
		brush 		<- input$brushMAX
		
		brushMAXzoom$x			<-	NULL
		if (!is.null(brush)) 	brushMAXzoom$x			<-	c(brush$xmin, brush$xmax)

		updateNumericInput(inputId	=	"brushMAXstart",	value	=	round(	brushMAXzoom$x[1], 		2)	)
		updateNumericInput(inputId	=	"brushMAXlength",	value	=	round(	abs(diff(brushMAXzoom$x)),	2)	)
		
		graphMAXzoom()
	})

	observeEvent(input$brushMAXupdate,	{
		req(DATA$dataALL)
		brushMAXzoom$x	=	c(input$brushMAXstart, input$brushMAXstart + input$brushMAXlength)
		
		graphMAXzoom()
	})

	observeEvent(input$brushMAXdbl,	{
		brush 		<-	input$brushMAXdbl
		brushMAXzoom$x			<-	c(min(DATA$dataALL$Time), max(DATA$dataALL$Time))
		
		updateNumericInput(inputId	=	"brushMAXstart",	value	=	brushMAXzoom$x[1])
		updateNumericInput(inputId	=	"brushMAXlength",	value	=	brushMAXzoom$x[2] - brushMAXzoom$x[1])
			
		graphMAXzoom()
	})
},	ignoreNULL = FALSE)


#	Frequency per Thread
graphFREQfilt	=	function(FILT, FREQ.COEF, CROP.x = NULL)	{
	GRAPH$graphFREQ(FREQ.COEF, FILT) + 
	coord_cartesian(xlim = CROP.x,	expand = FALSE) +
	labs(subtitle = paste0("X: ", paste(round(CROP.x, 2), collapse = " to "), " (s)")	)
}

observeEvent(list(input$COEFupd, brushFREQzoom$x), {	
	req(brushFREQzoom$FILTER)
	hold	<-	lapply(unique(DATA$dataALL$Thread), function(i) {
		tagList(
			strong(paste0("Thread: ", i)),
			renderPlot({
				graphFREQfilt(DATA$dataALL$Thread == i, GRAPH$FREQ.COEF, brushFREQzoom$x)
				}, height = 480)
			)
		})
	
	observeEvent(input$threadSEL, {
		req(input$threadSEL)
		output$brushFREQzoomFILT	<-	renderUI({	hold[as.numeric(input$threadSEL) + 1]	})
	})
})

brushFREQzoom	=	reactiveValues(x = c(min(DATA$dataALL$Time), max(DATA$dataALL$Time)),	y = c(-Inf, Inf),	FILTER	=	FALSE)
observeEvent(input$COEFupd,	{
	observeEvent(input$brushFREQ, {
		req(DATA$dataALL)
		brush 		<- input$brushFREQ
		
		brushFREQzoom$x			<-	NULL
		if (!is.null(brush)) 	brushFREQzoom$x			<-	c(brush$xmin, brush$xmax)

		updateNumericInput(inputId	=	"brushFREQstart",	value	=	round(	brushFREQzoom$x[1], 		2)	)
		updateNumericInput(inputId	=	"brushFREQlength",	value	=	round(	abs(diff(brushFREQzoom$x)),	2)	)
		
		brushFREQzoom$FILTER	<-	TRUE
	})

	observeEvent(input$brushFREQupdate,	{
		req(DATA$dataALL)
		brushFREQzoom$x	=	c(input$brushFREQstart, input$brushFREQstart + input$brushFREQlength)
		
		brushFREQzoom$FILTER	<-	TRUE
	})

	observeEvent(input$brushFREQdbl,	{
		brush 		<-	input$brushFREQdbl
		brushFREQzoom$x			<-	c(min(DATA$dataALL$Time), max(DATA$dataALL$Time))
		
		updateNumericInput(inputId	=	"brushFREQstart",	value	=	brushFREQzoom$x[1])
		updateNumericInput(inputId	=	"brushFREQlength",	value	=	brushFREQzoom$x[2] - brushFREQzoom$x[1])

		brushFREQzoom$FILTER	<-	TRUE
	})
},	ignoreNULL = FALSE)

#	Power per Core
graphPOWERfilt	=	function(FILT, FREQ.COEF, CROP.x = NULL)	{
	GRAPH$graphPOWER(FREQ.COEF, FILT) + 
	coord_cartesian(xlim = CROP.x,	expand = FALSE) +
	labs(subtitle = paste0("X: ", paste(round(CROP.x, 2), collapse = " to "), " (s)")	)
}

observeEvent(list(input$COEFupd, brushPOWERzoom$x), {	
	req(brushPOWERzoom$FILTER)
	hold	<-	lapply(unique(DATA$dataALL$Core), function(i) {
		tagList(
			strong(paste0("Core: ", i)),
			renderPlot({
				graphPOWERfilt(DATA$dataALL$Core == i, GRAPH$FREQ.COEF, brushPOWERzoom$x)
				}, height = 480)
			)
		})
	
	observeEvent(input$coreSEL, {
		req(input$coreSEL)
		output$brushPOWERzoomFILT	<-	renderUI({	hold[as.numeric(input$coreSEL) + 1]	})
	})
})

brushPOWERzoom	=	reactiveValues(x = c(min(DATA$dataALL$Time), max(DATA$dataALL$Time)),	y = c(-Inf, Inf),	FILTER	=	FALSE)
observeEvent(input$COEFupd,	{
	observeEvent(input$brushPOWER, {
		req(DATA$dataALL)
		brush 		<- input$brushPOWER
		
		brushPOWERzoom$x			<-	NULL
		if (!is.null(brush)) 	brushPOWERzoom$x			<-	c(brush$xmin, brush$xmax)

		updateNumericInput(inputId	=	"brushPOWERstart",	value	=	round(	brushPOWERzoom$x[1], 		2)	)
		updateNumericInput(inputId	=	"brushPOWERlength",	value	=	round(	abs(diff(brushPOWERzoom$x)),	2)	)
		brushPOWERzoom$FILTER	<-	TRUE
	})

	observeEvent(input$brushPOWERupdate,	{
		req(DATA$dataALL)
		brushPOWERzoom$x	=	c(input$brushPOWERstart, input$brushPOWERstart + input$brushPOWERlength)
		brushPOWERzoom$FILTER	<-	TRUE
	})

	observeEvent(input$brushPOWERdbl,	{
		brush 		<-	input$brushPOWERdbl
		brushPOWERzoom$x			<-	c(min(DATA$dataALL$Time), max(DATA$dataALL$Time))
		
		updateNumericInput(inputId	=	"brushPOWERstart",	value	=	brushPOWERzoom$x[1])
		updateNumericInput(inputId	=	"brushPOWERlength",	value	=	brushPOWERzoom$x[2] - brushPOWERzoom$x[1])
		brushPOWERzoom$FILTER	<-	TRUE
	})
},	ignoreNULL = FALSE)