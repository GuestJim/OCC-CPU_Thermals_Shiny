TIME	=	new.env()
TIME$COLORsets	=	list(
	"Socket_Energy"		=	"Socket Power",
	"Core_Energy"		=	"Core Power",
	"Frequency"			=	"Frequency",
	"CPU_Temp"			=	"Temperature"
)
TIME$NAMEsets	=	list(
	"Socket_Energy"		=	"Power (W)",
	"Core_Energy"		=	"Power (W)",
	"Frequency"			=	"Frequency (MHz)",
	"CPU_Temp"			=	"Temperature (°C)"
)
TIME$BREAKsets	=	list(
	"Core_Energy"		=	seq(0,	100,	by = 0.5),
	"Socket_Energy"		=	seq(0,	600,	by = 5),
	"Frequency"			=	waiver(),
	"CPU_Temp"			=	seq(0,	200,	by = 1)
)

TIME$DF	=	function(COL, DELT = DATA$duration)	{
	unitADJ	=	1
	if (grepl("Energy", COL))	unitADJ	=	1000
	TS	=	decompose(ts(DATA$dataALL[DATA$dataALL$Period == DATA$TESTname, COL] / unitADJ, deltat = 1/DELT, start = 0), type = "additive")	
		#	start = 0 to align with Time labels
	TS.df	=	data.frame(as.vector(time(TS$trend)), as.vector(TS$trend), as.vector(TS$seasonal))
	colnames(TS.df)	=	c("Index", "Trend", "Seasonal")
	return(TS.df)
}

	# TEXT	=	NULL
	# if (COL == "Frequency")	{
		# smoothDATA	=	layer_data(ggplot() + stat_smooth(data = TS.df, aes(x = Index, y = Trend), na.rm = TRUE))
		# MIN	=	which.min(smoothDATA$ymin)	;	MAX	=	which.max(smoothDATA$ymax)
		# TEXT		=	list(
			# annotate("label",	label	=	round(smoothDATA[MIN, "ymin"], 2),
				# x	=	smoothDATA[MIN, "x"],	y	=	smoothDATA[MIN, "ymin"],
				# hjust	=	"inward",	vjust	=	"inward"
				# ),
			# annotate("label",	label	=	round(smoothDATA[MAX, "ymax"], 2),
				# x	=	smoothDATA[MAX, "x"],	y	=	smoothDATA[MAX, "ymax"],
				# hjust	=	"inward",	vjust	=	"outward"
				# )
		# )
	# }
	

TIME$timeTREND	=	function(COL, DELT = DATA$duration, ...)	{
	if (!exists("TS.df"))	TS.df	=	TIME$DF(COL, DELT)
	TEXT	=	NULL
	
	ggplot(data = TS.df, aes(x = Index, y = Trend)) + 
	ggtitle(paste0(COL, " - Time Series Trend")) + GRAPH$CAPTION + 
	geom_line(aes(color = TIME$COLORsets[[COL]]), show.legend = FALSE) + 
	stat_smooth(na.rm = TRUE) + TEXT + 
	scale_x_continuous(
		name	=	"Approximate Time (seconds)",
		expand	=	c(0.02, 0),
		breaks	=	seq(0,	max(TS.df$Index),	by = max(TS.df$Index)/6),
		labels	=	seq(0,	DATA$duration,		by = DATA$duration/6),
		limits	=	c(0, NA)
		) + 
	scale_y_continuous(name = TIME$NAMEsets[[COL]],	breaks = TIME$BREAKsets[[COL]]) + 
	theme(plot.title.position = "plot") + GRAPH$COLORS
}

TIME$timeSEAS	=	function(COL, DELT = DATA$duration, ...)	{
	if (!exists("TS.df"))	TS.df	=	TIME$DF(COL, DELT)
	TEXT	=	NULL
	
	ggplot(data = TS.df, aes(x = Index, y = Seasonal + median(Trend, na.rm = TRUE))) +  
	ggtitle(paste0(COL, " - Time Series Seasonal")) + GRAPH$CAPTION + 
	geom_line(aes(color = TIME$COLORsets[[COL]]), show.legend = FALSE) + 
	# stat_smooth(na.rm = TRUE) + TEXT + 
	scale_x_continuous(
		name	=	"Approximate Time (seconds)",
		expand	=	c(0.02, 0),
		breaks	=	seq(0,	max(TS.df$Index),	by = max(TS.df$Index)/6),
		labels	=	seq(0,	DATA$duration,		by = DATA$duration/6),
		limits	=	c(0, NA)
		) + 
	scale_y_continuous(name = TIME$NAMEsets[[COL]],	breaks = TIME$BREAKsets[[COL]]) + 
	theme(plot.title.position = "plot") + GRAPH$COLORS
}



observeEvent(list(input$dataSelLOAD, DATA$LOAD) ,{
	req(DATA$dataALL)
	
	TS.df	<-	TIME$DF("CPU_Temp")
	output$timeTEMPtrend	=	renderPlot({	TIME$timeTREND("CPU_Temp", TS.df = TS.df)	})
	output$timeTEMPseas		=	renderPlot({	TIME$timeSEAS("CPU_Temp", TS.df = TS.df)	})
	
	TS.df	<-	TIME$DF("Frequency")
	output$timeFREQtrend	=	renderPlot({	TIME$timeTREND("Frequency", TS.df = TS.df)	})
	output$timeFREQseas		=	renderPlot({	TIME$timeSEAS("Frequency", TS.df = TS.df)	})
	
	TS.df	<-	TIME$DF("Socket_Energy")
	output$timeSOCKtrend	=	renderPlot({	TIME$timeTREND("Socket_Energy", TS.df = TS.df)	})
	output$timeSOCKseas		=	renderPlot({	TIME$timeSEAS("Socket_Energy", TS.df = TS.df)	})
	
	TS.df	<-	TIME$DF("Core_Energy")
	output$timeCOREtrend	=	renderPlot({	TIME$timeTREND("Core_Energy", TS.df = TS.df)	})
	output$timeCOREseas		=	renderPlot({	TIME$timeSEAS("Core_Energy", TS.df = TS.df)		})
	
})

# if (BRUSH) source("app_time_series_zoom.r", local = TRUE)