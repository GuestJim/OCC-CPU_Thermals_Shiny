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

tsFrame	<-	function(TYPE, DELT = DATA$duration)	{
	unitADJ	=	1
	if (grepl("Energy", TYPE))	unitADJ	=	1000
	TS	=	decompose(ts(DATA$dataALL[DATA$dataALL$Period == DATA$TESTname, TYPE] / unitADJ, deltat = 1/DELT, start = 0), type = "additive")
		#	start = 0 to align with Time labels
	out.df	=	data.frame(as.vector(time(TS$trend)), as.vector(TS$trend), as.vector(TS$seasonal))
	colnames(out.df)	=	c("Index", "Trend", "Seasonal")
	out.df
}

TS.list	<-	eventReactive(input$dataSelLOAD,	list(
	TEMP	=	tsFrame("CPU_Temp"),
	FREQ	=	tsFrame("Frequency"),
	SOCK	=	tsFrame("Socket_Energy"),
	CORE	=	tsFrame("Core_Energy")
	)	)

graphTS	<-	function(TYPE, TS.df)	{
	ggplot(data = TS.df, aes(x = Index)) +
	GRAPH$CAPTION +
	scale_x_continuous(
		name	=	"Approximate Time (seconds)",
		expand	=	c(0.02, 0),
		breaks	=	seq(0,	max(TS.df$Index),	by = max(TS.df$Index)/6),
		labels	=	seq(0,	DATA$duration,		by = DATA$duration/6),
		limits	=	c(0, NA)
		) +
	scale_y_continuous(name = TIME$NAMEsets[[TYPE]],	breaks = TIME$BREAKsets[[TYPE]]) +
	theme(plot.title.position = "plot") + GRAPH$COLORS
}

graphTREND	<-	function(TYPE, TS.df)	{
	TEXT	=	NULL
	
	graphTS(TYPE, TS.df) + 
	ggtitle(paste0(rem_(TYPE), " - Time Series Trend")) +
	geom_line(aes(y = Trend, color = TIME$COLORsets[[TYPE]]), show.legend = FALSE) +
	stat_smooth(aes(y = Trend), na.rm = TRUE) + TEXT
}
graphSEAS	<-	function(TYPE, TS.df)	{
	graphTS(TYPE, TS.df) + 
	ggtitle(paste0(rem_(TYPE), " - Time Series Seasonal")) +
	geom_line(aes(y = Seasonal + median(Trend, na.rm = TRUE), color = TIME$COLORsets[[TYPE]]), show.legend = FALSE)
}

TSgraphServer	<-	function(id, TYPE, TS.df, DELT = DATA$duration)	{moduleServer(id, function(input, output, session)	{
	output$TStrend	<-	renderPlot(graphTREND(TYPE, TS.df))
	output$TSseas	<-	renderPlot(graphSEAS(TYPE, TS.df))
})}

observeEvent(input$dataSelLOAD,	{
	TSgraphServer('TEMP',	"CPU_Temp",			TS.list()$TEMP)
	TSgraphServer('FREQ',	"Frequency",		TS.list()$FREQ)
	TSgraphServer('SOCK',	"Socket_Energy",	TS.list()$SOCK)
	TSgraphServer('CORE',	"Core_Energy",		TS.list()$CORE)
})

if (VIEW$BRUSH) source("app_time_series_zoom.r", local = TRUE)