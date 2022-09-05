GRAPH$graphHIST	=	function(TYPE, TITLE, X.name, X.break, X.limits, FILL.unit, FILL.mid, FILL.limits, FILL.breaks, binWID = 1, COEF = 1)	{
	ggplot(data = DATA$dataALL, aes(x = get(TYPE)*COEF)) +
	ggtitle(			TITLE,
		subtitle	=	"Histograms & Box Plots with Red Mean Line"	) + GRAPH$CAPTION +
	scale_fill_gradient2(FILL.unit, low="blue", mid = "green", midpoint = FILL.mid,  high="red", limits = FILL.limits, breaks = FILL.breaks) +
	theme(
		plot.title.position			=	"plot",
		legend.position				=	"bottom",
		legend.justification		=	"left",
		legend.margin				=	margin(t = -1, b = -2, l = -2, unit = "lines"),
		legend.key.width			=	unit(0.045, "npc")
		) +
	geom_boxplot(outlier.alpha = 0, 				coef = 0,	width = Inf,	position = position_nudge(y = 0.5)) +
	geom_histogram(aes(y = after_stat(ncount),	fill = after_stat(x)),	binwidth = binWID) +
	geom_boxplot(outlier.alpha = 0, alpha = 0.15,	coef = 0,	width = Inf,	position = position_nudge(y = 0.5)) +
	geom_vline(data = aggregate(DATA$dataALL[, TYPE], DATA$GROUPS, mean, na.rm = TRUE),	aes(xintercept = x*COEF), 	color = "red") +
	# facet_grid(rows = vars(Period), switch = "y", labeller = labeller(Period = label_wrap_gen(20))) +
	facet_grid(rows = vars(Period), switch = "y",
		labeller = labeller(Period = function(IN) gsub(" - ", "\n", IN))
		) +
	scale_x_continuous(
		name	=	X.name,
		breaks	=	seq(0, 10000, by = X.break),
		limits	=	X.limits,
		guide 	=	guide_axis(n.dodge = 2),
		expand	=	c(0.02, 0)
		) +
	scale_y_continuous(name = "", breaks = NULL)
}

FREQspec_line	=	function(FREQ	=	DATA$FREQspec)	{
	if	(!is.numeric(FREQ))	return(NULL)

	FREQdata	=	list(
		Period	=	ordered(DATA$levsPER[1], DATA$levsPER),	x	=	FREQ,	y	=	Inf,
		TEXT	=	FREQ,
		ECDF	=	paste0("\u2190 ", round(ecdf(DATA$dataALL[DATA$dataALL$Period == DATA$TESTname, ]$Frequency)(FREQ) * 100, 2), "%")
		#	\u2190 draws a left-pointing arrow
		)

	list(geom_vline(
			xintercept	=	FREQ,
			color		=	"black",
			linetype	=	"dashed"
		),
		geom_text(data	=	data.frame(FREQdata),
			aes(x = x,	y = y,	label = TEXT),
			vjust	=	-0.5
		),
		geom_text(data	=	data.frame(FREQdata),
			aes(x = x,	y = y,	label = ECDF),
			vjust	=	-2.0,	hjust	=	0.675
		),
		coord_cartesian(clip = "off")
	)
}

graphHISTServer	<-	function(id, TYPE, TITLE, X.name, X.break, FILL.unit, FILL.mid, FILL.limits, FILL.breaks, COEF = 1, LINES = FALSE)	{moduleServer(id, function(input, output, session)	{

		output$graphHIST	=	renderPlot({
			GRAPH$graphHIST(TYPE,	TITLE,	X.name,	X.break,	X.limits = c(input$MIN, NA),	FILL.unit,	FILL.mid,	FILL.limits,	FILL.breaks,	binWID = input$BIN,	COEF) +
			if (LINES)	{	FREQspec_line(	to.NUM(input$SPEC)	)	}
		})	%>% bindEvent(list(input$dataSelLOAD, input$UPD))
})}

graphHISTlineServer	<-	function(id, SPECdata = NULL, UNIT = NULL)	{moduleServer(id, function(input, output, session)	{
	if (exists("FREQspec", envir = DATA))	{
		updateTextInput(	inputId	=	'SPEC',	value	=	"")
		if (!is.null(SPECdata))	updateTextInput(	inputId	=	'SPEC',
			value	=	paste(paste0(SPECdata, " ", UNIT), collapse = ", ")	)
	}
})}

observeEvent(input$dataSelLOAD,	{
	graphHISTServer('TEMP',
		TYPE	=	"CPU_Temp",
		TITLE		=	"CPU Temperature Normalized Distribution by Period",
		X.name		=	"Temperature (°C)",
		X.break		=	5,
		FILL.unit	=	"°C",
		FILL.mid	=	60,
		FILL.limits	=	c(25, 95),
		FILL.breaks	=	seq(30, 90, by = 10)	)

	graphHISTlineServer('FREQ', DATA$FREQspec, "MHz")

	graphHISTServer('FREQ',
		TYPE		=	"Frequency",
		TITLE		=	"Frequency Normalized Distribution by Period",
		X.name		=	"Frequency (MHz)",
		X.break		=	200,
		FILL.unit	=	"MHz",
		FILL.mid	=	3000,
		FILL.limits	=	c(round(min(DATA$dataALL$Frequency)-500, -3), DATA$maxCLK),
		FILL.breaks	=	seq(0, 10000, by = 500),
		LINES	=	TRUE)

	graphHISTServer('SOCK',
		TYPE		=	"Socket_Energy",
		TITLE		=	"Socket Power Normalized Distribution by Period",
		X.name		=	"Power (W)",
		X.break		=	10,
		FILL.unit	=	"W",
		FILL.mid	=	80,
		FILL.limits	=	c(0, nearCEIL(DATA$maxPWR/1000 + 1, 30)),
		FILL.breaks	=	seq(0, nearCEIL(DATA$maxPWR/1000 + 1, 30), by = 30),
		COEF		=	1/1000	)

	graphHISTServer('CORE',
		TYPE		=	"Core_Energy",
		TITLE		=	"Core Power Normalized Distribution by Period",
		X.name		=	"Power (W)",
		X.break		=	1,
		FILL.unit	=	"W",
		FILL.mid	=	3,
		FILL.limits	=	c(0, nearCEIL(DATA$dataALL$Core_Energy/1000, 3)),
		FILL.breaks	=	seq(0, nearCEIL(DATA$dataALL$Core_Energy/1000, 5), by = 3),
		COEF		=	1/1000	)

	graphHISTServer('UNCORE',
		TYPE		=	"Uncore_Energy",
		TITLE		=	"Uncore Power Normalized Distribution by Period",
		X.name		=	"Power (W)",
		X.break		=	5,
		FILL.unit	=	"W",
		FILL.mid	=	30,
		FILL.limits	=	c(0, nearCEIL(DATA$dataALL$Uncore_Energy/1000, 5)),
		FILL.breaks	=	seq(0, nearCEIL(DATA$dataALL$Uncore_Energy/1000, 5), by = 15),
		COEF		=	1/1000	)
})

# if (VIEW$BRUSH)	source("app_histograms_brush.r", local = TRUE)
if (VIEW$MODES)	source("app_histograms_modes.r", local = TRUE)