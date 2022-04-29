if (!require(ggplot2))	install.packages('ggplot2')
library(ggplot2)

GRAPH$labelBreak	=	function(breaks, SEC = FALSE, r = 2)	{
	if (!app.BREAK)	return(breaks)
	if (is.numeric(breaks))	breaks	=	round(breaks, r)
	BREAK	=	c("", "\n")
	if	(is.numeric(breaks)	&	0 %in% breaks)	if	((which(breaks %in% 0) %% 2) == 0)	BREAK	=	rev(BREAK)
	if	(!SEC)	return(	paste0(rep(BREAK, length.out = length(breaks)),	breaks)	)
	if	(SEC)	return(	paste0(breaks, rep(BREAK, length.out = length(breaks)))	)
}

GRAPH$TEMP_point	=	function(COEF = 1)	{
	geom_point(
		# data	=	IN,
		aes(y	=	CPU_Temp*COEF, 			color	=	"Temperature"),
		stat 	=	"unique",
		# color	=	"red",
		shape 	=	3,
		show.legend	=	TRUE
	)
}
GRAPH$SOCK_point	=	function(COEF = 1/1000)	{
	geom_point(
		# data	=	IN,
		aes(y	=	Socket_Energy*COEF,	color	=	"Socket Power"),
		stat	=	"unique",
		# color	=	"green",
		shape 	=	3,
		show.legend	=	TRUE
	)
}
GRAPH$CORE_point	=	function(COEF = 1/1000)	{
	geom_point(
		# data	=	IN,
		aes(y	=	Core_Energy*COEF,	color	=	"Core Power"),
		stat	=	"unique",
		# color	=	"green",
		show.legend	=	TRUE
	)
}
GRAPH$unCORE_point	=	function(COEF = 1/1000)	{
	if (all(DATA$dataALL$Uncore_Energy == 0))	return(NULL)
	#	cannot get an Uncore from Intel CPUs
	geom_point(
		# data	=	IN,
		aes(y	=	Uncore_Energy*COEF,	color	=	"Uncore Power"),
		# color	=	"green",
		shape	=	18,
		show.legend	=	TRUE
	)
}
GRAPH$FREQ_point	=	function(COEF = FREQ.COEF, MEAN = FALSE, MAX = FALSE, ALPHA = .20)	{
	if (MEAN)	{	return(
		geom_point(
			# data	=	IN,
			aes(y	=	Frequency*COEF,	color	=	"Frequency"),
			alpha	=	ALPHA,
			stat	=	"summary",
			fun		=	mean,
			show.legend	=	TRUE
		)	)
	}
	if (MAX)	{	return(
		geom_point(
			# data	=	IN,
			aes(y	=	Frequency*COEF,	color	=	"Frequency"),
			alpha	=	ALPHA,
			stat	=	"summary",
			fun		=	max,
			show.legend	=	TRUE
		)	)
	}
	return(	geom_point(
				# data	=	IN,
				aes(y	=	Frequency*COEF,	color	=	"Frequency"),
				alpha	=	ALPHA,
				# color	=	"blue",
				show.legend	=	TRUE
		)	)
}

GRAPH$COLORS	=	scale_color_manual(
		name	=	NULL,
		values	=	c(
			Temperature		=	"red",
			Frequency		=	"blue",
			"Core Power"	=	"green",
			"Socket Power"	=	"darkgreen",
			"Uncore Power"	=	"yellowgreen")
	)

GRAPH$themeSCALES	=	function(COEF = FREQ.COEF){
	list(
		theme(
			plot.title.position		=	"plot",
			legend.position			=	"top",
			legend.justification	=	"left",
			legend.margin			=	margin(t = 0, unit = "cm")
			),
		scale_x_continuous(
			name	=	"Time (seconds)",
			# breaks	=	unique(c(seq(0, warm, by = warm/3), seq(warm, 2 * duration + warm, by = duration/6))),
			breaks	=	unique(c(seq(-DATA$warm, 0, by = DATA$warm/3), seq(0, 2 * DATA$duration, by = DATA$duration/6))),
			# labels	=	function(x)	labelBreak(x - warm),
			labels	=	GRAPH$labelBreak,
			minor_breaks	=	NULL,
			expand	=	c(0.02, 0),
			sec.axis	=	dup_axis(
				name	=	NULL,
				# breaks	=	c(warm, duration + warm),
				breaks	=	c(0, DATA$duration),
				labels	=	c("Load Start", "Load Stop/End")
			)
		),
		scale_y_continuous(
			breaks		=	seq(0, DATA$maxPWR/100, by = 10),
			limits		=	c(0, NA),
			expand		=	c(0.02, 0),
			sec.axis	=	dup_axis(
				name	=	"Frequency (MHz)",
				labels	=	function(IN)	round(IN / COEF, 2)
				)
			),
		GRAPH$COLORS
	)
}

GRAPH$graphMEAN	=	function(COEF = DATA$FREQ.COEF, FILT = TRUE)	{
	ggplot(data = DATA$dataALL[FILT, ], aes(x=Time)) +
	ggtitle("Mean Frequency with Temperature and Power") + GRAPH$CAPTION +
	GRAPH$TEMP_point() +
	GRAPH$SOCK_point() +
	GRAPH$FREQ_point(COEF = COEF, MEAN = TRUE) +
	GRAPH$themeSCALES(COEF) + ylab("Temperature (°C) and Power (W)")
}

GRAPH$graphMAX	=	function(COEF = DATA$FREQ.COEF, FILT = TRUE)	{
	ggplot(data = DATA$dataALL[FILT, ], aes(x=Time)) +
	ggtitle("Max Frequency with Temperature and Power") + GRAPH$CAPTION +
	GRAPH$TEMP_point() +
	GRAPH$SOCK_point() +
	GRAPH$FREQ_point(COEF = COEF, MAX = TRUE) +
	GRAPH$themeSCALES(COEF) + ylab("Temperature (°C) and Power (W)")
}

GRAPH$graphFREQ	=	function(COEF = FREQ.COEF, FILT = TRUE)	{
	ggplot(data = DATA$dataALL[FILT, ], aes(x=Time)) +
	ggtitle("Frequency with Temperature and Core Power",
		subtitle = "Even Thread: Physical, Odd Thread: Logical") + GRAPH$CAPTION +
	GRAPH$TEMP_point() +
	# GRAPH$SOCK_point() +
	GRAPH$CORE_point() +
	GRAPH$FREQ_point(COEF = COEF) +
	GRAPH$themeSCALES(COEF) + ylab("Temperature (°C) and Power (W)") + 
	NULL
	# facet_grid(rows = vars(Core, Thread),	switch = "y", labeller	=
		# labeller(Core	=	function(IN) paste0("Core: ", IN), Thread	=	function(IN) paste0("Thread: ", IN))
		# )
}
GRAPH$facetFREQ	=	facet_grid(rows = vars(Core, Thread),	switch = "y", labeller	=
		labeller(Core	=	function(IN) paste0("Core: ", IN), Thread	=	function(IN) paste0("Thread: ", IN))
		)

GRAPH$graphPOWER	=	function(COEF = FREQ.COEF, FILT = TRUE)	{
	ggplot(data = DATA$dataALL[FILT, ], aes(x=Time)) +
	ggtitle("Frequency with Core and Uncore Power",	subtitle = "") + GRAPH$CAPTION +
	GRAPH$FREQ_point(COEF = COEF) +
	GRAPH$unCORE_point() +
	GRAPH$CORE_point() +
	GRAPH$themeSCALES(COEF) + ylab("Power (W)") + expand_limits(y = c(0, 90)) +
	NULL
	# facet_grid(rows = vars(Core),	switch = "y", labeller	=
		# labeller(Core	=	function(IN) paste0("Core: ", IN))
		# )
}
GRAPH$facetPOWER	=	facet_grid(rows = vars(Core),	switch = "y", labeller	=
		labeller(Core	=	function(IN) paste0("Core: ", IN))
		)

observeEvent(list(input$dataSelLOAD, DATA$LOAD, input$FREQ.COEF) ,{
	output$graphMEAN	=	renderPlot({
		req(DATA$dataALL)
		GRAPH$graphMEAN(input$FREQ.COEF)
	})
	output$graphMAX	=	renderPlot({
		req(DATA$dataALL)
		GRAPH$graphMAX(input$FREQ.COEF)
	})
	output$graphFREQ	=	renderPlot({
		req(DATA$dataALL)
		GRAPH$graphFREQ(input$FREQ.COEF) + GRAPH$facetFREQ
	},	height = 720/3 * length(unique(DATA$dataALL$Thread))	)
	output$graphPOWER	=	renderPlot({
		req(DATA$dataALL)
		GRAPH$graphPOWER(input$FREQ.COEF) + GRAPH$facetPOWER
	},	height = 720/3 * length(unique(DATA$dataALL$Core))	)
})

if (VIEW$BRUSH)	source("app_graphs_zoom.r", local = TRUE)