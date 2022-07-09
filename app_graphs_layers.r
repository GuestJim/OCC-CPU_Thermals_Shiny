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
			aes(y	=	Frequency / COEF,	color	=	"Frequency"),
			alpha	=	ALPHA,
			stat	=	"summary",
			fun		=	mean,
			show.legend	=	TRUE
		)	)
	}
	if (MAX)	{	return(
		geom_point(
			# data	=	IN,
			aes(y	=	Frequency / COEF,	color	=	"Frequency"),
			alpha	=	ALPHA,
			stat	=	"summary",
			fun		=	max,
			show.legend	=	TRUE
		)	)
	}
	return(	geom_point(
				# data	=	IN,
				aes(y	=	Frequency / COEF,	color	=	"Frequency"),
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
				labels	=	function(IN)	round(IN * COEF, 2)
				)
			),
		GRAPH$COLORS
	)
}