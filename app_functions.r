sepCOL	=	function(aggOUT)	{
	matCOL	=	!sapply(aggOUT, is.factor)
	out		=	aggOUT[, !matCOL]
	if (!(is.matrix(aggOUT[, matCOL]) | is.data.frame(aggOUT[, matCOL])))	return(aggOUT)

	for (FUN in which(matCOL))	{
		numCOLs			=	as.data.frame(aggOUT[, FUN])
		colnames(numCOLs)	=	paste(colnames(aggOUT)[FUN], colnames(numCOLs), sep = " - ")

		out	=	cbind(out, numCOLs)
	}

	return(out)
}

remNA	=	function(IN)	IN[!is.na(IN)]

cutWithin	=	function(IN, LIMS, ..., INCLUDE = TRUE)	!is.na(cut(IN,	LIMS,	labels = FALSE,	include.lowest = INCLUDE, ...))

stats	=	function(DATA)	{
	return(c(
		Min		=	min(DATA),
		Median	=	median(DATA),
		Mean	=	mean(DATA),
		Max		=	max(DATA)
	)	)
}

PERC	=	function(IN, listPERC = NULL)	{
	if (is.null(listPERC))	return(NULL)
	listPERC	=	c(listPERC, 0)
	#	this is to trick aggregate to always output a matrix

	listPERC	=	unique(sort(listPERC, decreasing = FALSE))
	if (any(listPERC >= 100))	listPERC	<-	listPERC[-which(listPERC >= 100)]
	if (any(listPERC < 0))		listPERC	<-	listPERC[-which(listPERC < 0)]

	setNames(quantile(IN, listPERC/100), paste0(listPERC, "%"))
}

ECDF	=	function(IN, listECDF = NULL)	{
	if (is.null(listECDF))	return(NULL)
	listECDF	=	c(listECDF, 0)
	#	this is to trick aggregate to always output a matrix
	listECDF	=	unique(sort(listECDF, decreasing = FALSE))

	setNames(ecdf(IN)(listECDF) * 100, paste0("<", listECDF))
}

tablePERC	<-	function(IN, COL, listPERC, engPOW = "W", valOFF = 0)	{
	if (any(is.null(listPERC), listPERC == 100, listPERC == 0, listPERC == ""))	return(NULL) 
	
	INdata	<-	IN[[COL]]
	if (grepl("Energy", COL)	&	engPOW == "W")	{
		INdata	<-	INdata/1000
		COL		<-	gsub("Energy",	"Power",	COL)
	}
	if (grepl("Temp", COL))	INdata	<-	INdata - valOFF
	
	dataSUM	=	sepCOL(aggregate(INdata, DATA$GROUPS, PERC, listPERC))
	if (unique(dataSUM$Socket) == 0)	dataSUM$Socket	=	NULL
	names(dataSUM)		<-	gsub("x - ", "", names(dataSUM))
	names(dataSUM)[1]	<-	unitCOL(COL)

	dataSUM[, !grepl("^0%$", names(dataSUM))]
}
tableECDF	<-	function(IN, COL, listECDF, engPOW = "W", valOFF = 0)	{
	if (any(is.null(listECDF), listECDF == ""))	return(NULL) 
	
	INdata	<-	IN[[COL]]
		if (grepl("Energy", COL)	&	engPOW == "W")	{
		INdata	<-	INdata/1000
		COL		<-	gsub("Energy",	"Power",	COL)
	}
	if (grepl("Temp", COL))	INdata	<-	INdata - valOFF
	
	dataSUM	=	sepCOL(aggregate(INdata, DATA$GROUPS, ECDF, listECDF))
	if (unique(dataSUM$Socket) == 0)	dataSUM$Socket	=	NULL
	names(dataSUM)		<-	gsub("x - ", "", names(dataSUM))
	names(dataSUM)[1]	<-	unitCOL(COL)

	dataSUM[, !grepl("^<0$", names(dataSUM))]
}

rem_		=	function(INPUT)	gsub("_", " ", INPUT)
unitCOL		=	function(IN)	{
	levs	=	levels(IN)
	if	(is.character(IN))	levs	=	IN
	levs[grep("Temp",		levs)]	=	paste0(levs[grep("Temp",	levs)],	" (°C)")
	levs[grep("Frequency",	levs)]	=	paste0(levs[grep("Frequency",	levs)],	" (MHz)")
	levs[grep("Energy",		levs)]	=	paste0(levs[grep("Energy",		levs)],	" (mJ)")
	levs[grep("Power",		levs)]	=	paste0(levs[grep("Power",		levs)],	" (W)")

	return(rem_(levs))
}

nearCEIL	=	function(DATA, VAL)	ceiling(max(DATA, na.rm = TRUE) / VAL) * VAL
nearFLOOR	=	function(DATA, VAL)	floor(max(DATA, na.rm = TRUE) / VAL) * VAL

to.NUM	=	function(IN)	{
	if (IN == "")	return(NULL)
	IN	=	gsub("[MHz]", "", IN)
	IN	=	unlist(strsplit(IN, "[, ]"))
	out	=	as.numeric(IN)
	out	=	out[!is.na(out)]
	return(out)
}

mJ2W	=	function(IN){
	IN$Measurement	=	as.character(IN$Measurement)
	ENG	=	which(grepl("Energy", IN$Measurement))

	POW	=	IN[ENG, ]
	numCOL	=	sapply(POW, is.numeric)
	POW$Measurement	=	gsub("Energy", "Power", POW$Measurement)
	POW$Measurement	=	gsub("mJ", "W", POW$Measurement)

	POW	=	cbind(POW[, !numCOL], POW[, numCOL]/1000)
	out	=	rbind(IN, POW)

	out$Measurement	=	ordered(out$Measurement, levels = unique(out$Measurement))

	out[order(out$Period, out$Socket, out$Measurement), ]
}

tempCROSS	=	function(IN, PERIOD, LIM, OP = NULL, LIST = 10)	{
	COLS	=	c("Time", "CPU_Temp", "CPU_Temp_Diff")
	out		=	IN[IN$Thread == 0 & IN$Period == PERIOD, COLS]
	out$Time	=	as.integer(out$Time)
	if (PERIOD == "Cooldown")	out	=	cbind(out, "Cooldown Time" = as.integer(out$Time - DATA$duration))
	
	timeCOLs	=	grepl("Time", names(out))
	# out[, timeCOLs]	=	as.integer(out[, timeCOLs])
	
	# if (QUAN < 1)	LIM	=	quantile(out$CPU_Temp, QUAN)
	# if (QUAN > 1)	LIM	=	QUAN
	
	if (is.null(OP))	{
		if (PERIOD == DATA$TESTname)	OP	=	">="
		if (PERIOD == "Cooldown")		OP	=	"<="
	}
	
	if (OP == "<=")		out	<-	out[out$CPU_Temp <= LIM, ][1:LIST, ]
	if (OP == ">=")		out	<-	out[out$CPU_Temp >= LIM, ][1:LIST, ]
	
	names(out)[1:3]	=	c("Time", "Temperature", "Difference")
	out[!is.na(out$Time), c(which(timeCOLs), which(!timeCOLs))]
}