# OCC-CPU_Thermals_Shiny
The scripts behind the R-Shiny applet for processing CPU thermal performance data collected with https://github.com/GuestJim/OCC-CPU_Thermals

Link to ShinyApps.io applet:
*   https://guestjim.shinyapps.io/OCC-CPU_Thermals
    *   includes 32 datasets covering multiple hardware and software configurations

The Shiny package for R allows the creation of web applets so the processing can be interactive.
R packages involved:
    *   Shiny
    *   ggplot2
    *   multimodes

In this case the features include:
*	Tutorial video behind button
	*	video may be out of date but should cover the majority of features
*   Summary Statistics table
    *   statistics:
        *   mean
        *   median
        *   minimum
        *   maximum
    *   statistics grouped by period:
        *   Warm-up
        *   test
        *   Cooldown
    *   measurements:
        *	Temperature
	    *	Frequency
	    *	Socket Power
	    *	Core Power
    	*	Uncore Power
            *   hidden by default because this is a calculated value (Socket Power - sum(Core Power)) rather than a measured value
    *   Statistics, periods, and measurements can all be toggled on and off
    *   Order priority can be changed from Period first, Measurement second (the default) to Measurement first, Period second
    *   Median Warm-up period Temperature can be subtracted from other temperature values
        *   impersonation of "temperature difference over ambient," which is a statistic I find dubious for multiple reasons
*   Custom Statistics tables for all measurements
    *   custom percentiles can be given and values returned
    *   custom values can be given and corresponding percentiles returned
*   Temperature Crosses tables
    *   table indicating when the test period temperature went above a certain value
        *   default value is the lower quartile
    *   table indicating when the Cooldown period temperature went below a certain value
        *   default value is the upper quartile
    *   length of tables adjustable to allow inspection of stability
*   Course graphs, showing data over the length of the test
    *   Mean Frequency
        *   for multi-thread tests, as thread frequencies are near each other
    *   Maximum Frequency
        *   for single-thread tests, as only loaded thread should be boosted
    *   Per-Thread Frequency
        *   graph contains a faceted plot for each thread
    *   Per-Core Power
        *   graph contains a faceted plot for each core
    *   interactivity:
        *   zooming via mouse brush over the graph and numeric input system
            *   zooming is done on second graph
            *   Per-Thread and Per-Core zoomed graphs can have specific thread and core plots disabled
*   Histograms to show distribution of data
    *   measurements:
        *   Temperature
        *   Frequency
        *   Socket Power
        *   Core Power
        *   Uncore Power
    *   interactivity:
        *   graph adjustments:
            *   have their bin width and X-scale lower limit adjusted
            *   Frequency graph can also draw lines at provided values, potentially marking base and boost clocks
        *   modality:
            *   have the number of modes (peaks in the distribution) calculated based on bin width, lower limit, and upper limit
            *   have the values corresponding to the modes determined
        *   percentiles
            *   mouse brush and numeric inputs for selecting values
            *   table generated of percentiles for selected values, and difference between them
*   Time Series graphs (additive) for test period data
    *   Trend and Seasonal graphs for:
        *   Temperature
        *   Frequency
        *   Socket Power
        *   Core Power
    *   Seasonal graphs centered on median of appropriate measurement
    *   interactivity:
        *   mouse brush on Trend graph for minimum and maximum values selected
            *   sometimes the Y-scale is unclear
        *   mouse brush on Seasonal graph to create zoomed in graph
        *   button to apply Seasonal zoom for current graph to other Seasonal-zoom graphs

