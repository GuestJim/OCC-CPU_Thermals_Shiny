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

## Modular-Rebuild branch notes:

This branch has a version of the scripts where I attempted and mostly succeeded in modularizing much of the code in alignment with what is described in the article [Modularizing Shiny ap code](https://shiny.rstudio.com/articles/modules.html).
Unfortunately that article has a number of deficiencies, at the time I am writing this at least.
Among these is a confusing use of object IDs.

At the core of much of the modularizing the article describes are the concept and use of ***namespaces*** that, in effect, allow for objects segregated from others.
This can allow one module to use one ID internally, but the module is called multiple times without confusion between values.

The confusion within the article is its use of `id` as the object name passed to the `NS` function that creates namespaces.
If you review the documentation for [`NS`](https://shiny.rstudio.com/reference/shiny/1.7.0/NS.html), you find it has an argument named `id`, but closer examination reveals this is not the argument the article refers to.
The article's `id` object is going to the `namespace` argument.

Putting this into code, the article is written like this, when verbose with the IDs and arguments:

```
function(id) OR moduleServer(id, ...
ns	<-	NS(namespace = id, id = NULL)
ns("OBJECT_ID")
```

To avoid this confusion within my code, I have replaced the `id` object name with `name`.

```
function(name) OR moduleServer(name, ...
ns	<-	NS(namespace = name, id = NULL)
ns("OBJECT_ID")
```

I should also note how Shiny appears to implement namespaces, as I do abuse this at times, having found such abuse my only means of achieving certain behaviors.

It appears namespaces work by adding a prefix to object IDs, so an object `ID` in namespace `NAME` has a true ID of `NAME-ID`, though there is likely more going on than that.
Objects are filtered by the presence of the prefix in order to segregate them from everything else, but I had instances when piercing the namespace proved the only solution to a problem.
To achieve this I simply constructed the true ID and would reference it, which is probably not proper, but it gets the job done when nothing else I tried would.

I have only attempted to pierce the namespaces for `input` and `output` objects, and calling them is achieved like this: `input$"NAME-ID"`.
The quotes around the true ID, as I am calling it, are necessary.

Another trick I use after discovering it works allows me to more efficiently (at least in the code) pass external information into the server module.

The way a server module is construction according to the earlier article is like this:

```
testServer	<-	function(name)	{moduleServer(name, function(input, output, session)	{
	MODULE CODE
})}
```

Within the article it is mention "Inside of the module function, it can use parameters from the outside function", where parameter is what I call an argument.
What is not stated but is true, and is what I am taking advantage of, is everything from the outer function is accessible, not just the arguments.

```
a	<-	1	;	b	<-	2
testServer	<-	function(name)	{
	A	<-	a	;	B	<-	b
	moduleServer(name, function(input, output, session)	{
		MODULE CODE
})}
```

In this crude example, both `a` and `b` will be accessible to the `moduleServer` via the `A` and `B` objects.
If you have something you consistently want available to the `moduleServer` but without having to always pass it an argument, this is a viable means of doing that.

Here is a slightly more advanced but still crude example of this:

```
numericInput("roundTerm",	value = 2,	step = 1)

testServer	<-	function(name)	{
	roundTerm	<-	reactive(input$roundTerm)
	moduleServer(name, function(input, output, session)	{
		TAB	<-	table_function(INPUTS)
		output$table	<-	renderTable(	TAB,	digits = reactive(roundTerm()))
})}
```

Each time that module is called, it will be passed `input$roundTerm` as a reactive with the ID `roundTerm`.
That new reactive can then be used to control the rounding of the table, and without having to constantly recalculate the table and without needing to place the call to the module within an `observeEvent`, or similar.
This is also achieved without `roundTerm` needing to be an argument for the module, because it simply knows to call that dynamic value itself.

Though this is not perfect and it has not enabled me to do everything I wish, it is still quite effectivein many places and has allowed me to simplify various things.