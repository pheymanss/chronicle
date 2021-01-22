{chronicle}
================

This is an R package for rendering attractive R Markdown html files,
with interactive html widgets like [plotly](https://plotly.com/r/)
charts, [dygraphs](https://rstudio.github.io/dygraphs/) and
[DataTables](https://rstudio.github.io/DT/). It is built using a layered
paradigm that will be familiar to any {ggplot2} or {tensorflow} R user.

A quick demo:

``` r
install.packages('chronicle')
library(chronicle)

new_report(title = '\{chronicle\} demo') %>%
    add_title('This is how a chronicle report looks', title_level = 1) %>%
    add_density(dt = iris, groups = 'Species', value = 'Sepal.Length', faceted = F) %>%
    add_table(table = iris, table_title = 'This is the iris dataset. Smells good!', html_table_type = 'DT') %>%
    add_boxplot(dt = iris, groups = 'Species', value = 'Sepal.Length') %>%
    add_barplot(dt = iris, bars = 'Species', value = 'Sepal.Length') %>%
    render_report(filename = 'index', keep_rmd = TRUE)
```

And you can see the ouput [here](https://pheymanss.github.io/).

#### How to use chronicle?

Similar to starting off with the ggplot() function to create an empty
plot, you start off with new\_report(), which creates an empty R
Markdown header with a few presets to make the html output useful and
appealing (mostly through
[prettydoc](https://github.com/yixuan/prettydoc).) You can print your
current formatted R Markdown structure in the R console using the cat()
function.

Additional to the empty header, new\_report() creates a first chunk in
which it loads the package, and suggests adding all the preprocessing in
that chunk to have reproducibility by rendering through direct knitting
instead of relying on chronicle's rendering.

``` r
empty_header <- new_report()
cat(empty_header)
```

    ## ---
    ## title:  New chronicle Report
    ## date: "`r Sys.Date()`"
    ## author:  chronicle user
    ## output: 
    ##   prettydoc::html_pretty: 
    ##     theme: leonids
    ##     fig_width: 11
    ##     fig_height: 5
    ## ---
    ## 
    ## 
    ## ```{r, echo = FALSE, message = FALSE, warning = FALSE}
    ## library(chronicle)
    ## # If you want this report to be reproducible, add on this chunk all
    ## # the libraries, data loading and preprocessing done before executing
    ## # the chronicle report.
    ## 
    ## ```

After this, you can then just add new chunks with the add\_\* family of
functions. These include:

-   add\_barplot
-   add\_boxplot
-   add\_chunk
-   add\_code
-   add\_density
-   add\_dygraph
-   add\_histogram
-   add\_lineplot
-   add\_table
-   add\_text
-   add\_title
-   add\_violin

Each of these functions has its own set of parameters, and aim to take
the burden of sophistication from the user, to help you create
interactive data visualizations by just specifying the parameters of
each function. Be sure to go through ?add\_\* to see the full set of
options each function has.

These functions will create chunks that will be concatenated into your
report, similar to adding layers to a ggplot.

After having all your chunks, just call render\_report() to knit the
html file. A great upside of using chronicle is that **the report is
rendered in you global environment**, which means that you will not have
to repeat all the processing as you would have if you knitted
traditionally. You can also choose to keep the .Rmd file for later
reproducibility.

``` r
finished_report <- new_report()
render_report(report = finished_report, filename = "my_report", keep_rmd = TRUE)
```

#### What's next for chronicle?

-   Add other output types besides prettydoc: bookdown, pagedown,
    flexdashboard (!), blogdown and github .rd (would have been useful, right)
-   Add new add\_\* functions and additional parameters to the existing
    ones.
-   Tryout other plotting engines besides plotly: ggiraph, e\_charts,
    highcharter\*, D3 (!).
