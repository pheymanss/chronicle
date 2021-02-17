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

demo_report <- add_title(title = 'This is how a chronicle report looks', title_level = 1) %>%
    add_density(dt = iris, groups = 'Species', value = 'Sepal.Length', faceted = F) %>%
    add_table(table = iris, table_title = 'This is the iris dataset. Smells good!', html_table_type = 'DT') %>%
    add_boxplot(dt = iris, groups = 'Species', value = 'Sepal.Length') %>%
    add_barplot(dt = iris, bars = 'Species', value = 'Sepal.Length')
    
render_report(report = demo_report,
              filename = 'chronicle_demo',
              output_format = c('prettydoc', 'ioslides'),
              title = 'Demo Output',
              author = 'You created this',
              keep_rmd = TRUE)
```

And you can see the ouput of the prettydoc format [here](https://pheymanss.github.io/chronicle-demos/).

#### How to use chronicle?

As R Markdown files are a collection of chunks, you can start off a report just by calling any of the add\_\* family of
functions to add a chunk with a specific type of content. These currently include:

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
-   add\_scatterplot
-   add\_violin

Each of these functions has its own set of parameters, and aim to take
the burden of sophistication from the user, to help you create
interactive data visualizations by just specifying the parameters of
each function. Be sure to go through ?add\_\* to see the full set of
options each function has.

These functions will create chunks that will be concatenated into your
report, similar to adding layers to a ggplot.

After having all your chunks, just call render\_report() to knit the
html file. This function will create the yaml header for all the output
formats you selected, and render all of them in a single call. Albeit 
quite unformed yet, chronicle currently supports 'prettydoc', 'ioslides',
'tufte', 'flexdashboard', 'slidy_presentation', 'html_document' and
'html_notebook', althouhg I heavily suggest sticking to prettydoc and 
ioslides for now.

A great upside of using chronicle is that **the report is
rendered in you global environment**, which means that you will not have
to repeat all the data processing as you would have if you knitted
traditionally. You can also choose to keep the .Rmd file for later
reproducibility. As shown in the demo, this is the call that renders 
the R Markdown file into a prettydoc *and* an ioslides output:

``` r
render_report(report <- demo_report
              filename = 'chronicle_demo',
              output_format = c('prettydoc', 'ioslides')
              title = 'Demo Output',
              author = 'You created this',
              keep_rmd = TRUE)
```

#### What's next for chronicle?

-   Add other interactive output formats (mainly bookdown), and 
    polish the outputs of the ones currently supported.
-   Add non-interactive outputs (where you cannot embed html objects, 
    but it can have plots and tables as static figures) like word,
    powerpoint, pagedown, and git readme.
-   Add new add\_\* functions, and additional parameters to the existing
    ones.
-   Try out other plotting engines besides plotly: ggiraph, e\_charts,
    highcharter\*, D3 (!).
