{chronicle}
================
2021-03-03

## An R package for easy R Markdown reporting

``` r
install.packages('chronicle')
```

This R package allows the user to create beautiful R Markdown reports in
a wide gamut of outputs, without the need to be exposed to the code
necessary to create each of its elements. chronicle is built on a
layered paradigm, which will be familiar to any ggplot user.

### A quick demo

You can build R Markdown reports through the `add_*` family of
functions, layering one below the previous one.

``` r
library(chronicle)

demo_report <-
  add_text(text_title = "This is the output of a chronicle call",
           text = "Each element has been added through and add_* function.",
           title_level = 1) %>%
  add_table(table = head(iris),
            table_title = "A glimpse at the iris dataset",
            html_table_type = "kable",
            title_level = 1) %>%
  add_raincloud(dt = iris,
                value = "Sepal.Length",
                groups = "Species",
                raincloud_title = "Distribution of sepal length by species",
                title_level = 2) %>%
  add_scatterplot(dt = iris,
                  x = "Petal.Width",
                  y = "Petal.Length",
                  groups = "Species",
                  scatterplot_title = "Comparison of petal width and length",
                  title_level = 2)

render_report(report = demo_report,
              output_format = "rmdformats",
              filename = "quick_demo",
              title = "A quick chronicle demo",
              author = "You did this!",
              keep_rmd = TRUE)
```

You can see the output of this call
[here](https://pheymanss.github.io/chronicle-demos/quick_demo), and a
full showcase of the elements supported by chronicle
[here](https://pheymanss.github.io/chronicle-demos/showcase).

What happens behind these calls is that chronicle writes an R Markdown
for you! you can see the report we’ve built by calling it through
`cat()`

``` r
cat(demo_report)
```

    
    
    # This is the output of a chronicle call
    
    Each element has been added through and add_* function.
    
    # A glimpse at the iris dataset
    
    ```{r, echo = FALSE, message = FALSE, warning = FALSE}
    knitr::kable(head(iris))
    ```
    
    ## Distribution of sepal length by species
    ```{r, echo=FALSE, message=FALSE, warning=FALSE, fig.width=figure_width, fig.height=figure_height}
    make_raincloud(dt = iris, value = 'Sepal.Length',
      groups = 'Species',
      adjust = '0.5',
      include_boxplot = 'TRUE',
      include_mean = 'FALSE',
      include_median = 'TRUE',
      force_all_jitter_obs = 'FALSE',
      ggtheme = 'minimal',
      plot_palette_generator = 'plasma', static = set_static)
    ```
    
    ## Comparison of petal width and length
    ```{r, echo=FALSE, message=FALSE, warning=FALSE, fig.width=figure_width, fig.height=figure_height}
    make_scatterplot(dt = iris, x = 'Petal.Width',
      y = 'Petal.Length',
      groups = 'Species', static = set_static)
    ```

### The `make_*` family of functions

Every plot added with an `add_*` function will be built through its
correpsonding `make_*` function. These functions take care of the heavy
lifting, avoiding the cumbersome (albeit powerful) sintax of ggplot,
plotly and other html widgets. The parameters of the make\_functions are
simple and intuitive specifications on how to make each plot, and they
can be called independently and used in any instance where a ggplot or
an html widget would fit.

``` r
make_barplot(dt = ggplot2::mpg,
             value = 'cty',
             bars = 'manufacturer',
             break_bars_by = 'drv',
             horizontal = TRUE,
             sort_by_value = TRUE,
             static = TRUE)
```

``` r
make_raincloud(dt = iris,
             value = 'Sepal.Length',
             groups = 'Species')
```

### Rendering chronicle reports

Once the structure of the report has been defined, the rendering process
is done by render\_report(). This uses rmarkdonw::render() as a backend
for rendering the report, which gives chronicle the capability to render
the reports with full visibility to all objects in the global
environment. This gives chronicle two of its main strengths:

1.  You don’t need to include nor run all your data processing code
    again for a new report output. This means you can build several
    report recipes for different audiences out of the same data
    processing, with each one having their own report recipe.

2.  It can render *several* output formats in a single call. For
    instance, it is possible to render the same content as
    [ioslides](https://garrettgman.github.io/rmarkdown/ioslides_presentation_format.html)
    for a presentation, as
    [tufte\_html](https://rstudio.github.io/tufte/) for handouts and as
    [rmdformats](https://github.com/juba/rmdformats) for a site upload.

Take our quick demo as an example, to render this as the three outputs
mentioned previously, you only need to add that vector to the
`output_format` parameter of `render_report()`

``` r
render_report(report = demo_report,
              output_format = c("ioslides", "tufte_html", "rmdformats"),
              filename = "quick_demo",
              title = "A quick chronicle demo",
              author = "You did this!",
              keep_rmd = TRUE)
```

### The `report_columns()` function

chronicle also includes a function called report\_columns(), that will
create an entire chronicle report for a single dataset. It includes a
comprehensive summary of the data through the skimr::skim() function,
along with one plot for each column present in the data: bar plots for
categorical variables and rain cloud plots for numerical variables. This
gives you an immediate view of a dataset with a single line of code!

``` r
report_columns(dt = palmerpenguins::penguins,
               by_column = 'species')
```

you can see the example of this output
[here](https://pheymanss.github.io/chronicle-demos/report_columns)
