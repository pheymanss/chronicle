tictoc::tic()
library(chronicle)


# intro -------------------------------------------------------------------
add_title(title = 'An R package for easy R Markdown reporting',
          title_level = 2) %>%

  add_code(code = "install.packages('chronicle')", eval = FALSE) %>%

  add_text(text = 'This R package allows the user to create beautiful R Markdown
reports in a wide gamut of outputs, without the need to be exposed to the code
necessary to create each of its elements. chronicle is built on a layered
paradigm, which will be familiar to any ggplot user.') %>%

  # quick demo --------------------------------------------------------------
add_title(title = 'A quick demo', title_level = 3) %>%
  add_text(text = 'You can build R Markdown reports through the `add_*` family
           of functions, layering one below the previous one.') %>%
  add_code(code = '
library(chronicle)

demo_report <-
  add_text(text_title = "This is the output of a chronicle call",
           text = "Each element has been added through an add_* function.",
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
              keep_rmd = TRUE)', eval = TRUE) %>%
  add_code(code = "file.remove('quick_demo.Rmd'); file.remove('quick_demo.html')", echo = FALSE, eval = TRUE) %>%
  add_text(text = "You can see
[the output of this call](https://pheymanss.github.io/chronicle-demos/quick_demo), and
[a full showcase of all the elements supported by chronicle](https://pheymanss.github.io/chronicle-demos/showcase).

What happens behind these calls is that chronicle writes an R Markdown for you!
you can see the report we've built by calling it through `cat()`") %>%
  add_code(code = 'cat(demo_report)', eval = TRUE) %>%
  # make_ -------------------------------------------------------------------
add_text(text_title = "The `make_*` family of functions", title_level = 3,
text = "Every plot added with an `add_*` function will be built through its
correpsonding `make_*` function. These functions take care of the heavy lifting,
avoiding the cumbersome (albeit powerful) sintax of ggplot, plotly and other
html widgets. The parameters of the make_functions are simple and intuitive
specifications on how to make each plot, and they can be called independently
and used in any instance where a ggplot or an html widget would fit.") %>%
add_code(code = "make_barplot(dt = ggplot2::mpg,
             value = 'cty',
             bars = 'manufacturer',
             break_bars_by = 'drv',
             horizontal = TRUE,
             sort_by_value = TRUE,
             static = TRUE)",
         eval = FALSE, echo = TRUE) %>%
  add_text(text = '![chronicle bar plot](https://raw.githubusercontent.com/pheymanss/chronicle/master/readme1.png)') %>%
  add_code(code =
             "make_raincloud(dt = iris,
             value = 'Sepal.Length',
             groups = 'Species')",
           eval = FALSE, echo = TRUE) %>%
  add_text(text = '![chronicle rain cloud plot](https://raw.githubusercontent.com/pheymanss/chronicle/master/readme2.png)') %>%
# rendering ---------------------------------------------------------------
add_text(text_title = 'Rendering chronicle reports', title_level = 3, text = "
Once the structure of the report has been defined, the rendering process is done
by render_report(). This uses rmarkdonw::render() as a backend for rendering the
report, which gives chronicle the capability to render the reports with full
visibility to all objects in the global environment. This gives chronicle two of
its main strengths:

1. You don't need to include nor run all your data processing code again for a
new report output. This means you can build several report recipes for different
audiences out of the same data processing, with each one having their own report
recipe.

2. It can render *several* output formats in a single call. For instance, it is
possible to render the same content as
[ioslides](https://garrettgman.github.io/rmarkdown/ioslides_presentation_format.html)
for a presentation, as [tufte_html](https://rstudio.github.io/tufte/) for handouts
and as [rmdformats](https://github.com/juba/rmdformats) for a site upload.
") %>%
  add_text("Take our quick demo as an example, to render this as the three outputs
mentioned previously, you only need to add that vector to the `output_format`
parameter of `render_report()`") %>%
  add_code(code = '
render_report(report = demo_report,
              output_format = c("ioslides", "tufte_html", "rmdformats"),
              filename = "quick_demo",
              title = "A quick chronicle demo",
              author = "You did this!",
              keep_rmd = TRUE)', eval = FALSE) %>%

# report_columns ----------------------------------------------------------
add_text(text = 'chronicle also includes a function called report_columns(),
that will create an entire chronicle report for a single dataset. It includes a
comprehensive summary of the data through the skimr::skim() function, along with
one plot for each column present in the data: bar plots for categorical variables
and rain cloud plots for numerical variables. This gives you an immediate view of
a dataset with a single line of code!',
text_title = 'The `report_columns()` function', title_level = 3) %>%
  add_code(code = "report_columns(dt = palmerpenguins::penguins,
               by_column = 'species')", echo = TRUE, eval = FALSE) %>%
  add_text(text = 'And you can see
[the output of this call](https://pheymanss.github.io/chronicle-demos/report_columns)') %>%
  add_text(text_title = 'Supported formats', title_level = 3,
           text = 'As of version 0.2.5, chronicle can output both static and dynamic
outputs. Dynamic outputs refer to R Markdown formats that support html
widgets, hence the elements added will be dynamic plots (plotly,
dygraph, DT). For static outputs, these will roll back to ggplot and
static table prints.') %>%
  add_text(text_title = 'Dynamic outputs (html)', title_level = 5, text = '

-   bookdown
-   github_document (you are reading one right now!)
-   [html_document](https://pheymanss.github.io/chronicle-demos/outputs/output_html_document)
-   [html_notebook](https://pheymanss.github.io/chronicle-demos/outputs/output_html_notebook.nb.html)
-   [ioslides](https://pheymanss.github.io/chronicle-demos/outputs/output_ioslides)
-   [prettydoc](https://pheymanss.github.io/chronicle-demos/outputs/output_prettydoc)
-   [rmdformats](https://pheymanss.github.io/chronicle-demos/outputs/output_rmdformats) (this is currently the default)
-   [slidy_presentation](https://pheymanss.github.io/chronicle-demos/outputs/output_slidy_presentation)
-   [tufte_html](https://pheymanss.github.io/chronicle-demos/outputs/output_tufte_html)') %>%
  add_text(text_title = 'Static outputs ', title_level = 5, text = '

-   [pagedown](https://pheymanss.github.io/chronicle-demos/outputs/output_pagedown)
-   pdf
-   powerpoint_presentation
-   [rolldown](https://pheymanss.github.io/chronicle-demos/outputs/output_rolldown)
-   tufte_handout
-   word_document

Additionally, [\\{flexdashboard\\}](https://rmarkdown.rstudio.com/flexdashboard/)
and [\\{xaringan\\}](https://slides.yihui.org/xaringan/#1) technically compile,
but the layout is stiff in flexdashboard and altogether incorrect in xaringan. Also,
[\\{rticles\\}](https://github.com/rstudio/rticles) support can technically be
added, but that would involve a plethora of additional parameters for the header,
and frankly, writing a journal article is not the intended use of the package') %>%
  add_text(text_title = 'Supported report elements', title_level = 3, text = 'I highly encourage
  you to review the enitre
[showcase](https://pheymanss.github.io/chronicle-demos/showcase), words are not
as adequate to describe each element. But for a quick glance, as of version 0.2.5
chronicle supports:

-   Bar plots
-   Box plots
-   Code (optionally evaluated)
-   Density plots
-   Dygraphs
-   Histograms
-   Line plots
-   Rain cloud plots
-   Scatter plots
-   Tables
-   Texts
-   Titles
-   Violin plots') %>%
  chronicle::render_report(filename = 'README',
                           title = '\\{chronicle\\}',
                           # author = NULL,
                           output_format = c('github_document'),
                           keep_rmd = TRUE,
                           table_of_content = FALSE,
                           number_sections = FALSE)
tictoc::toc()
