#' Plot all columns of a table
#'
#' Make boxplots for each numerical variable on a table, and barplots for each
#' categorical variable.
#'
#' @param dt Table to be plotted.
#' @param by_column Name of the colum to use as groups for all the other plots
#'
#' @return A list of plotly::ggplotly objects, one for each column of the table.
#' @export
#'
#' @examples
#' chronicle::plot_columns(dt = iris, by_column = 'Species')
plot_columns <- function(dt, by_column = NULL){
  # numeric values
  nums <- dt %>% purrr::keep(is.numeric) %>% colnames()
  nums %<>% purrr::set_names(nums)
  if(length(nums) > 0){
    num_plots <- nums %>% purrr::map(~make_boxplot(dt = dt,
                                                   value = .x,
                                                   groups = by_column,
                                                   jitter = TRUE))
  }

  # categorical values
  cats <- dt %>% purrr::discard(is.numeric) %>% colnames()
  cats %<>% purrr::set_names(cats)
  if(length(cats) > 0){
    cat_plots <- cats %>% purrr::map(~make_barplot(dt = dt,
                                                   bars = .x,
                                                   break_bars_by = by_column))
  }
  return(append(num_plots, cat_plots)[colnames(dt)])
}


#' HTML interactive report detailing each column on a table
#'
#' Creates an Rmarkdown report plotting each column of a dataset.
#' Categorical columns are plotted in bar plots, and numerical
#' columns are plotted in box plots. If 'by_column' is provided,
#' these plots will be grouped by the values of that column
#'
#' @param dt Table to be plotted.
#' @param by_column Name of the column to use as groups for all the other plots
#' @param filename Name of the output file.
#' @param output_format The format of the R Markdown file. Default is prettydoc. Currently supported: 'prettydoc', 'ioslides', 'tufte', 'flexdashboard', 'slidy_presentation', 'html_document' and 'html_notebook'.
#' @param author Author of the report.
#' @param horizontal_bars Plot bars for categorical variables horizontally. Default is FALSE
#' @param sort_bars_value Sort the bars by value. Default is FALSE.
#' @param sort_bars_decreasingly Sort the bars decreasingly. Default is TRUE.
#' @param prettydoc_theme Name of the theme used on prettydoc. Default is leonids.
#' @param number_sections Whether or not to number the sections and subsections fo the report.
#' @param table_of_content Whether or not to include a table fo content at the beginning of the report.
#' @param table_of_content_depth The depth of sections and subsections to be displayed on the table of content.
#' @param fig_width Set the global figure width or the rmarkdown file.
#' @param fig_height Set the global figure height or the rmarkdown file.
#' @param directory The directory in which to render the .html report
#' @param keep_rmd Whether or not to keep the .Rmd file. Default is false.
#' @param render_reports Whether or not to render the reports. Default is TRUE. Set render_reports = FALSE and keep_rmd = TRUE to only build the R Markdown files
#'
#' @return Creates an HTML file with a plot for each column on the given table: a box plot for each numerical variable, and a bar plot for each categorical variable.
#' @export
#'
#' @examples
#' # chronicle::report_columns(dt = iris,
#' #                           by_column = 'Species',
#' #                           horizontal_bars = TRUE,
#' #                           keep_rmd = TRUE)
#' # file.remove('iris_column_analysis.Rmd')
#' # file.remove('iris_column_analysis.html')
report_columns <- function(dt,
                           by_column = NULL,
                           filename = NULL,
                           output_format = 'rmdformats',
                           author = 'chronicle user',
                           horizontal_bars = FALSE,
                           sort_bars_value = FALSE,
                           sort_bars_decreasingly = TRUE,
                           prettydoc_theme = 'leonids',
                           number_sections = TRUE,
                           table_of_content = TRUE,
                           table_of_content_depth = 1,
                           fig_width = 11,
                           fig_height = 5,
                           directory = getwd(),
                           keep_rmd = FALSE,
                           render_reports = TRUE){

  # create report title
  dt_name <- deparse(substitute(dt))
  title <- paste(dt_name,
                 'Variable Analysis',
                 if(!is.null(by_column)){paste('by', by_column)})

  # create filename
  if(is.null(filename)){
    filename <- gsub(pattern = '[^[:alnum:][:space:]]',
                     replacement = '',
                     x = dt_name) %>%
      paste0('_column_analysis')
  }

  # add sections for all numeric values
  nums <- dt %>% purrr::keep(is.numeric) %>% colnames()
  nums %<>% purrr::set_names(nums)
  if(length(nums) > 0){
    num_plots <- nums %>% purrr::map(~chronicle::add_boxplot(dt = dt_name,
                                                  value = .x,
                                                  groups = by_column,
                                                  title_level = 1,
                                                  jitter = TRUE,
                                                  boxplot_title = .x))
  }else{
    num_plots <- NULL
  }

  # add sections for all categorical values
  cats <- dt %>% purrr::discard(is.numeric) %>% colnames()
  cats %<>% purrr::set_names(cats)
  if(length(cats) > 0){
    cat_plots <- cats %>% purrr::map(~chronicle::add_barplot(dt = dt_name,
                                                  bars = .x,
                                                  break_bars_by = by_column,
                                                  horizontal = horizontal_bars,
                                                  sort_by_value = sort_bars_value,
                                                  sort_decreasing = sort_bars_decreasingly,
                                                  title_level = 1,
                                                  barplot_title = .x))
  }else{
    cat_plots <- NULL
  }

  # join both cats and nums in their order of appearance in dt
  if(is.null(by_column)){
    plot_sections <- append(num_plots,cat_plots)[colnames(dt)] %>%
      purrr::reduce(paste, sep = '\n')
  }else{
    # arranged by column number but keeping `by_column` first
    plot_sections <-
      append(num_plots, cat_plots
      )[c(by_column,
          setdiff(colnames(dt), by_column))] %>%
      purrr::reduce(paste, sep = '\n')
  }

  if(!render_reports){
    return((plot_sections))
  }

  # render report
  chronicle::render_report(report = plot_sections,
                           title = title,
                           author = author,
                           filename = filename,
                           output_format = output_format,
                           prettydoc_theme = prettydoc_theme,
                           number_sections = number_sections,
                           table_of_content = table_of_content,
                           table_of_content_depth = table_of_content_depth,
                           fig_width = fig_width,
                           fig_height = fig_height,
                           directory = directory,
                           keep_rmd = keep_rmd,
                           render_reports = render_reports)

}
