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
#' @param by_column Name of the colum to use as groups for all the other plots
#' @param filename Name of the output file.
#' @param author Author of the report.
#' @param horizontal_bars Plot bars for categorical variables horizontally. Default is FALSE
#' @param sort_bars_value Sort the bars by value. Default is FALSE.
#' @param sort_bars_decreasingly Sort the bars decreasingly. Default is TRUE.
#' @param prettydoc Whether or not to use prettydoc formatting on the html report. Default is TRUE.
#' @param prettydoc_theme Name of the theme used on prettydoc. Default is cayman.
#' @param highlight Rmarkdown highlight theming. Default is github highlighting.
#' @param number_sections Whether or not to number the sections and subsections fo the report.
#' @param table_of_content Whether or not to include a table fo content at the beginning of the report.
#' @param table_of_content_depth The depth of sections and subsections to be displayed on the table of content.
#' @param fig_width Set the global figure width or the rmarkdown file.
#' @param fig_height Set the global figure height or the rmarkdown file.
#' @param directory The directory in which to render the .html report
#' @param keep_rmd Whether or not to keep the .Rmd file. Default is false.
#' @param render_html Whether or not to render the report as an interactive hmtl file.
#'
#' @return An HTML file with a plot for each column on the given table: a boxplot for each numerical variable, and a barplot for each categorical variable.
#' @export
#'
#' @examples chronicle::report_columns(dt = iris,
#'                                     by_column = 'Species',
#'                                     horizontal_bars = TRUE,
#'                                     keep_rmd = TRUE)
#'           file_delete('iris_column_analysis.Rmd')
#'           file_delete('iris_column_analysis.html')
report_columns <- function(dt,
                           by_column = NULL,
                           filename = NULL,
                           author = 'chronicle user',
                           horizontal_bars = FALSE,
                           sort_bars_value = FALSE,
                           sort_bars_decreasingly = TRUE,
                           prettydoc = TRUE,
                           prettydoc_theme = 'leonids',
                           highlight = 'github',
                           number_sections = TRUE,
                           table_of_content = TRUE,
                           table_of_content_depth = 1,
                           fig_width = 11,
                           fig_height = 5,
                           directory = getwd(),
                           keep_rmd = FALSE,
                           render_html = TRUE){

  # create report title
  dt_name <- deparse(substitute(dt))
  title <- paste(dt_name, 'Variable Analysis', if(!is.null(by_column)){paste('by', by_column)})

  # create filename
  if(is.null(filename)){
    filename <- gsub('[^[:alnum:][:space:]]',
                     '',
                     dt_name) %>%
      paste0('_column_analysis')
  }

  #create report header
  column_report <- chronicle::new_report(title = title,
                                         author = author,
                                         prettydoc = prettydoc,
                                         prettydoc_theme = prettydoc_theme,
                                         highlight = highlight,
                                         number_sections = number_sections,
                                         table_of_content = table_of_content,
                                         table_of_content_depth = table_of_content_depth)

  # add sections for all numeric values
  nums <- dt %>% purrr::keep(is.numeric) %>% colnames()
  nums %<>% purrr::set_names(nums)
  if(length(nums) > 0){
    num_plots <- nums %>% purrr::map(~add_boxplot(report = '',
                                                  dt = dt_name,
                                                  value = .x,
                                                  groups = by_column,
                                                  title_level = 1,
                                                  jitter = TRUE))
  }else{
    num_plots <- NULL
  }

  # add sections for all categorical values
  cats <- dt %>% purrr::discard(is.numeric) %>% colnames()
  cats %<>% purrr::set_names(cats)
  if(length(cats) > 0){
    cat_plots <- cats %>% purrr::map(~add_barplot(report = '',
                                                  dt = dt_name,
                                                  bars = .x,
                                                  break_bars_by = by_column,
                                                  horizontal = horizontal_bars,
                                                  sort_by_value = sort_bars_value,
                                                  sort_decreasing = sort_bars_decreasingly,
                                                  title_level = 1))
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
          colnames(dt)[-which(colnames(dt) == by_column)])] %>%
      purrr::reduce(paste, sep = '\n')
  }

  # join header and plots
  column_report %<>% paste(plot_sections, sep = '\n')

  if(!render_html){
    return(column_report)
  }
  # render report
  chronicle::render_report(report = column_report,
                           filename = filename,
                           directory = directory,
                           keep_rmd = keep_rmd,
                           render_html = render_html)

}
