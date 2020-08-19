

#' Plot a time series from a data frame through dygraph's interactive html plot interface
#'
#' @param dt data.frame containing the data to plot. It must have a numerical variable, a date variable, and optionally a grouping variable to split the data and plot them as individual time sieries inside the same plot.
#' @param num_var Name of the column of the data frame containing the numerical variables of the time series.
#' @param date_var Name of the column containing the date variable. It must be already a date or time object.
#' @param group_var Name of the columns containing the different groups.
#' @param y_axis_label Label for the y axis. x axis is the date (or time) so it is not needed
#' @param plot_palette Character vector of hex codes specifying the colors to use on the plot. Default is RColorBrewer's Paired and Spectral colors concatenated.
#'
#' @return a dygraph of the numerical variable specified, optionally split by the values of a group_var.
#' @export
#'
#' @examples
#'
#' # mock data
#' dat <- data.frame(x = rnorm(1000, mean = 100, sd = 15),
#'                   date = as.Date('2020-01-01') + floor(runif(1000, min = -100, max = 100))
#' make_dygraph(dt = dat, num_var = 'x', date_var = 'date')
#'
make_dygraph <- function(dt,
                         num_var,
                         date_var,
                         group_var = NULL,
                         y_axis_label = NULL,
                         plot_palette = c("#58508d", "#ffa600", "#ff6361", "#003f5c", "#bc5090", "#A6CEE3",
                                          "#1F78B4", "#B2DF8A", "#33A02C", "#FB9A99", "#E31A1C", "#FDBF6F",
                                          "#FF7F00", "#CAB2D6", "#6A3D9A", "#FFFF99")){
  # create ths custom dygraph structure needed for plotting: fisrt column should be the time,
  # all columns after that should be numerical variables.

  # if(!is.numeric(dt[[num_var]])){
  #   stop(paste0('dt$', num_var, 'must be a numeric column.'))
  # }

  if(any(grepl(x = class(dt[[num_var]]), pattern = 'POSIXct|Date|timeDate|yearmon|yearqtr'))){
    stop(paste0('dt$', date_var, 'must be a class supported by xts::xts. As of version 0.10-2, supported classes include: "Date", "POSIXct", "timeDat", as well as "yearmon" and "yearqtr"'))
  }

  dy_cols <- c(date_var, num_var, group_var)
  dy_data <- data.table::as.data.table(dt) %>%
    purrr::keep(colnames(.) %in% dy_cols) %>%
  # aggregate values by date_var
    data.table::dcast(
      formula = as.formula(paste(c(date_var,'~',ifelse(is.null(group_var),
                                                       yes = '.',
                                                       no = group_var)), collapse = ' ')),
                      value.var = num_var,
                      fun.aggregate	= sum) %>%
    zoo::na.locf()

  # labeling
  y_axis_label <- ifelse(test = is.null(y_axis_label),
                         yes = num_var,
                         no = y_axis_label)

  dy_plot <- dygraphs::dygraph(dy_data) %>%
    dygraphs::dyOptions(includeZero = FALSE,
                        colors = plot_palette,
                        fillGraph = TRUE,
                        fillAlpha = 0.4,
                        drawPoints = TRUE,
                        pointSize = 2) %>%
    dygraphs::dyRangeSelector(height = 20) %>%
    dygraphs::dyHighlight(highlightCircleSize = 5,
                          hideOnMouseOut = TRUE) %>%
    dygraphs::dyAxis("x", drawGrid = FALSE) %>%
    dygraphs::dyAxis('y', label = y_axis_label,
                     valueFormatter = 'function(d){return d.toString().replace(/\\B(?=(\\d{3})+(?!\\d))/g, ",");}')

  return(dy_plot)
}


#' Add a dygraph to a chronicle report
#'
#' @param report character containing the text of an Rmarkdown report header (and possibly more chunks). Easily create one with chronicle::new_report()
#' @param dt data.frame containing the data to plot. It must have a numerical variable, a date variable, and optionally a grouping variable to split the data and plot them as individual time sieries inside the same plot.
#' @param num_var Name of the column of the data frame containing the numerical variables of the time series. If there are several values for the same value of date_var, the function automatically sums all the values to report a single point by time value of the series.
#' @param date_var Name of the column containing the date variable. It must be already a date or time object.
#' @param group_var Name of the columns containing the different groups.
#' @param dygraph_title Title of the dygraph.
#' @param title_level Level of the section title of this plot (ie, number of # on Rmarkdown syntax.)
#' @param y_axis_label Label for the y axis. x axis is the date (or time) so it is not needed.
#' @param plot_palette Character vector of hex codes specifying the colors to use on the plot. Default is RColorBrewer's Paired and Spectral colors concatenated.
#'
#' @return
#' @export
#'
#' @examples
add_dygraph <- function(report = new_report(),
                        dt,
                        num_var,
                        date_var,
                        group_var = NULL,
                        dygraph_title = NULL,
                        title_level = 2,
                        y_axis_label = NULL,
                        plot_palette = c("#58508d", "#ffa600", "#ff6361", "#003f5c", "#bc5090", "#A6CEE3",
                                         "#1F78B4", "#B2DF8A", "#33A02C", "#FB9A99", "#E31A1C", "#FDBF6F",
                                         "#FF7F00", "#CAB2D6", "#6A3D9A", "#FFFF99")){
  dygraph_title <- ifelse(test = is.null(dygraph_title),
                          yes = paste0(num_var, if(!is.null(group_var)){paste(', by', group_var)}),
                          no = dygraph_title)

  report <- paste(report, # report object (Rmarkdown file text)
                  paste(paste(rep('#', title_level), collapse = ''), dygraph_title),
                  paste('```{r, echo = FALSE, message = FALSE, warning = FALSE}', # r chunk header
                    # make_dygraph
                    paste(c(paste0('chronicle::make_dygraph(dt = ', deparse(substitute(dt)), ','),
                            paste0('                        num_var = "', num_var, '",'),
                            paste0('                        date_var = "', date_var, '",'),
                            if (!is.null(group_var)) {paste0('                        group_var = "', group_var, '",')},
                            if (!is.null(y_axis_label)) {paste0('                        y_axis_label = "', y_axis_label, '",')},
                            paste0('                        plot_palette = c(', paste(paste0('"', plot_palette, '"'), collapse = ', '), '))')),
                          collapse = '\n'),
                    #finish
                    '```', sep = '\n'),
                  sep = '\n\n')
  return(report)
}



