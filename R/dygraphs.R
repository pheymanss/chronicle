#' Plot a time series from a data frame through dygraph's interactive html plot interface
#'
#' @param dt data.frame containing the data to plot. It must have a numerical variable, a date variable, and optionally a grouping variable to split the data and plot them as individual time series inside the same plot.
#' @param value Name of the column of the data frame containing the numerical variables of the time series.
#' @param date Name of the column containing the date variable. It must be already a date or time object.
#' @param groups Name of the columns containing the different groups.
#' @param y_axis_label Label for the y axis. x axis is the date (or time) so it is not needed
#' @param plot_palette Character vector of hex codes specifying the colors to use on the plot. Default is RColorBrewer's Paired and Spectral colors concatenated.
#' @param plot_palette_generator Palette from the viridis package used in case plot_palette is unspecified or insufficient for the number of colors required.
#' @param static If TRUE (or if the dataset is over 10,000 rows), the output will be static ggplot chart instead of a dygraph. Default is FALSE.
#'
#' @return A dygraph of the numerical variable specified, optionally split by the values of 'groups'. If static is set to TRUE, it will return a ggplot line plot
#' @export
#'
#' @examples
#' dat <- data.frame(x = c(rnorm(100, 2, 4),
#'                         rnorm(100, 6, 1),
#'                         rnorm(100, 8, 2)),
#'                   group = c(rep('A', 100),
#'                             rep('B', 100),
#'                             rep('C', 100)),
#'                   date = rep(seq(as.Date("2020-01-01"),
#'                                  as.Date("2020-04-09"),
#'                                  'days'),
#'                              3))
#' make_dygraph(dt = dat,
#'              value = 'x',
#'              date = 'date')
#' make_dygraph(dt = dat,
#'              value = 'x',
#'              groups = 'group',
#'              date = 'date')
make_dygraph <- function(dt,
                         value ,
                         date,
                         groups = NULL,
                         y_axis_label = NULL,
                         plot_palette = NULL,
                         plot_palette_generator = 'plasma',
                         static = FALSE){

  dt_cols <- c(value, date, groups)
  if(any((!dt_cols %in% colnames(dt)))){
    stop(paste(setdiff(dt_cols, colnames(dt)), collapse = ', '), ' not found on dt.')
  }

  # coerce groups to character
  if(!is.null(groups)){
    dt <- chronicle::set_classes(dt, character = c(groups))
  }

  # only build dygraphs if the dataset is under 10,000 rows
  #if not, build a line plot
  if(!as.logical(static) & nrow(dt) <= 10000){
  # check how many colors are needed for plotting
  plot_palette_length <- ifelse(test = is.null(groups),
                                yes = 1,
                                no = data.table::uniqueN(dt[[groups]]))

  # map the generator to its corresponding viridis palette
  plot_palette_generator <- switch(plot_palette_generator,
                                   'cividis' = viridis::cividis,
                                   'inferno' = viridis::inferno,
                                   'magma' = viridis::magma,
                                   'plasma' = viridis::plasma,
                                   'viridis' = viridis::viridis,
                                   viridis::magma)

  #if not provided, use palette from viridis::plasma
  if(is.null(plot_palette)){
    plot_palette <- plot_palette_generator(plot_palette_length, begin = 0, end = .8)
  }else if(plot_palette_length > length(plot_palette)){
    warning('Insufficient palette length provided for a dygraph plot of ',
            value, ' by ', if(!is.null(groups)){paste(' by', groups)},
            '. Adding the missing ', (plot_palette_length - length(plot_palette)),
            ' colors from plot_palette_generator')
    plot_palette <- c(plot_palette,
                      plot_palette_generator(plot_palette_length - length(plot_palette), begin = 0, end = .8))
  }

    # create the custom dygraph structure needed for plotting: first column should be the time,
    # all columns after that should be numerical variables.

    # check data_var is actually of class Date
    if(any(grepl(x = class(dt[[value]]), pattern = 'POSIXct|Date|timeDate|yearmon|yearqtr'))){
      stop(paste0('dt$', date, 'must be a class supported by xts::xts. As of version 0.10-2, supported classes include: "Date", "POSIXct", "timeDat", as well as "yearmon" and "yearqtr"'))
    }

    dy_cols <- c(date, value, groups)
    dy_data <- data.table::as.data.table(dt)
    dy_data <- purrr::keep(dy_data, colnames(dy_data) %in% dy_cols) %>%
      # aggregate values by date
      data.table::dcast(
        formula = stats::as.formula(paste(c(date,'~',ifelse(is.null(groups),
                                                            yes = '.',
                                                            no = groups)), collapse = ' ')),
        value.var = value,
        fun.aggregate	= sum) %>%
      zoo::na.locf()

    # labeling
    y_axis_label <- ifelse(test = is.null(y_axis_label),
                           yes = value,
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

  }else{
    dy_plot <-  make_lineplot(dt = dt,
                              x = date,
                              y = value,
                              groups = groups,
                              y_axis_label = y_axis_label,
                              plot_palette = plot_palette,
                              plot_palette_generator = plot_palette_generator,
                              static = static)
  }

  return(dy_plot)
}

#' Add a dygraph to a chronicle report
#'
#' @param dt Data to plot
#' @param report Character string containing all the R Markdown chunks previously added. Default is '', an empty report.
#' @param value Name of the column of the data frame containing the numerical variables of the time series.
#' @param date Name of the column containing the date variable. It must be already a date or time object.
#' @param groups Name of the columns containing the different groups.
#' @param y_axis_label Label for the y axis. x axis is the date (or time) so it is not needed
#' @param plot_palette Character vector of hex codes specifying the colors to use on the plot.
#' @param plot_palette_generator Palette from the viridis package used in case plot_palette is unspecified or insufficient for the number of colors required.
#' @param dygraph_title Title for the Rmarkdown section containing the dygraph
#' @param title_level Level of the section title of this plot (ie, number of # on Rmarkdown syntax.)
#' @param echo Whether to display the source code in the output document. Default is FALSE.
#' @param message Whether to preserve messages on rendering. Default is FALSE.
#' @param warning Whether to preserve warnings on rendering. Default is FALSE.
#' @param fig_width Width of the plot (in inches).
#' @param fig_height Height of the plot (in inches).
#'
#' @return An R Markdown file as a character string, now containing a chunk for the specified dygraph.
#' @export
#'
#' @examples
#' dat <- data.frame(x = c(rnorm(100, 2, 4),
#'                         rnorm(100, 6, 1),
#'                         rnorm(100, 8, 2)),
#'                  group = c(rep('A', 100),
#'                            rep('B', 100),
#'                            rep('C', 100)),
#'                  date = rep(seq(as.Date("2020-01-01"),
#'                                 as.Date("2020-04-09"),
#'                                 'days'),
#'                             3))
#'html_report <- add_dygraph(report = '',
#'                           dt = dat,
#'                           value = 'x',
#'                           date = 'date')
#'cat(html_report)
add_dygraph <- function(report = '',
                        dt,
                        value ,
                        date,
                        groups = NULL,
                        y_axis_label = NULL,
                        plot_palette = NULL,
                        plot_palette_generator = NULL,
                        dygraph_title = NULL,
                        title_level = 2,
                        echo = FALSE,
                        message = FALSE,
                        warning = FALSE,
                        fig_width = NULL,
                        fig_height = NULL){

  dt_cols <- c(value, date, groups)
  if(any((!dt_cols %in% colnames(dt)))){
    stop(paste(setdiff(dt_cols, colnames(dt)), collapse = ', '), ' not found on dt.')
  }

  params <- list(dt = ifelse(test = is.character(dt),
                             yes = dt,
                             no = deparse(substitute(dt))),
                 value = value,
                 date = date,
                 groups = groups,
                 y_axis_label = y_axis_label,
                 plot_palette = ifelse(is.null(plot_palette), 'params$plot_palette', plot_palette),
                 plot_palette_generator = ifelse(is.null(plot_palette_generator), 'params$plot_palette_generator', plot_palette_generator),
                 static = 'params$set_static') %>%
    purrr::discard(is.null)

  report <- chronicle::add_chunk(report = report,
                                 fun = chronicle::make_dygraph,
                                 params = params,
                                 chunk_title = dygraph_title,
                                 title_level = title_level,
                                 echo = echo,
                                 message = message,
                                 warning = warning,
                                 fig_width = fig_width,
                                 fig_height = fig_height,
                                 guess_title = TRUE)
  return(report)
}

