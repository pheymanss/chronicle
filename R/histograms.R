#' Create a histogram plot from a data frame through ggplotly
#'
#' @param dt data.frame containing the data to plot.
#' @param value Name of the column to use as values on the y axis of the plot.
#' @param groups Name of the column containing the different groups.
#' @param binwidth Width of the histogram bins.
#' @param bins Number of bins. Overridden by binwidth. Defaults to 30.
#' @param scales From ggplot2::facet_wrap: Should scales be 'fixed', 'free', or free in one dimension ('free_x', 'free_y'). Default is 'fixed'.
#' @param ggtheme ggplot2 theme function to apply. Default is ggplot2::theme_minimal.
#' @param x_axis_label Label for the x axis.
#' @param plot_palette Character vector of hex codes specifying the colors to use on the plot.
#' @param plot_palette_generator Palette from the viridis package used in case plot_palette is unspecified or insufficient for the number of colors required.
#' @param static If TRUE (or if the dataset is over 10,000 rows), the output will be static ggplot chart instead of an interactive ggplotly chart. Default is FALSE.
#'
#' @export
#' @return A plotly-ized version of a grouped ggplot histogram plot.
#'
#' @examples
#' make_histogram(dt = iris,
#'                value = 'Sepal.Length',
#'                groups = 'Species')
#' @importFrom rlang .data
make_histogram <- function(dt,
                           value,
                           groups = NULL,
                           binwidth = NULL,
                           bins = 30,
                           scales = 'fixed',
                           ggtheme = 'minimal',
                           x_axis_label = NULL,
                           plot_palette = NULL,
                           plot_palette_generator = 'plasma',
                           static = FALSE){

  dt_cols <- c(value, groups)
  if(any((!dt_cols %in% colnames(dt)))){
    stop(paste(setdiff(dt_cols, colnames(dt)), collapse = ', '), ' not found on dt.')
  }

  # coerce groups and split_groups_by to characters
  if(!is.null(groups)){
    dt <- chronicle::set_classes(dt, character = c(groups))
  }

  # check how many colors are needed for plotting
  plot_palette_length <- ifelse(test = is.null(groups),
                                yes = 1,
                                no = data.table::uniqueN(dt[[groups]]))


  # map the gg theme to its corresponding ggplot2::theme_ function
  ggtheme <- switch(ggtheme,
                    'bw' = ggplot2::theme_bw,
                    'classic' = ggplot2::theme_classic,
                    'dark' = ggplot2::theme_dark,
                    'gray' = ggplot2::theme_gray,
                    'grey' = ggplot2::theme_grey,
                    'light' = ggplot2::theme_light,
                    'linedraw' = ggplot2::theme_linedraw,
                    'minimal' = ggplot2::theme_minimal,
                    'void' = ggplot2::theme_void,
                    ggplot2::theme_minimal)


  # map the generator to its corresponding viridis palette
  plot_palette_generator <- switch(plot_palette_generator,
                                   'cividis' = viridis::cividis,
                                   'inferno' = viridis::inferno,
                                   'magma' = viridis::magma,
                                   'plasma' = viridis::plasma,
                                   'viridis' = viridis::viridis,
                                   viridis::plasma)

  #if not provided, use palette from viridis::plasma
  if(is.null(plot_palette)){
    plot_palette <- plot_palette_generator(plot_palette_length, begin = 0, end = .80)
  }else if(plot_palette_length > length(plot_palette)){
    warning('Insufficient palette length provided for a histogram plot of ',
            value, if(!is.null(groups)){paste(' by', groups)},
            '. Adding the missing ', (plot_palette_length - length(plot_palette)),
            ' colors from plot_palette_generator')
    plot_palette <- c(plot_palette,
                      plot_palette_generator(plot_palette_length - length(plot_palette), begin = 0, end = .8))
  }

  # create the plot structure depending of the group
  hide_groups <- FALSE
  if(is.null(groups)){
    # make a dummy group variable
    hide_groups <- TRUE
    groups <- 'groups'
    dt$groups <- 'A'
  }
  histogram <- ggplot2::ggplot(dt,
                             ggplot2::aes(x = .data[[value]],
                                          fill = .data[[groups]],
                                          color = .data[[groups]])) +
    ggtheme() +
    ggplot2::theme(panel.background = ggplot2::element_rect(fill = "transparent", colour = NA),
                   plot.background =  ggplot2::element_rect(fill = "transparent", colour = NA)) +
    ggplot2::scale_y_continuous(labels = scales::number_format(accuracy = 0.01,
                                                               decimal.mark = '.',
                                                               big.mark = ',')) +
    ggplot2::geom_histogram(alpha = 0.85, bins = bins) +
    ggplot2::scale_fill_manual(values = plot_palette) +
    ggplot2::scale_color_manual(values = plot_palette)

  if(hide_groups){
    # remove all references to the dummy group variable from the plot
    histogram <- histogram +
      ggplot2::theme(legend.position = 'none',
                     axis.title.x = ggplot2::element_blank(),
                     axis.text.x = ggplot2::element_blank(),
                     axis.ticks.x = ggplot2::element_blank(),
                     strip.text.x = ggplot2::element_blank())
  }

  # axes
  if(!is.null(x_axis_label)){
    histogram <- histogram + ggplot2::xlab(x_axis_label)
  }

  # facet by groups (to avoid stacking)
  histogram <- histogram + ggplot2::facet_wrap(stats::as.formula(paste(groups, '~ .')),
                                               scales = scales)

  # only convert to plotly if the dataset is under 10,000 rows
  if(!as.logical(static) & nrow(dt) <= 10000){
    histogram <- plotly::ggplotly(histogram,
                                  tooltip = c('x', 'y', if(groups != 'groups'){'fill'}))
  }

  return(histogram)
}

#' Add a histogram plot to a chronicle report
#'
#' @param report Character string containing all the R Markdown chunks previously added. Default is '', an empty report.
#' @param dt data.frame containing the data to plot.
#' @param value Name of the column to use as values on the y axis of the plot.
#' @param groups Name of the column containing the different groups.
#' @param binwidth Width of the histogram bins.
#' @param bins Number of bins. Overridden by binwidth. Defaults to 30.
#' @param scales From ggplot2::facet_wrap: Should scales be 'fixed', 'free', or free in one dimension ('free_x', 'free_y'). Default is 'fixed'.
#' @param ggtheme ggplot2 theme function to apply. Default is ggplot2::theme_minimal.
#' @param x_axis_label Label for the x axis.
#' @param plot_palette Character vector of hex codes specifying the colors to use on the plot.
#' @param plot_palette_generator Palette from the viridis package used in case plot_palette is unspecified or insufficient for the number of colors required.
#' @param histogram_title Title of the histogram plot  section on the report. If NULL, chronicle will try to parse a generic title using make_title()
#' @param title_level Level of the section title of this plot (ie, number of # on Rmarkdown syntax.)
#' @param echo Whether to display the source code in the output document. Default is FALSE.
#' @param message Whether to preserve messages on rendering. Default is FALSE.
#' @param warning Whether to preserve warnings on rendering. Default is FALSE.
#' @param fig_width Width of the plot (in inches).
#' @param fig_height Height of the plot (in inches).
#'
#' @return An rmarkdown chunk as a character string, now containing a chunk for adding the histogram plot.
#' @export
#'
#' @examples
#' html_report <- add_histogram(report = "",
#'                              dt = iris,
#'                              value = 'Sepal.Length',
#'                              groups = 'Species')
#' cat(html_report)
add_histogram <- function(report = '',
                        dt,
                        value,
                        groups = NULL,
                        binwidth = NULL,
                        bins = NULL,
                        scales = 'fixed',
                        ggtheme = NULL,
                        x_axis_label = NULL,
                        plot_palette = NULL,
                        plot_palette_generator = NULL,
                        histogram_title = NULL,
                        title_level = 2,
                        echo = FALSE,
                        message = FALSE,
                        warning = FALSE,
                        fig_width = NULL,
                        fig_height = NULL){

  dt_cols <- c(value, groups)
  if(any((!dt_cols %in% colnames(dt)))){
    stop(paste(setdiff(dt_cols, colnames(dt)), collapse = ', '), ' not found on dt.')
  }

  params <- list(dt = ifelse(test = is.character(dt),
                             yes = dt,
                             no = deparse(substitute(dt))),
                 value = value,
                 groups = groups,
                 binwidth = binwidth,
                 bins = bins,
                 scales = scales,
                 ggtheme = ggtheme,
                 x_axis_label = x_axis_label,
                 plot_palette = ifelse(is.null(plot_palette), 'params$plot_palette', plot_palette),
                 plot_palette_generator = ifelse(is.null(plot_palette_generator), 'params$plot_palette_generator', plot_palette_generator),
                 static = 'params$set_static') %>%
    purrr::discard(is.null)

  report <- chronicle::add_chunk(report = report,
                                 fun = chronicle::make_histogram,
                                 params = params,
                                 chunk_title = histogram_title,
                                 title_level = title_level,
                                 echo = echo,
                                 message = message,
                                 warning = warning,
                                 fig_width = fig_width,
                                 fig_height = fig_height,
                                 guess_title = TRUE)
  return(report)
}

