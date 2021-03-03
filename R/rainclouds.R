#' Create a raincloud plot from a data frame through ggplotly
#'
#' @param dt data.frame containing the data to plot.
#' @param value Name of the column to use as values on the y axis of the plot.
#' @param groups Name of the column containing the different groups.
#' @param adjust Width of the kernel bins. The smaller the value, the higher the resolution of the density. For full details, see ?ggplot2::stat_density.
#' @param include_boxplot Include a boxplot over the raincloud. Default is TRUE.
#' @param include_mean Mark the median of each distribution. Default is TRUE.
#' @param include_median Mark the mean of each distribution. Default is FALSE.
#' @param force_all_jitter_obs When the data has more than 1000 observations, the function will sample 1000 observations in order to keep the object reasonably small. If you need to override it, set this value to TRUE.
#' @param ggtheme ggplot2 theme function to apply. Default is ggplot2::theme_minimal.
#' @param x_axis_label Label for the x axis.
#' @param plot_palette Character vector of hex codes specifying the colors to use on the plot.
#' @param plot_palette_generator Palette from the viridis package used in case plot_palette is unspecified or insufficient for the number of colors required.
#' @param static If TRUE, the output will be static ggplot chart instead of an interactive ggplotly chart. Default is FALSE.
#'
#' @export
#' @return A plotly-ized version of a ggplot raincloud plot.
#'
#' @examples
#' make_raincloud(dt = iris, value = 'Sepal.Width')
#' make_raincloud(dt = iris, value = 'Sepal.Width', adjust = 1)
#' make_raincloud(dt = iris, value = 'Petal.Length', groups = 'Species', static = TRUE, adjust = 1)
#' make_raincloud(dt = iris, value = 'Sepal.Length', groups = 'Species', adjust = 1)
#' @importFrom rlang .data
make_raincloud <- function(dt,
                           value,
                           groups = NULL,
                           adjust = .5,
                           include_boxplot = TRUE,
                           include_mean = FALSE,
                           include_median = TRUE,
                           force_all_jitter_obs = FALSE,
                           ggtheme = 'minimal',
                           x_axis_label = NULL,
                           plot_palette = NULL,
                           plot_palette_generator = 'plasma',
                           static = FALSE){

  # check all columns are present in the data
  dt_cols <- c(value, groups)
  if(any((!dt_cols %in% colnames(dt)))){
    stop(paste(setdiff(dt_cols, colnames(dt)), collapse = ', '), ' not found on dt.')
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
    warning('Insufficient palette length provided for a raincloud plot of ',
            value, if(!is.null(groups)){paste(' by', groups)},
            '. Adding the missing ', (plot_palette_length - length(plot_palette)),
            ' colors from plot_palette_generator')
    plot_palette <- c(plot_palette,
                      plot_palette_generator(plot_palette_length - length(plot_palette), begin = 0, end = .8))
  }

  if(is.null(groups)){
    # make a dummy group variable
    groups <- 'groups'
    dt$groups <- 'A'
  }

  # prepare data for plotting
  plot_dt <- data.table::copy(dt)
  data.table::setDT(plot_dt)

  #NULL assignment to please RMD check
  dens_height <- density <- iqr <- lower_whisker <- median <-  p25 <- p75 <- quantile <- upper_whisker <- NULL
  plot_dt[, dens_height := max(density(get(value), na.rm = TRUE)$y), by = groups]

  boxplot_stats <- plot_dt[, list(median = round(median(get(value), na.rm = TRUE), 2),
                               mean = round(mean(get(value), na.rm = TRUE), 2),
                               p25 = round(quantile(get(value), .25, na.rm = TRUE),2),
                               p75 = round(quantile(get(value), .75, na.rm = TRUE),2),
                               dens_height = max(density(get(value), na.rm = TRUE)$y)),
                           by = groups
                           ][, iqr := p75 - p25
                           ][, lower_whisker := p25-1.5*iqr
                           ][, upper_whisker := p75+1.5*iqr][]

  # if more than 1000 rows, create the jitter over a sample
  jitter_some <- is.null(groups) & !as.logical(force_all_jitter_obs)
  if(jitter_some){
    warning('geom_jitter will be created with a sample of 1000 observations')
    jitter_dt <- plot_dt[sample(1000, replace = FALSE)]
  }

  jitter_height <- min(boxplot_stats$dens_height)


  # Raincloud ---------------------------------------------------------------

  raincloud <-
    ggplot2::ggplot() +
    # colors
    ggplot2::scale_fill_manual(values = plot_palette) +
    ggplot2::scale_color_manual(values = plot_palette) +
    # cloud
    ggplot2::geom_density(data = plot_dt,
                          mapping = ggplot2::aes(x = .data[[value]],
                                                 fill = .data[[groups]],
                                                 color = NA),
                          alpha = .4,
                          adjust = as.numeric(adjust)) +
    # rain
    ggplot2::geom_jitter(data = if(jitter_some){jitter_dt}else{plot_dt},
                         mapping = ggplot2::aes(x = .data[[value]],
                                                y = -dens_height/6,
                                                fill = .data[[groups]],
                                                color = .data[[groups]]),
                         height = jitter_height/6,
                         alpha = .5,
                         size = 2) +
    ggplot2::facet_grid(rows = groups, scales = 'free_y') +
    # theming
    ggtheme() +
    ggplot2::theme(panel.background = ggplot2::element_rect(fill = "transparent", colour = NA),
                   plot.background =  ggplot2::element_rect(fill = "transparent", colour = NA),
                   legend.position = 'none',
                   panel.grid.major.y = ggplot2::element_blank(),
                   panel.grid.minor.x = ggplot2::element_blank(),
                   axis.title.y = ggplot2::element_blank(),
                   axis.text.y = ggplot2::element_blank(),
                   axis.ticks.y = ggplot2::element_blank(),
                   strip.text.y = ggplot2::element_text(size = 13)) +
    ggplot2::scale_y_continuous(labels = scales::number_format(accuracy = 0.01,
                                                               decimal.mark = '.',
                                                               big.mark = ',')) +
    ggplot2::coord_cartesian(clip = "off", expand = TRUE)


  # Boxplot -----------------------------------------------------------------
  if(as.logical(include_boxplot)){
    include_median <- TRUE
    raincloud <- raincloud +
      # box
      ggplot2::geom_rect(data = boxplot_stats,
                         mapping = ggplot2::aes(xmin = p25,
                                                xmax = median,
                                                ymin = -dens_height/3,
                                                ymax = dens_height/3,
                                                fill = .data[[groups]]),
                         color = NA,
                         size = .9) +
      ggplot2::geom_rect(data = boxplot_stats,
                         mapping = ggplot2::aes(xmin = median,
                                                xmax = p75,
                                                ymin = -dens_height/3,
                                                ymax = dens_height/3,
                                                fill = .data[[groups]]),
                         color = NA,
                         size = .9) +
      # lower_whisker
      ggplot2::geom_segment(data = boxplot_stats,
                            mapping = ggplot2::aes(x = lower_whisker,
                                                   xend = lower_whisker,
                                                   y = -dens_height/5,
                                                   yend = dens_height/5,
                                                   color = .data[[groups]]),
                            size = .9) +
      # upper whisker
      ggplot2::geom_segment(data = boxplot_stats,
                            mapping = ggplot2::aes(x = upper_whisker,
                                                   xend = upper_whisker,
                                                   y = -dens_height/5,
                                                   yend = dens_height/5,
                                                   color = .data[[groups]]),
                            size = .9) +
      # horizontal line
      ggplot2::geom_segment(data = boxplot_stats,
                            mapping = ggplot2::aes(x = lower_whisker,
                                                   xend = upper_whisker,
                                                   y = 0,
                                                   yend = 0,
                                                   color = .data[[groups]]),
                            size = 1.4)
  }


  # median ------------------------------------------------------------------
  if(as.logical(include_median)){
    raincloud <- raincloud +
      ggplot2::geom_segment(data = boxplot_stats,
                            mapping = ggplot2::aes(x = median,
                                                   xend = median,
                                                   y = -dens_height/3,
                                                   yend = dens_height/3),
                            color = 'white',
                            alpha = .5,
                            size = 1)  +
      # ggplot2::geom_point(data = boxplot_stats,
      #                     ggplot2::aes(x = median,
      #                                  y = 0,
      #                                  fill = .data[[groups]]),
      #                     color = 'white',
      #                     size = 4,
      #                     shape = 21) +
      ggplot2::geom_text(data = boxplot_stats,
                         ggplot2::aes(x = median,
                                      label = median,
                                      y = dens_height/3*1.3,
                                      color = .data[[groups]]),
                         # fontface = 'bold',
                         size = 4)
  }


  # mean --------------------------------------------------------------------
  if(as.logical(include_mean)){
    raincloud <- raincloud +
      ggplot2::geom_segment(data = boxplot_stats,
                            mapping = ggplot2::aes(x = mean,
                                                   xend = mean,
                                                   y = 0,
                                                   yend = dens_height,
                                                   color = .data[[groups]]),
                            linetype = 2,
                            size = 1)
  }

  # axes
  if(!is.null(x_axis_label)){
    raincloud <- raincloud + ggplot2::xlab(x_axis_label)
  }

  if(!as.logical(static)){
    raincloud <- plotly::ggplotly(raincloud, tooltip = c('x', if(groups != 'groups'){'fill'}))
  }
  return(raincloud)
}


#' Add a raincloud plot to a chronicle report
#'
#' @param report Character string containing all the R Markdown chunks previously added. Default is '', an empty report.
#' @param dt data.frame containing the data to plot.
#' @param value Name of the column to use as values on the y axis of the plot.
#' @param groups Name of the column containing the different groups.
#' @param adjust Width of the kernel bins. The smaller the value, the higher the resolution of the density. For full details, see ?ggplot2::stat_density.
#' @param include_boxplot Include a boxplot over the raincloud. Default is TRUE.
#' @param include_mean Mark the median of each distribution. Default is TRUE.
#' @param include_median Mark the mean of each distribution. Default is FALSE.
#' @param force_all_jitter_obs When the data has more than 1000 observations, the function will sample 1000 observations in order to keep the object reasonably small. If you need to override it, set this value to TRUE.
#' @param ggtheme ggplot2 theme function to apply. Default is ggplot2::theme_minimal.
#' @param x_axis_label Label for the x axis.
#' @param plot_palette Character vector of hex codes specifying the colors to use on the plot.
#' @param plot_palette_generator Palette from the viridis package used in case plot_palette is unspecified or insufficient for the number of colors required.
#' @param static If TRUE, the output will be static ggplot chart instead of an interactive ggplotly chart. Default is FALSE.
#' @param raincloud_title Title of the raincloud plot  section on the report. If NULL, chronicle will try to parse a generic title using make_title()
#' @param title_level Level of the section title of this plot (ie, number of # on Rmarkdown syntax.)
#' @param echo Whether to display the source code in the output document. Default is FALSE.
#' @param message Whether to preserve messages on rendering. Default is FALSE.
#' @param warning Whether to preserve warnings on rendering. Default is FALSE.
#' @param fig_width Width of the plot (in inches).
#' @param fig_height Height of the plot (in inches).
#'
#' @return An rmarkdown file as a character string, now containing a chunk for adding the specified raincloud plot.
#' @export
#'
#' @examples
#' html_report <- add_raincloud(report = "",
#'                              dt = iris,
#'                              value = 'Sepal.Length',
#'                              groups = 'Species')
#' cat(html_report)
add_raincloud <- function(report = '',
                          dt,
                          value,
                          groups = NULL,
                          adjust = .5,
                          include_boxplot = TRUE,
                          include_mean = FALSE,
                          include_median = TRUE,
                          force_all_jitter_obs = FALSE,
                          ggtheme = 'minimal',
                          x_axis_label = NULL,
                          plot_palette = NULL,
                          plot_palette_generator = 'plasma',
                          static = NULL,
                          raincloud_title = NULL,
                          title_level = 2,
                          echo = FALSE,
                          message = FALSE,
                          warning = FALSE,
                          fig_width = NULL,
                          fig_height = NULL){

  # if a data.frame is provided, check if the specified columns are present
  if(is.data.frame(dt)){
    dt_cols <- c(value, groups)
    if(any(!(dt_cols %in% colnames(dt)))){
      stop(paste(setdiff(dt_cols, colnames(dt)), collapse = ', '), ' not found on dt.')
    }
  }

  # if not specified, make sure each raincloud has 1 inch
  # if(is.null(fig_width)){
  #   fig_width <- ifelse(test = is.null(groups),
  #                       yes = 1,
  #                       no = data.table::uniqueN(dt[[groups]]))
  # }

  params <- list(value = value,
                 groups = groups,
                 adjust = adjust,
                 include_boxplot = include_boxplot,
                 include_mean = include_mean,
                 include_median = include_median,
                 force_all_jitter_obs = force_all_jitter_obs,
                 ggtheme = ggtheme,
                 x_axis_label = x_axis_label,
                 plot_palette = plot_palette,
                 plot_palette_generator = plot_palette_generator) %>%
    purrr::discard(is.null)

  report <- chronicle::add_chunk(report = report,
                                 dt_expr = ifelse(test = is.character(dt),
                                                  yes = dt,
                                                  no = deparse(substitute(dt))),
                                 fun = make_raincloud,
                                 params = params,
                                 chunk_title = raincloud_title,
                                 title_level = title_level,
                                 echo = echo,
                                 message = message,
                                 warning = warning,
                                 fig_width = fig_width,
                                 fig_height = fig_height)
  return(report)
}
