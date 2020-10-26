#' Create a density plot from a data frame through ggplotly
#'
#' @param dt data.frame containing the data to plot.
#' @param value Name of the column to use as values on the y axis of the plot.
#' @param groups Name of the column containing the different groups.
#' @param faceted If TRUE (defualt), each group will be plotted separatedly.
#' @param ggtheme ggplot2 theme function to apply. Default is ggplot2::theme_minimal.
#' @param x_axis_label Label for the x axis.
#' @param plot_palette Character vector of hex codes specifying the colors to use on the plot.
#' @param plot_palette_generator Palette from the viridis package used in case plot_palette is unspecified or insufficient for the number of colors required.
#'
#' @export
#' @return A plotly-ized version of a grouped ggplot density plot.
#'
#' @examples
make_density <- function(dt,
                         value,
                         groups = NULL,
                         faceted = TRUE,
                         ggtheme = 'minimal',
                         x_axis_label = NULL,
                         plot_palette = NULL,
                         plot_palette_generator = 'plasma'){

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
    warning('Insufficient palette length provided for a density plot of ',
            value, if(!is.null(groups)){paste(' by', groups)},
            '. Adding the missing ', (plot_palette_length - length(plot_palette)),
            ' colors from plot_palette_generator')
    plot_palette <- c(plot_palette,
                      plot_palette_generator(plot_palette_length - length(plot_palette), begin = 0, end = .8))
  }

  # create the plot structure depending of the group
  if(is.null(groups)){
    # make a dummy group variable
    groups <- 'groups'
    dt$groups <- 'A'

    density <- ggplot2::ggplot(dt,
                               ggplot2::aes_string(x = value,
                                                   fill = groups,
                                                   color = groups)) +
      ggplot2::geom_density(alpha = 0.7) +
      ggplot2::theme(legend.position = 'none',
                     axis.title.x = ggplot2::element_blank(),
                     axis.text.x = ggplot2::element_blank(),
                     axis.ticks.x = ggplot2::element_blank())

  }else{
    density <- ggplot2::ggplot(dt,
                               ggplot2::aes_string(x = value,
                                                   fill = groups,
                                                   color = groups)) +
      ggplot2::scale_fill_manual(values = plot_palette) +
      ggplot2::scale_color_manual(values = plot_palette) +
      ggplot2::geom_density(alpha = 0.7)
  }

  # add theme and y axis number format
  density <- density +
    ggtheme() +
    ggplot2::scale_y_continuous(labels = scales::number_format(accuracy = 0.01,
                                                               decimal.mark = '.',
                                                               big.mark = ','))

  # axes
  if(!is.null(x_axis_label)){
    density <- density + ggplot2::xlab(x_axis_label)
  }

  # facet by groups
  if(faceted){
    density <- density + facet_wrap(as.formula(paste(groups, '~ .')))
  }

  density <- plotly::ggplotly(density,  tooltip = c('x', if(groups != 'groups'){'fill'}))
  return(density)
}



#' Add a density plot to a chronicle report
#'
#' @param report Character string containing the text of an Rmarkdown report header (and possibly more chunks). Easily create one with chronicle::new_report(), and if NULL, that will be the default value.
#' @param dt data.frame containing the data to plot.
#' @param value Name of the column to use as values on the y axis of the plot.
#' @param groups Name of the column containing the different groups.
#' @param faceted If TRUE (defualt), each group will be plotted separatedly.
#' @param ggtheme ggplot2 theme function to apply. Default is ggplot2::theme_minimal.
#' @param x_axis_label Label for the x axis.
#' @param plot_palette Character vector of hex codes specifying the colors to use on the plot.
#' @param plot_palette_generator Palette from the viridis package used in case plot_palette is unspecified or insufficient for the number of colors required.
#' @param density_title Title of the density plot  section on the report. If NULL, chronicle will try to parse a generic title using make_title()
#' @param title_level Level of the section title of this plot (ie, number of # on Rmarkdown syntax.)
#' @param echo Whether to display the source code in the output document. Default is FALSE.
#' @param message Whether to preserve messages on rendering. Default is FALSE.
#' @param warning Whether to preserve warnings on rendering. Default is FALSE.
#' @param fig_width Width of the plot (in inches).
#' @param fig_height Height of the plot (in inches).
#'
#' @return An rmarkdown chunk as a character string, now containing a chunk for adding the density plot.
#' @export
#'
#' @examples
add_density <- function(report = new_report(),
                        dt,
                        value,
                        groups = NULL,
                        faceted = TRUE,
                        ggtheme = NULL,
                        x_axis_label = NULL,
                        plot_palette = NULL,
                        plot_palette_generator = NULL,
                        density_title = NULL,
                        title_level = 2,
                        echo = FALSE,
                        message = FALSE,
                        warning = FALSE,
                        fig_width = NULL,
                        fig_height = NULL){

  params <- list(value = value,
                 groups = groups,
                 faceted = faceted,
                 ggtheme = ggtheme,
                 x_axis_label = x_axis_label,
                 plot_palette = plot_palette,
                 plot_palette_generator = plot_palette_generator) %>%
    purrr::discard(is.null)

  report <- chronicle::add_chunk(report = report,
                                 dt_expr = ifelse(test = is.character(dt),
                                                  yes = dt,
                                                  no = deparse(substitute(dt))),
                                 fun = make_density,
                                 params = params,
                                 chunk_title = density_title,
                                 title_level = title_level,
                                 echo = echo,
                                 message = message,
                                 warning = warning,
                                 fig_width = fig_width,
                                 fig_height = fig_height)
  return(report)
}

