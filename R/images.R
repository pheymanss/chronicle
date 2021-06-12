#' Add an image to a chronicle Rmarkdown report
#'
#' @param report Character string containing all the R Markdown chunks previously added. Default is '', an empty report.
#' @param image_path The path to the image that will be added to the report.
#' @param image_caption A caption to be printed for the image.
#' @param image_title The title of the text section. Default is NULL.
#' @param title_level Level of the section title of this text (ie, number of # on Rmarkdown syntax.)
#' @param fig_width Width of the figures printed from this code.
#' @param fig_height Height of the figures printed from this code.
#'
#' @return The text of the Rmarkdown report plus an additional section with the text.
#' @export
#'
#' @examples
#'
#' library(chronicle)
#' report <- add_image(image_path = 'readme1.png',
#'                     image_caption = 'This is the caption of the image',
#'                     image_title = 'This is the image that I want to include')
add_image <- function(report = '',
                      image_path,
                      image_caption = NULL,
                      image_title = NULL,
                      title_level = 2,
                      fig_width = NULL,
                      fig_height = NULL){
  if(!is.null(image_title)){
    report <- chronicle::add_title(report, image_title, title_level = title_level)
  }

  if(is.null(image_caption)){
    warning('Consider adding figure captions to your images to make them accesible to people with visual impairements.
Just add the parameter "image_caption" to your add_image() calls.')
  }

  open_chunk <- paste0('```{r, echo=FALSE,',
                       if(!is.null(image_caption)){paste0(', fig.cap="',image_caption, '"')},
                       if(!is.null(fig_width)){paste(', fig.width=', fig_width)}else{', fig.width=params$figure_width'},
                       if(!is.null(fig_width)){paste(', fig.height=', fig_height)}else{', fig.height=params$figure_height'},
                       '}')

  report <- glue::glue('{report}

{open_chunk}
knitr::include_graphics("{image_path}")
```')
  return(report)
}
