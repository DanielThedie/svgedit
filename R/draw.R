#' Replace an svg object by a ggplot2 graph
#'
#' @param input_svg Path to the input svg file
#' @param output_svg Path to the output svg file
#' @param plots A named list of ggplot2 objects. The list names should correspond to
#'   the labels of the svg elements to be replaced.
#' @param plot_scale A named list of numeric values to scale the inserted plots.
#' The names should correspond to the labels of the svg elements to be replaced.
#' @param text A named list of character vectors. The list names should correspond to
#'   the labels of the svg text elements to be modified. Each character vector
#'   will be used to replace "{}" placeholders in the text element in order.
#' @param images A named list of paths to image files. The list names should correspond to
#'   the labels of the svg image elements to be replaced.
#' @param dpi The resolution to use when rendering the ggplot2 objects.
#' @export
draw <- function(
  input_svg,
  output_svg,
  plots = NULL,
  plot_scale = NULL,
  text = NULL,
  images = NULL,
  dpi = 150
) {
  doc <- xml2::read_xml(input_svg)
  doc_unit <- get_doc_unit(doc)

  # Insert plots
  for (label in names(plots)) {
    target <- find_element(doc, label)
    target_dim <- get_element_dimensions(target, doc_unit, dpi)
    scale <- plot_scale[[label]] %||% 1

    plot_path <- tempfile(fileext = ".svg")
    ggplot2::ggsave(
      filename = plot_path,
      plot = plots[[label]],
      width = unit_to_inch(target_dim$width, doc_unit, dpi) * scale,
      height = unit_to_inch(target_dim$height, doc_unit, dpi) * scale,
      dpi = dpi
    )

    doc <- insert_svg(doc, label, plot_path, dpi)
  }

  # Insert text
  for (label in names(text)) {
    doc <- insert_text(doc, label, text[[label]])
  }

  # Insert images
  for (label in names(images)) {
    doc <- insert_image(doc, label, images[[label]], dpi)
  }

  svg_text <- as.character(doc)
  svg_text_clean <- gsub(">\\s+<", "><", svg_text)
  writeLines(svg_text_clean, output_svg)
}
