#' Replace an svg object by a ggplot2 graph
#'
#' @param input_svg Path to the input svg file
#' @param output_svg Path to the output svg file
#' @param plots A named list of ggplot2 objects. The names should correspond to
#'   the labels of the svg elements to be replaced.
#' @param text A named list of character vectors. The names should correspond to
#'   the labels of the svg text elements to be modified. Each character vector
#'   will be used to replace "{}" placeholders in the text element in order.
#' @param dpi The resolution to use when rendering the ggplot2 objects.
#' @export
draw <- function(input_svg, output_svg, plots = NULL, text = NULL, dpi = 150) {
  doc <- xml2::read_xml(input_svg)
  doc_unit <- get_doc_unit(doc)

  for (label in names(plots)) {
    target <- find_element(doc, label)
    target_dim <- get_element_dimensions(target, doc_unit, dpi)

    plot_path <- tempfile(fileext = ".svg")
    ggplot2::ggsave(
      filename = plot_path,
      plot = plots[[label]],
      width = unit_to_inch(target_dim$width, doc_unit, dpi),
      height = unit_to_inch(target_dim$height, doc_unit, dpi),
      dpi = dpi
    )

    doc <- insert_svg(doc, label, plot_path, dpi)
  }

  for (label in names(text)) {
    doc <- insert_text(doc, label, text[[label]])
  }

  svg_text <- as.character(doc)
  svg_text_clean <- gsub(">\\s+<", "><", svg_text)
  writeLines(svg_text_clean, output_svg)
}
