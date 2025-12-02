#' Add an element to an SVG document, replacing a target element
#'
#' @param doc An xml2 SVG document
#' @param label The label of the target element to be replaced
#' @param svg_file Path to the SVG file to be inserted
#' @param dpi The resolution to use when interpreting pixel units
#' @return The modified xml2 SVG document (doc) with the svg file added and the target removed
#' @export
insert_svg <- function(doc, label, insert_file, dpi = 150) {
  doc_unit <- get_doc_unit(doc)
  target <- find_element(doc, label)
  target_dim <- get_element_dimensions(target, doc_unit, dpi)

  insert_root <- xml2::xml_root(xml2::read_xml(insert_file))

  insert_width <- as.numeric(gsub("[^0-9.]", "", xml2::xml_attr(insert_root, "width")))
  insert_height <- as.numeric(gsub("[^0-9.]", "", xml2::xml_attr(insert_root, "height")))

  insert_group <- xml2::xml_new_root("g", ns = inkscape_ns)
  xml2::xml_set_attr(insert_group, "transform",
    sprintf("translate(%f,%f) scale(%f,%f)",
            target_dim$x,
            target_dim$y,
            target_dim$height / insert_height,
            target_dim$width / insert_width)
  )

  bg_rect <- xml2::xml_find_first(insert_root, ".//svg:rect", ns = inkscape_ns)
  if (!is.na(bg_rect)) xml2::xml_remove(bg_rect)

  for (child in xml2::xml_children(insert_root)) {
    xml2::xml_add_child(insert_group, child)
  }

  xml2::xml_set_attr(insert_group, "inkscape:label", xml2::xml_attr(target, "inkscape:label", ns = inkscape_ns))
  xml2::xml_add_child(xml2::xml_parent(target), insert_group)
  xml2::xml_remove(target)
  doc
}

#' Replace "{}" in a text element by provided values
#'
#' @param doc An SVG document
#' @param label The label of the text element to edit
#' @param values A character vector to replace each "{}" in order
#' @return The modified SVG document (doc) with the text inserted
#' @export
insert_text <- function(doc, label, values) {
  text_node <- find_element(doc, label)
  tspans <- xml2::xml_find_all(text_node, ".//svg:tspan", ns = inkscape_ns)
  value_idx <- 1

  for (tspan in tspans) {
    contents <- xml2::xml_contents(tspan)
    text_nodes <- contents[xml2::xml_type(contents) == "text"]
    for (text_node_i in text_nodes) {
      tspan_text <- xml2::xml_text(text_node_i)
      while (grepl("\\{\\}", tspan_text) && value_idx <= length(values)) {
        tspan_text <- sub("\\{\\}", values[[value_idx]], tspan_text)
        value_idx <- value_idx + 1
      }
      xml2::xml_set_text(text_node_i, tspan_text)
    }
  }
  doc
}
