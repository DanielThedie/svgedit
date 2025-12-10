#' Add an element to an SVG document, replacing a target element
#'
#' @param doc An SVG document
#' @param label The label of the target element to be replaced
#' @param insert_file Path to the SVG file to be inserted
#' @param dpi The resolution to use when interpreting pixel units
#' @return The modified SVG document (doc) with the svg file added and the target removed
insert_svg <- function(doc, label, insert_file, dpi = 150) {
  doc_unit <- get_doc_unit(doc)
  target <- find_element(doc, label)
  target_dim <- get_element_dimensions(target, doc_unit, dpi)

  insert_root <- xml2::xml_root(xml2::read_xml(insert_file))

  insert_width <- as.numeric(gsub("[^0-9.]", "", xml2::xml_attr(insert_root, "width")))
  insert_height <- as.numeric(gsub("[^0-9.]", "", xml2::xml_attr(insert_root, "height")))

  insert_group <- xml2::xml_new_root("g")
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
  xml2::xml_add_sibling(target, insert_group, .where = "after")
  xml2::xml_remove(target)
  doc
}

#' Replace "{}" in a text element by provided values
#'
#' @param doc An SVG document
#' @param label The label of the text element to edit
#' @param values A character vector to replace each "{}" in order
#' @return The modified SVG document (doc) with the text inserted
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

#' Insert a raster image (PNG/JPG) into an SVG document, replacing a target element
#'
#' @param doc An SVG document
#' @param label The label of the target element to be replaced
#' @param image_file Path to the PNG or JPG image to be inserted
#' @param dpi The resolution to use when interpreting pixel units (in the template svg)
#' @return The modified SVG document (doc) with the image added and the target removed
insert_image <- function(doc, label, image_file, dpi = 150) {
  if (!file.exists(image_file)) {
    cli::cli_abort("Could not find image file {image_file}.")
  }
  doc_unit <- get_doc_unit(doc)
  target <- find_element(doc, label)
  target_dim <- get_element_dimensions(target, doc_unit, dpi)

  img_ext <- tools::file_ext(image_file)
  mime_type <- if (img_ext == "png") "image/png" else "image/jpeg"
  img_data <- base64enc::dataURI(file = image_file, mime = mime_type)

  xml2::xml_set_attr(xml2::xml_root(doc), "xmlns:xlink", "http://www.w3.org/1999/xlink")

  image_node <- xml2::xml_new_root("image")
  xml2::xml_set_attr(image_node, "x", target_dim$x)
  xml2::xml_set_attr(image_node, "y", target_dim$y)
  xml2::xml_set_attr(image_node, "width", target_dim$width)
  xml2::xml_set_attr(image_node, "height", target_dim$height)
  xml2::xml_set_attr(image_node, "xlink:href", img_data)
  xml2::xml_set_attr(image_node, "inkscape:label", xml2::xml_attr(target, "inkscape:label", ns = inkscape_ns))

  xml2::xml_add_sibling(target, image_node, .where = "after")
  xml2::xml_remove(target)
  doc
}
