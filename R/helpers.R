find_element <- function(doc, id) {
  target_xpath <- paste0(".//*[@id='", id, "']")
  target <- xml2::xml_find_first(doc, target_xpath, ns = c(svg = "http://www.w3.org/2000/svg"))
  if (is.na(target)) {
    cli::cli_abort(c(
      "x" = "No element with id {id} found in the SVG document.",
      "i" = "Please check that the id is correct and exists in the SVG.",
      "i" = "Use svgedit::get_ids() to list all ids in the SVG file."
    ))
  }
  target
}

get_element_dimensions <- function(element, doc_unit, dpi = 150, call = rlang::caller_env()) {
  x <- as.numeric(gsub("[^0-9.]", "", xml2::xml_attr(element, "x")))
  y <- as.numeric(gsub("[^0-9.]", "", xml2::xml_attr(element, "y")))
  w <- as.numeric(gsub("[^0-9.]", "", xml2::xml_attr(element, "width")))
  h <- as.numeric(gsub("[^0-9.]", "", xml2::xml_attr(element, "height")))
  if (is.na(w) || is.na(h)) {
    cli::cli_abort(c(
      "x" = "SVG element does not have valid width and/or height attributes."
    ), call = call)
  }

  list(
    x = x,
    y = y,
    width = w, doc_unit, dpi,
    height = h, doc_unit, dpi
  )
}

add_plot <- function(target, doc, plot_file, dpi = 150) {
  svg_ns <- "http://www.w3.org/2000/svg"
  svg_root <- xml2::xml_root(doc)

  plot_doc <- xml2::read_xml(plot_file)
  plot_root <- xml2::xml_root(plot_doc)

  doc_unit <- get_doc_unit(doc)
  target_dim <- get_element_dimensions(target, doc_unit, dpi)

  plot_width <- as.numeric(gsub("[^0-9.]", "", xml2::xml_attr(plot_root, "width")))
  plot_height <- as.numeric(gsub("[^0-9.]", "", xml2::xml_attr(plot_root, "height")))

  plot_group <- xml2::xml_new_root("g", ns = svg_ns)
  xml2::xml_set_attr(plot_group, "transform",
    sprintf("translate(%f,%f) scale(%f,%f)",
            target_dim$x,
            target_dim$y,
            target_dim$height / plot_height,
            target_dim$width / plot_width)
  )

  bg_rect <- xml2::xml_find_first(plot_root, ".//svg:rect", ns = c(svg = svg_ns))
  if (!is.na(bg_rect)) xml2::xml_remove(bg_rect)

  for (child in xml2::xml_children(plot_root)) {
    xml2::xml_add_child(plot_group, child)
  }

  xml2::xml_remove(target)
  xml2::xml_set_attr(plot_group, "id", xml2::xml_attr(target, "id"))
  xml2::xml_add_child(svg_root, plot_group)
  doc
}

get_doc_unit <- function(doc) {
  svg_root <- xml2::xml_root(doc)
  svg_width_attr <- xml2::xml_attr(svg_root, "width")
  unit_match <- regmatches(svg_width_attr, regexpr("[a-zA-Z]+$", svg_width_attr))
  if (length(unit_match) == 1) unit_match else "px"
}

unit_to_inch <- function(val, unit, dpi = 150) {
  switch(unit,
    "mm" = val / 25.4,
    "cm" = val / 2.54,
    "in" = val,
    "px" = val / dpi,
    val
  )
}
