#' Find an element by label in an Inkscape SVG document
#'
#' @param doc An xml2 SVG document
#' @param id The label of the element to find
#' @return The xml2 node corresponding to the element with the given id
find_element <- function(doc, label) {
  target_xpath <- paste0(".//*[@inkscape:label='", label, "']")
  target <- xml2::xml_find_first(doc, target_xpath, ns = inkscape_ns)
  if (is.na(target)) {
    cli::cli_abort(c(
      "x" = "No element with label {label} found in the SVG document.",
      "i" = "Please check that the label is correct and exists in the SVG."
    ))
  }
  target
}

#' Get the dimensions of an SVG element
#'
#' @details The function expects the element to have 'x', 'y', 'width', and 'height' attributes.
#' @param element An xml2 node corresponding to an SVG element
#' @param doc_unit The unit used in the SVG document (e.g., "px", "mm", "cm", "in")
#' @param dpi The resolution to use when interpreting pixel units
#' @param call The calling environment for error reporting
#' @return A list with x, y, width, and height of the element
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
    width = w,
    height = h
  )
}

#' Get the unit used in the SVG document
#'
#' @param doc An xml2 SVG document
#' @return The unit used in the SVG document (e.g., "px", "mm", "cm", "in")
get_doc_unit <- function(doc) {
  svg_root <- xml2::xml_root(doc)
  svg_width_attr <- xml2::xml_attr(svg_root, "width")
  unit_match <- regmatches(svg_width_attr, regexpr("[a-zA-Z]+$", svg_width_attr))
  if (length(unit_match) == 1) unit_match else "px"
}

#' Convert a measurement to inches based on the unit
#'
#' @param val The measurement value
#' @param unit The unit of the measurement (e.g., "px", "mm", "cm", "in")
#' @param dpi The resolution to use when interpreting pixel units
#' @return The measurement converted to inches
unit_to_inch <- function(val, unit, dpi = 150) {
  switch(unit,
    "mm" = val / 25.4,
    "cm" = val / 2.54,
    "in" = val,
    "px" = val / dpi,
    val
  )
}
