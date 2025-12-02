test_that("find_element finds element by label", {
  svg_txt <- '
  <svg xmlns="http://www.w3.org/2000/svg"
       xmlns:inkscape="http://www.inkscape.org/namespaces/inkscape">
    <rect id="rect1" inkscape:label="panel_A" x="10" y="20" width="30" height="40"/>
  </svg>'
  doc <- xml2::read_xml(svg_txt)
  element <- find_element(doc, "panel_A")
  expect_equal(xml2::xml_attr(element, "id"), "rect1")
})

test_that("get_element_dimensions returns correct values", {
  svg_txt <- '
  <svg xmlns="http://www.w3.org/2000/svg">
    <rect id="rect1" x="10.4164" y="20.2877" width="30.2998" height="40.0987"/>
  </svg>'
  doc <- xml2::read_xml(svg_txt)
  element <- xml2::xml_find_first(doc, ".//svg:rect", ns = inkscape_ns)
  dims <- get_element_dimensions(element, "mm")
  expect_equal(dims$x, 10.4164)
  expect_equal(dims$y, 20.2877)
  expect_equal(dims$width, 30.2998)
  expect_equal(dims$height, 40.0987)
})

test_that("get_doc_unit extracts unit from SVG width", {
  svg_txt <- '<svg width="100mm" height="50mm"></svg>'
  doc <- xml2::read_xml(svg_txt)
  expect_equal(get_doc_unit(doc), "mm")
})

test_that("unit_to_inch converts units correctly", {
  expect_equal(unit_to_inch(25.4, "mm"), 1)
  expect_equal(unit_to_inch(2.54, "cm"), 1)
  expect_equal(unit_to_inch(1, "in"), 1)
  expect_equal(unit_to_inch(150, "px", dpi = 150), 1)
  expect_equal(unit_to_inch(1, "xxxx"), 1)
  expect_equal(unit_to_inch(1, ""), 1)
})
