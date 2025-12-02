test_that("insert_text replaces {} in tspans by label", {
  svg_txt <- '
  <svg xmlns="http://www.w3.org/2000/svg"
       xmlns:inkscape="http://www.inkscape.org/namespaces/inkscape">
    <text inkscape:label="caption">
      <tspan>Figure {}</tspan>
      <tspan>: {}</tspan>
    </text>
  </svg>'
  doc <- xml2::read_xml(svg_txt)
  doc <- insert_text(doc, "caption", c("1", "Penguins"))
  tspans <- xml2::xml_find_all(doc, ".//svg:tspan", ns = inkscape_ns)
  expect_equal(xml2::xml_text(tspans[1]), "Figure 1")
  expect_equal(xml2::xml_text(tspans[2]), ": Penguins")
})

test_that("insert_svg adds group with correct label and transform", {
  # Figure to be inserted (just a circle in this case)
  insert_svg_txt <- '
  <svg xmlns="http://www.w3.org/2000/svg" width="10" height="20">
    <circle cx="5" cy="10" r="5"/>
  </svg>'
  insert_file <- tempfile(fileext = ".svg")
  writeLines(insert_svg_txt, insert_file)

  # Template svg with a rectangle as target
  svg_txt <- '
  <svg xmlns="http://www.w3.org/2000/svg"
       xmlns:inkscape="http://www.inkscape.org/namespaces/inkscape">
    <rect id="rect1" inkscape:label="panel_A" x="1" y="2" width="10" height="20"/>
  </svg>'
  doc <- xml2::read_xml(svg_txt)
  doc <- insert_svg(doc, "panel_A", insert_file)
  group <- xml2::xml_find_first(doc, ".//g", ns = inkscape_ns)
  expect_equal(xml2::xml_attr(group, "inkscape:label"), "panel_A")
  expect_true(!is.na(xml2::xml_attr(group, "transform")))
  rect <- xml2::xml_find_first(doc, ".//svg:rect", ns = inkscape_ns)
  expect_true(is.na(rect))
})
