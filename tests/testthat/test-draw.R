test_that("draw replaces text placeholders and inserts plot", {
  # SVG template with text and a panel
  svg_txt <- '
  <svg xmlns="http://www.w3.org/2000/svg"
       xmlns:inkscape="http://www.inkscape.org/namespaces/inkscape">
    <text inkscape:label="caption">
      <tspan>Figure {}</tspan>
      <tspan>: {}</tspan>
    </text>
    <rect inkscape:label="panel_A" x="1" y="2" width="10" height="20"/>
  </svg>'
  template_file <- tempfile(fileext = ".svg")
  writeLines(svg_txt, template_file)

  # ggplot to insert
  plot <- ggplot2::ggplot(palmerpenguins::penguins,
                 ggplot2::aes(flipper_length_mm, body_mass_g)) +
          ggplot2::geom_point()

  output_file <- tempfile(fileext = ".svg")

  expect_warning(
    svgedit::draw(
      input_svg = template_file,
      output_svg = output_file,
      plots = list(panel_A = plot),
      text = list(caption = c("1", "Test"))
    )
  )

  out_doc <- xml2::read_xml(output_file)
  tspans <- xml2::xml_find_all(out_doc,
                               ".//svg:tspan",
                               ns = inkscape_ns)
  expect_equal(xml2::xml_text(tspans[1]), "Figure 1")
  expect_equal(xml2::xml_text(tspans[2]), ": Test")

  group <- xml2::xml_find_first(out_doc, ".//svg:g", ns = inkscape_ns)
  expect_equal(xml2::xml_attr(group, "inkscape:label", ns = inkscape_ns), "panel_A")
  expect_true(!is.na(xml2::xml_attr(group, "transform")))
})
