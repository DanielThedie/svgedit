# svgedit

svgedit allows creating figures with complex layout programmatically in R, by using an SVG file as a template.

## Installation

You can install svgedit from CRAN with:

```r
install.packages("svgedit")
```

## Workflow

There are four main steps to use svgedit:

1. Create an SVG template file using the [Inkscape](https://inkscape.org/) graphics editor
2. Create your figure layout in the SVG file, including:
    - Rectangles for ggplot2 plots and images (they will be replaced by your plots and images)
    - Text elements containing the `{}` placeholder for dynamic text (a single text box can contain multiple `{}` placeholders)
    - Any "static" elements you like (text, images, shapes...)
3. Open Inkscape's "Layers and Objects" (Ctrl + Shift + L), and click on your placeholder elements to set their labels
4. In R, use svgedit to replace the placeholders with your plots, images, and text

## Example

See the package vignette by running `vignette("multi-panel-figure", package = "svgedit")` for an example use.
