library(palmerpenguins)
library(ggplot2)

data("penguins")

# Number of penguins per species/island
penguins_count <- ggplot(penguins, aes(x = island, fill = species)) +
  geom_bar(alpha = 0.8) +
  scale_fill_manual(values = c("darkorange","purple","cyan4"), 
                    guide = "none") +
  theme_minimal() +
  facet_wrap(~species, ncol = 1) +
  coord_flip()

# Flipper length distribution by species
flipper_length <- ggplot(data = penguins, aes(x = flipper_length_mm)) +
  geom_histogram(aes(fill = species), alpha = 0.5, position = "identity") +
  scale_fill_manual(values = c("darkorange","darkorchid","cyan4"))

# Body mass vs flipper length colored by species
body_mass_vs_flipper_length <- ggplot(data = penguins, aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point(aes(color = species, 
                 shape = species),
             size = 2) +
  scale_color_manual(values = c("darkorange","darkorchid","cyan4"))

svgedit::draw(
  input_svg = system.file("examples", "Template2.svg", package = "svgedit"),
  output_svg = paste0(system.file("examples", package = "svgedit"), "/penguins_figure.svg"),
  plots = list(
      panel_A = penguins_count,
      panel_B = body_mass_vs_flipper_length,
      panel_C = flipper_length
  )
)
