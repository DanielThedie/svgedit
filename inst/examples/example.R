library(palmerpenguins)
library(ggplot2)

data("penguins")

# Number of penguins per species/island
penguins_count <- ggplot(penguins, aes(x = island, fill = species)) +
  geom_bar(alpha = 0.8) +
  scale_fill_manual(values = c("darkorange", "purple", "cyan4"), 
                    guide = "none") +
  theme_minimal() +
  facet_wrap(~species, ncol = 1) +
  coord_flip()

# Flipper length distribution by species
flipper_length <- ggplot(data = penguins, aes(x = flipper_length_mm)) +
  geom_histogram(aes(fill = species), alpha = 0.5, position = "identity") +
  scale_fill_manual(values = c("darkorange", "darkorchid", "cyan4"))

# Body mass vs flipper length colored by species
body_mass_vs_flipper_length <- ggplot(
  data = penguins,
  aes(x = flipper_length_mm, y = body_mass_g)
) +
  geom_point(aes(color = species, 
                 shape = species),
             size = 2) +
  scale_color_manual(values = c("darkorange", "darkorchid", "cyan4"))

svgedit::draw(
  input_svg = system.file("examples", "Template.svg", package = "svgedit"),
  output_svg = paste0(system.file("examples", package = "svgedit"),
                      "/penguins_figure.svg"),
  plots = list(
    panel_A = penguins_count,
    panel_B = body_mass_vs_flipper_length,
    panel_C = flipper_length
  ),
  plot_scale = list(
    panel_A = 1,
    panel_B = 1,
    panel_C = 1
  ),
  text = list(
    fig_caption = c(
      "1",
      "Number of penguins of each species per island",
      "Body mass vs flipper length colored by species",
      "Flipper length distribution by species",
      "Photo of an Adelie Penguin (Diego Tirira, CC BY-SA 2.0)"
    )
  ),
  images = list(
    panel_D = system.file("examples", "adelie_penguin.jpeg", package = "svgedit")
  )
)
