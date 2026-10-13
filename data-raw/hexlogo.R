library(ggplot2)
library(grid)
library(insperplot)

colors <- list(
  bricks = insper_palette("cidades_tijolo")[4:7],
  mortar = "#E8DCD0",
  label = "#000000",
  border = "#000000",
  text = "#ffffff"
)

# Hexagon -----------------------------------------------------------------

# Pointy-top hexagon centered at (0, 0) with radius 1
angles <- seq(90, 390, by = 60)[1:6] * pi / 180
hex <- data.frame(x = cos(angles), y = sin(angles))

# Brick wall --------------------------------------------------------------

brick_w <- 0.36
brick_h <- 0.16
rows <- seq(-1.1, 1.1, by = brick_h)

set.seed(42)
bricks <- do.call(
  rbind,
  lapply(seq_along(rows), \(i) {
    # Offset every other row by half a brick
    offset <- if (i %% 2 == 0) brick_w / 2 else 0
    data.frame(x = seq(-1.2, 1.2, by = brick_w) + offset, y = rows[i])
  })
)
bricks$fill <- sample(colors$bricks, nrow(bricks), replace = TRUE)

p <- ggplot(bricks, aes(x, y)) +
  geom_tile(
    aes(fill = fill),
    width = brick_w * 0.92,
    height = brick_h * 0.82
  ) +
  geom_label(
    data = data.frame(x = 0, y = -0.15),
    aes(label = "utilscidados"),
    family = "Inter 24pt",
    fontface = "bold",
    size = 12,
    fill = colors$label,
    color = colors$text,
    linewidth = 0,
    label.padding = unit(0.35, "lines"),
    label.r = unit(0.1, "lines")
  ) +
  scale_fill_identity() +
  coord_fixed(xlim = c(-1, 1), ylim = c(-1, 1), expand = FALSE) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = colors$mortar, color = NA),
    plot.margin = margin(0, 0, 0, 0)
  )

# Render ------------------------------------------------------------------

# Draw the plot masked to the hexagon, then the border on top. Masks
# need R >= 4.1 and a device that supports them (ragg does).
hex_grob <- \(...) {
  polygonGrob(
    x = unit(0.5 + hex$x / 2, "npc"),
    y = unit(0.5 + hex$y / 2, "npc"),
    ...
  )
}

out <- "man/figures/logo.png"
dir.create(dirname(out), showWarnings = FALSE, recursive = TRUE)

ragg::agg_png(
  out,
  width = 518,
  height = 600,
  res = 144,
  background = "transparent"
)
# Square viewport spanning the hexagon height; the hexagon width (sqrt(3))
# fits inside the 518 px device width
pushViewport(viewport(
  width = unit(290, "points"),
  height = unit(290, "points")
))
pushViewport(viewport(mask = hex_grob(gp = gpar(fill = "black"))))
grid.draw(ggplotGrob(p))
popViewport()
grid.draw(hex_grob(gp = gpar(fill = NA, col = colors$border, lwd = 8)))
popViewport()
dev.off()
