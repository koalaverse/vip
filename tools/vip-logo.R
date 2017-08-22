# Load required packages
library(ggimage)
library(ggplot2)
library(grid)
library(png)

# Load vip image
img <- rasterGrob(readPNG("tools\\vip-img.png"), interpolate = TRUE, width = 0.8)

# Hexagon data
hex <- data.frame(x = 1.35 * 1 * c(-sqrt(3) / 2, 0, rep(sqrt(3) / 2, 2), 0,
                                   rep(-sqrt(3) / 2, 2)),
                  y = 1.35 * 1 * c(0.5, 1, 0.5, -0.5, -1, -0.5, 0.5))

# Color palettes
greys <- RColorBrewer::brewer.pal(9, "Greys")
reds <- RColorBrewer::brewer.pal(9, "Reds")

# Hexagon logo
g <- ggplot() +
  geom_polygon(data = hex, aes(x, y), color = reds[6L], fill = greys[1L], size = 3) +
  annotation_custom(img, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
  annotate(geom = "text", label = "vip", x = 0, y = -0.05,
           family = "Open Sans Light", color = greys[7L], size = 7) +
  coord_equal(xlim = range(hex$x), ylim = range(hex$y)) +
  scale_x_continuous(expand = c(0.04, 0)) +
  scale_y_reverse(expand = c(0.04, 0)) +
  theme_void() +
  theme_transparent() +
  theme(axis.ticks.length = unit(0, "mm"))
print(g)

png("tools\\vip-logo.png", width = 181, height = 209, bg = "transparent", type = "cairo-png")
print(g)
dev.off()

svg("tools\\vip-logo.svg", width = 181 / 72, height = 209 / 72, bg = "transparent")
print(g)
dev.off()
