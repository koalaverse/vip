# Load required packages
library(ggimage)
library(ggplot2)
library(grid)
library(png)

# Load vip image
img <- rasterGrob(readPNG("tools/vip-img.png"),
                  interpolate = TRUE, width = 1)

koala <- rasterGrob(readPNG("/home/w108bmg/Desktop/suit-and-tie.png"),
                    interpolate = TRUE, width = 1)

# Hexagon data
hex <- data.frame(x = 1.35 * 1 * c(-sqrt(3) / 2, 0, rep(sqrt(3) / 2, 2), 0,
                                   rep(-sqrt(3) / 2, 2)),
                  y = 1.35 * 1 * c(0.5, 1, 0.5, -0.5, -1, -0.5, 0.5))

# Color palettes
reds <- RColorBrewer::brewer.pal(9, "Reds")

# Hexagon logo
g <- ggplot() +
  geom_polygon(data = hex, aes(x, y), color = reds[5L], fill = "white", size = 4) +
  # annotation_custom(img, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
  annotation_custom(koala, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
  # annotation_custom(img, xmin = -1.25, xmax = 1.25, ymin = -1.25, ymax = 1.25) +
  annotate(geom = "text", x = 0, y = 0, color = "white", size = 8,
           label = "vip",
           family = "Open Sans Light") +
  coord_equal(xlim = range(hex$x), ylim = range(hex$y)) +
  scale_x_continuous(expand = c(0.04, 0)) +
  scale_y_reverse(expand = c(0.04, 0)) +
  theme_void() +
  theme_transparent() +
  theme(axis.ticks.length = unit(0, "mm"))
print(g)

# png("tools\\vip-logo.png", width = 181, height = 209, bg = "transparent", type = "cairo-png")
# print(g)
# dev.off()
#
# svg("tools\\vip-logo.svg", width = 181 / 72, height = 209 / 72, bg = "transparent")
# print(g)
# dev.off()
