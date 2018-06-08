# Load required packages
library(ggplot2)

# Load vip image
img <- grid::rasterGrob(
  png::readPNG("tools/logo-vip-img.png"), interpolate = TRUE, width = 0.85
)

# Hexagon data
hex <- data.frame(x = 1.35 * 1 * c(-sqrt(3) / 2, 0, rep(sqrt(3) / 2, 2), 0,
                                   rep(-sqrt(3) / 2, 2)),
                  y = 1.35 * 1 * c(0.5, 1, 0.5, -0.5, -1, -0.5, 0.5))

# Color palettes
reds <- RColorBrewer::brewer.pal(9, "Reds")
greys <- RColorBrewer::brewer.pal(9, "Greys")

# Hexagon logo
g <- ggplot() +
  geom_polygon(data = hex, aes(x, y), color = reds[6L], fill = greys[1L],
               size = 3) +
  annotation_custom(img, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
  annotate(geom = "text", label = "vip", x = 0, y = -0.05,
           family = "Open Sans Light", color = greys[9L], size = 7) +
  coord_equal(xlim = range(hex$x), ylim = range(hex$y)) +
  scale_x_continuous(expand = c(0.04, 0)) +
  scale_y_reverse(expand = c(0.04, 0)) +
  theme(axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "none",
        plot.background = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
print(g)

png("tools/logo-vip.png", width = 181, height = 209,
    bg = "transparent", type = "cairo-png")
print(g)
dev.off()

svg("tools/logo-vip.svg", width = 181 / 72, height = 209 / 72,
    bg = "transparent")
print(g)
dev.off()
