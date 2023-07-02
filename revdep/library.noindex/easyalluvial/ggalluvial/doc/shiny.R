## ----setup, echo = FALSE, message = FALSE, warning = FALSE--------------------
knitr::opts_chunk$set(fig.width = 6, fig.height = 3, fig.align = "center")
library(ggalluvial)
pdf(NULL)

## ----run wide app locally, eval = FALSE---------------------------------------
#  shiny::shinyAppDir(system.file("examples/ex-shiny-wide-data", package="ggalluvial"))

## ----pseudocode, eval = FALSE-------------------------------------------------
#  
#  '<(1) Load data.>'
#  
#  '<(2) Create "ggplot" object for alluvial plot and build it.>'
#  
#  '<(3) Extract data from built plot object used to create alluvium polygons.>'
#  
#  for (polygon in polygons) {
#       '<(4) Use polygon splines to generate coordinates of alluvium boundaries.>'
#  }
#  
#  '<(5) Define range of coordinates in grid units and plot units.>'
#  
#  for (polygon in polygons) {
#       '<(6) Convert coordinates from grid units to plot units.>'
#  }
#  
#  ui <- fluidPage(
#       '<(7) Output plot with hovering enabled.>'
#  
#       '<(8) Output tooltip.>'
#  )
#  
#  
#  server <- function(input, output, session) {
#  
#    output$alluvial_plot <- renderPlot({
#      '<(9) Render the plot.>'
#    })
#  
#    output$tooltip <- renderText({
#      if ('<(10) mouse cursor is within the plot panel>') {
#        if ('<(11) mouse cursor is within a stratum box>') {
#          '<(11b) Render stratum tooltip.>'
#        } else {
#          if ('<(12) mouse cursor is within an alluvium polygon>') {
#            '<(12b) Render alluvium tooltip.>'
#          }
#        }
#      }
#    })
#  
#  }

## ----load dataset, eval = FALSE-----------------------------------------------
#  data(UCBAdmissions)
#  ucb_admissions <- as.data.frame(UCBAdmissions)

## ----set options, eval = FALSE------------------------------------------------
#  # Offset, in pixels, for location of tooltip relative to mouse cursor,
#  # in both x and y direction.
#  offset <- 5
#  # Width of node boxes
#  node_width <- 1/4
#  # Width of alluvia
#  alluvium_width <- 1/3

## ----draw and build plot, eval = FALSE----------------------------------------
#  # Draw plot.
#  p <- ggplot(ucb_admissions,
#              aes(y = Freq, axis1 = Gender, axis2 = Dept)) +
#    geom_alluvium(aes(fill = Admit), knot.pos = 1/4, width = alluvium_width) +
#    geom_stratum(width = node_width, reverse = TRUE, fill = 'black', color = 'grey') +
#    geom_label(aes(label = after_stat(stratum)),
#               stat = "stratum",
#               reverse = TRUE,
#               size = rel(2)) +
#    theme_bw() +
#    scale_fill_brewer(type = "qual", palette = "Set1") +
#    scale_x_discrete(limits = c("Gender", "Dept"), expand = c(.05, .05)) +
#    scale_y_continuous(expand = c(0, 0)) +
#    ggtitle("UC Berkeley admissions and rejections", "by sex and department") +
#    theme(plot.title = element_text(size = rel(1)),
#          plot.subtitle = element_text(size = rel(1)),
#          legend.position = 'bottom')
#  
#  # Build the plot.
#  pbuilt <- ggplot_build(p)

## ----get xsplines and draw curves, eval = FALSE-------------------------------
#  # Add width parameter, and then convert built plot data to xsplines
#  data_draw <- transform(pbuilt$data[[1]], width = alluvium_width)
#  groups_to_draw <- split(data_draw, data_draw$group)
#  group_xsplines <- lapply(groups_to_draw,
#                           data_to_alluvium)
#  
#  # Convert xspline coordinates to grid object.
#  xspline_coords <- lapply(
#    group_xsplines,
#    function(coords) grid::xsplineGrob(x = coords$x,
#                                       y = coords$y,
#                                       shape = coords$shape,
#                                       open = FALSE)
#  )
#  
#  # Use grid::xsplinePoints to draw the curve for each polygon
#  xspline_points <- lapply(xspline_coords, grid::xsplinePoints)

## ----get coordinate ranges, eval = FALSE--------------------------------------
#  # Define the x and y axis limits in grid coordinates (old) and plot
#  # coordinates (new)
#  xrange_old <- range(unlist(lapply(
#    xspline_points,
#    function(pts) as.numeric(pts$x)
#  )))
#  yrange_old <- range(unlist(lapply(
#    xspline_points,
#    function(pts) as.numeric(pts$y)
#  )))
#  xrange_new <- c(1 - alluvium_width/2, max(pbuilt$data[[1]]$x) + alluvium_width/2)
#  yrange_new <- c(0, sum(pbuilt$data[[2]]$count[pbuilt$data[[2]]$x == 1]))

## ----transform coordinates, eval = FALSE--------------------------------------
#  # Define function to convert grid graphics coordinates to data coordinates
#  new_range_transform <- function(x_old, range_old, range_new) {
#    (x_old - range_old[1])/(range_old[2] - range_old[1]) *
#      (range_new[2] - range_new[1]) + range_new[1]
#  }
#  
#  # Using the x and y limits, convert the grid coordinates into plot coordinates.
#  polygon_coords <- lapply(xspline_points, function(pts) {
#    x_trans <- new_range_transform(x_old = as.numeric(pts$x),
#                                   range_old = xrange_old,
#                                   range_new = xrange_new)
#    y_trans <- new_range_transform(x_old = as.numeric(pts$y),
#                                   range_old = yrange_old,
#                                   range_new = yrange_new)
#    list(x = x_trans, y = y_trans)
#  })

## ----ui, eval = FALSE---------------------------------------------------------
#  ui <- fluidPage(
#    fluidRow(tags$div(
#      style = "position: relative;",
#      plotOutput("alluvial_plot", height = "650px",
#                 hover = hoverOpts(id = "plot_hover")
#                 ),
#      htmlOutput("tooltip")))
#  )

## ----renderPlot, eval = FALSE-------------------------------------------------
#  output$alluvial_plot <- renderPlot(p, res = 200)

## ---- eval = FALSE------------------------------------------------------------
#  output$tooltip <- renderText(
#    if(!is.null(input$plot_hover)) { ... }
#    ...
#  )

## ---- eval = FALSE------------------------------------------------------------
#  hover <- input$plot_hover
#  x_coord <- round(hover$x)
#  
#  if(abs(hover$x - x_coord) < (node_width / 2)) { ... } else { ... }

## ---- eval = FALSE------------------------------------------------------------
#  node_row <-
#    pbuilt$data[[2]]$x == x_coord & hover$y > pbuilt$data[[2]]$ymin & hover$y < pbuilt$data[[2]]$ymax

## ---- eval = FALSE------------------------------------------------------------
#  node_label <- pbuilt$data[[2]]$stratum[node_row]
#  node_n <- pbuilt$data[[2]]$count[node_row]

## ----render strata tooltip, eval = FALSE--------------------------------------
#  renderTags(
#    tags$div(
#      node_label, tags$br(),
#      "n =", node_n,
#      style = paste0(
#        "position: absolute; ",
#        "top: ", hover$coords_css$y + offset, "px; ",
#        "left: ", hover$coords_css$x + offset, "px; ",
#        "background: gray; ",
#        "padding: 3px; ",
#        "color: white; "
#      )
#    )
#  )$html

## ----test within polygon, eval = FALSE----------------------------------------
#  hover_within_flow <- sapply(
#    polygon_coords,
#    function(pol) point.in.polygon(point.x = hover$x,
#                                   point.y = hover$y,
#                                   pol.x = pol$x,
#                                   pol.y = pol$y)
#  )

## ---- eval = FALSE------------------------------------------------------------
#  if (any(hover_within_flow)) { ... }

## ----info for alluvia tooltip, eval = FALSE-----------------------------------
#  coord_id <- rev(which(hover_within_flow == 1))[1]
#  flow_label <- paste(groups_to_draw[[coord_id]]$stratum, collapse = ' -> ')
#  flow_n <- groups_to_draw[[coord_id]]$count[1]

## ----render alluvia tooltip, eval = FALSE-------------------------------------
#  renderTags(
#    tags$div(
#      flow_label, tags$br(),
#      "n =", flow_n,
#      style = paste0(
#        "position: absolute; ",
#        "top: ", hover$coords_css$y + offset, "px; ",
#        "left: ", hover$coords_css$x + offset, "px; ",
#        "background: gray; ",
#        "padding: 3px; ",
#        "color: white; "
#      )
#    )
#  )$html

## ----run long app locally, eval = FALSE---------------------------------------
#  shiny::shinyAppDir(system.file("examples/ex-shiny-long-data", package="ggalluvial"))

