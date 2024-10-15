# Reproduction of plot huangRelationshipResidentialPatterns2024 (p. 17)
require(grid)
require(here)

draw_diagram <- function() {
  grid.newpage()
  
  # Helper function for text boxes
  draw_text_box <- function(x, y, label, width = NULL, height = NULL, fill = "white", border = "solid", text_color = "black") {
    if (is.null(width)) width <- unit(1, "strwidth", label) + unit(0.2, "inches")
    if (is.null(height)) height <- unit(1, "strheight", label) + unit(0.1, "inches")
    pushViewport(viewport(x = x, y = y, just = "center"))
    grid.rect(width = width, height = height, gp = gpar(fill = fill, lty = border))
    grid.text(label, gp = gpar(fontsize = 8, col = text_color))
    popViewport()
  }
  
  # Helper function for various types of lines and arrows
  draw_connection <- function(x1, y1, x2, y2, type = "solid", arrow = "none", curve = 0, size = 1) {
    if (curve == 0) {
      grid.lines(x = c(x1, x2), y = c(y1, y2), 
                 gp = gpar(lty = type, lwd = size),
                 arrow = if(arrow != "none") arrow(length = unit(0.1 * size, "inches"), ends = arrow) else NULL)
    } else {
      grid.curve(x1, y1, x2, y2, curvature = curve, 
                 gp = gpar(lty = type, lwd = size),
                 arrow = if(arrow != "none") arrow(length = unit(0.1 * size, "inches"), ends = arrow) else NULL)
    }
  }
  
  # Draw main boxes
  draw_text_box(0.5, 0.95, "Marketization of\nurban housing")
  draw_text_box(0.5, 0.05, "Reconstruction of\nurban space")
  draw_text_box(0.5, 0.7, "Spatialization of\nsociety", fill = "lightgrey", border = "dashed")
  draw_text_box(0.5, 0.3, "Socialization of\nspace", fill = "lightgrey", border = "dashed")
  
  # Center boxes
  center_width <- unit(0.13, "npc")
  center_height <- unit(0.15, "npc")
  boxes <- c("Spatial\nreconstrution", "Dwelling\nspace"
             , "housing type\nhousing price\nliving facility\ntraffic location"
             , "age/gender\noccupation\nincome\nlife cycle", "Social\ngroups"
             , "Social\ndifferentiation")
  for (i in 1:6) {
    fill_color <- "white"
    text_color <- if(i == 3 || i == 4) "darkgrey" else "black"
    draw_text_box(0.1 + (i-1)*0.16, 0.5, boxes[i], center_width, center_height
                  , fill = fill_color, text_color = text_color)
  }
  
  # Coupling text
  grid.text("Coupling", x = 0.5, y = 0.5, rot = 90, gp = gpar(fontsize = 8))
  
  # Corner text
  grid.text("Diversified housing\nsupply", x = 0.1, y = 0.8, just = "left", gp = gpar(fontsize = 8))
  grid.text("Personalized\nchoice of housing", x = 0.9, y = 0.8, just = "right", gp = gpar(fontsize = 8))
  grid.text("Differential resource\nallocation", x = 0.1, y = 0.2, just = "left", gp = gpar(fontsize = 8))
  grid.text("Active / Passive\nresidential migration", x = 0.9, y = 0.2, just = "right", gp = gpar(fontsize = 8))
  
  # Arrows and lines
  # Vertical arrows (thicker)
  draw_connection(0.5, 0.9, 0.5, 0.75, type = "solid", arrow = "last", size = 2)
  draw_connection(0.5, 0.1, 0.5, 0.25, type = "solid", arrow = "last", size = 2)
  
  # Arrows between center boxes (thinner)
  for (i in 1:5) {
    draw_connection(0.1 + (i-1)*0.16 + 0.065, 0.5, 0.1 + i*0.16 - 0.065, 0.5, type = "solid", arrow = "both", size = 1)
  }
  
  # Curved dotted lines (thinner)
  for (i in c(0, 5)) {
    draw_connection(0.1 + i*0.16, 0.45, 0.1 + i*0.16 + 0.08, 0.45, type = "dotted", curve = -0.5, size = 1)
    draw_connection(0.1 + i*0.16 + 0.08, 0.45, 0.1 + i*0.16 + 0.16, 0.45, type = "dotted", curve = 0.5, size = 1)
  }
  
  # Dashed arrows from corners to center (medium thickness)
  draw_connection(0.15, 0.75, 0.25, 0.55, type = "dashed", arrow = "last", size = 1.5)
  draw_connection(0.85, 0.75, 0.75, 0.55, type = "dashed", arrow = "last", size = 1.5)
  draw_connection(0.15, 0.25, 0.25, 0.45, type = "dashed", arrow = "last", size = 1.5)
  draw_connection(0.85, 0.25, 0.75, 0.45, type = "dashed", arrow = "last", size = 1.5)
  
  # Dashed lines connecting corner text (medium thickness)
  draw_connection(0.1, 0.85, 0.9, 0.85, type = "dashed", size = 1.5)
  draw_connection(0.1, 0.15, 0.9, 0.15, type = "dashed", size = 1.5)
}

draw_diagram()