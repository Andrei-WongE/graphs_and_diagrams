require(grid)
require(here)

draw_diagram <- function() {
  grid.newpage()
  
  # Helper function for text boxes
  draw_text_box <- function(x, y, label, width = NULL, height = NULL) {
    if (is.null(width)) width <- unit(1, "strwidth", label) + unit(0.2, "inches")
    if (is.null(height)) height <- unit(1, "strheight", label) + unit(0.1, "inches")
    
    pushViewport(viewport(x = x, y = y, just = "center"))
    grid.rect(width = width, height = height, gp = gpar(fill = "white"))
    grid.text(label, gp = gpar(fontsize = 8))
    popViewport()
  }
  
  # Main boxes
  draw_text_box(0.5, 0.95, "Marketization of\nurban housing")
  draw_text_box(0.5, 0.05, "Reconstruction of\nurban space")
  
  # Side boxes
  draw_text_box(0.2, 0.8, "Diversified housing\nsupply")
  draw_text_box(0.8, 0.8, "Personalized\nchoice of housing")
  draw_text_box(0.2, 0.2, "Differential resource\nallocation")
  draw_text_box(0.8, 0.2, "Active / Passive\nresidential migration")
  
  # Center boxes
  center_width <- unit(0.15, "npc")
  center_height <- unit(0.1, "npc")
  draw_text_box(0.2, 0.5, "Spatial\nreconstrution", center_width, center_height)
  draw_text_box(0.4, 0.5, "Dwelling\nspace", center_width, center_height)
  draw_text_box(0.6, 0.5, "Social\ngroups", center_width, center_height)
  draw_text_box(0.8, 0.5, "Social\ndifferentiation", center_width, center_height)
  
  # Coupling text
  grid.text("Coupling", x = 0.5, y = 0.5, rot = 90, gp = gpar(fontsize = 8))
  
  # Center labels
  grid.text("housing type    age/gender\nhousing price   occupation\nliving facility     income\ntraffic location   life cycle", 
            x = 0.5, y = 0.35, just = "center", gp = gpar(fontsize = 7))
  
  # Arrows (using grid.lines with arrow)
  grid.lines(x = c(0.5, 0.5), y = c(0.9, 0.6), 
             arrow = arrow(length = unit(0.1, "inches")), 
             gp = gpar(fill = "black"))
  grid.lines(x = c(0.5, 0.5), y = c(0.1, 0.4), 
             arrow = arrow(length = unit(0.1, "inches")), 
             gp = gpar(fill = "black"))
}

draw_diagram()