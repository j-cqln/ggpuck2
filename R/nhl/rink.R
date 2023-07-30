# Rink plotting functionality from Brian Macdonald

library(ggplot2)
library(grid)

dark_rink_color <- '#000000'
light_rink_color <- '#ffffff'

polygon2 = function(x, y, border = dark_rink_color, lwd = 1, col = NA){
  a = eval(substitute(
    expr = {
      aes(x = x, y = y)
    },
    env = list(x = x, y = y)))
  lp = geom_polygon(a, fill = col, linetype = 'solid', colour = border, size = lwd/2)
  assign('p', p + lp, envir = .GlobalEnv)
}

segments2 = function(x, y, xend, yend, col = 1, lwd = 3, lty = 1){
  a = eval(substitute(
    expr = {
      aes(x = x,y = y, xend = xend, yend = yend)
    },
    env = list(x = x, y = y, xend = xend, yend = yend)))
  lp = geom_segment(a, size = lwd/2, colour = col, lineend = 'square', linetype = lty)
  assign('p', p + lp, envir= .GlobalEnv)
}

lines2 = function(x, y, col = dark_rink_color, lwd = stdlwd, lty = 1){
  a = eval(substitute(
    expr = {
      aes(x = x, y = y)
    },
    env = list(x = x, y = y)))
  lp = geom_line(a, size = lwd/2, colour = col, lineend = 'square', linetype = lty)
  assign('p', p + lp, envir = .GlobalEnv)
}

rect2 = function(xleft, ybottom, xright, ytop, col = NA, border = dark_rink_color, lwd = stdlwd){
  polygon2(c(xleft, xleft, xright, xright), c(ytop, ybottom, ybottom, ytop), border = border, col = col, lwd = lwd)
}

full_rink = function(color = TRUE) {
  red = '#b3b3b3'; blue = '#b3b3b3'; lightblue = '#cccccc'
  
  if (color) { red = '#da6e80'; blue = '#85a3e2'; lightblue = '#9ad3eb' }
  
  faceoff_circle = function(x, y) {
    theta = seq(0,2*pi, length = 300)
    dd <- (5+7/12)/2 # http://www.nhl.com/ice/news.htm?id=733257
    
    polygon2 (
      x = x + 15*cos(theta), # face-off outer circle
      y = y + 15*sin(theta),
      lwd = stdlwd,
      border = red)
    
    polygon2 (
      x = x + 1*cos(theta), # face-off dot
      y = y + 1*sin(theta),
      col = red, # color of inside face-off dot
      border = red) # red color outline
    
    # face-off guides
    segments2 (
      x = c(x-0.75,x-0.75, x+0.75,x+0.75, x-0.75,x-0.75, x+0.75,x+0.75), # 0.75 = 9 inches
      y = c(y-2,y-2, y-2,y-2, y+2,y+2,y+2,y+2),
      xend = c(x-0.75,x-3.75, x+0.75,x+3.75, x-0.75,x-3.75, x+0.75,x+3.75),
      yend = c(y-6,y-2, y-6,y-2, y+6,y+2,y+6,y+2),
      col = red, lwd = stdlwd)
    
    # hash marks
    segments2 (
      x = c(x-15, x-15, x+15, x+15), # vertical points
      y = c(y-dd, y+dd, y-dd, y+dd), # lwd ? may not be exactly 2 inches
      xend = c(x-17, x-17, x+17, x+17),
      yend = c(y-dd, y+dd, y-dd, y+dd),
      col = red, lwd = stdlwd)
  }
  
  goal_crease_2 = function(flip = FALSE) {
    xseq = c()
    yseq = c()
    theta = c()
    
    if (flip) { # round part up
      x = 0 # center of oval coordinates (x,y)
      y = -84.5
      theta = seq(0,pi,length=300) # top part of oval
      xseq = x+4*cos(theta) # long radius
      yseq = y+1.5*sin(theta) # short radius
      xseq = c(xseq,x-4,x+4) # bottom rectangle points
      yseq = c(yseq,y-4.5,y-4.5) 
    }
    
    if (!flip) { # round part down
      x = 0
      y = 84.5
      theta = seq(pi,2*pi, length = 300) # bottom of oval
      xseq = x+4*cos(theta)
      yseq = y+1.5*sin(theta)
      xseq = c(xseq, x+4, x-4) # top rectangle points 
      yseq = c(yseq, y+4.5, y+4.5)
    }
    
    polygon2(xseq,
             yseq,
             lwd = stdlwd, # width of red line
             border = red,
             col = lightblue)
  }
  
  # center circle
  theta = seq(0,2*pi, length = 300)
  polygon2(15*cos(theta), 15*sin(theta), lwd = stdlwd, border = blue)
  
  # ref circle
  theta2 = seq (-pi/2, pi/2, length = 300)
  polygon2(-42.5 + 10*cos(theta2), 10*sin(theta2), lwd = stdlwd, border = red)
  
  # top base/blue line
  rect2(-42.5, 25, 42.5, 26, col = blue, border = light_rink_color, lwd = .001)
  
  # bottom blue line
  rect2(-42.5, -25, 42.5, -26, col = blue, border = light_rink_color, lwd = .001)
  
  # center red line
  rect2(-42.5, -0.5, 42.5, 0.5, col = red, border = light_rink_color, lwd = .001)
  
  # dotted white line
  lines2(c(-42.5, 42.5), c(0, 0), lty = 2, lwd = 1, col = light_rink_color)
  
  # goal line
  goal_line_extreme = 42.5 - 28 + sqrt(28^2 - (28-11)^2)
  
  lines2(goal_line_extreme*c(-1, 1), rep(89,2), col = red, lwd = stdlwd)
  lines2(goal_line_extreme*c(-1, 1), rep(-89,2), col = red, lwd = stdlwd)
  
  # goal net
  lines2(c(-3,-3,3,3), c(90,92,92,90)-1, col = 1, lwd = stdlwd)
  lines2(c(-3,-3,3,3), -(c(90,92,92,90)-1), col = 1, lwd = stdlwd)
  goal_crease_2();
  goal_crease_2(flip = TRUE)
  
  # traps
  segments2(c(-11, 11, -11, 11), c(89, 89,-89, -89),
            c(-14, 14, -14, 14), c(100, 100, -100, -100), col = red, lwd = stdlwd)
  segments2(c( -11, 11), c(-89, -89),
            c(-14, 14), c(-100, -100), col = red, lwd = stdlwd)
  
  faceoff_circle(-22, 69)
  faceoff_circle(22, 69)
  
  faceoff_circle(-22, -69)
  faceoff_circle(22, -69)
  
  faceoff_dot = function(x, y, r = 1, col = red) {
    polygon2(x + r*cos(theta),
             y + r*sin(theta),
             col = col,
             border = light_rink_color, lwd = .001)
  }
  
  faceoff_dot(22, 20);
  faceoff_dot(22, -20);
  faceoff_dot(-22, 20);
  faceoff_dot(-22, -20);
  
  lines2(c(-42.5, # outer edge, top
           -42.5 + 28 - 28*cos(seq(0, pi/2, length = 20)),
           42.5 - 28 + 28*cos(seq(pi/2, 0, length = 20)),
           42.5),
         c(-24,
           72 + 28*sin(seq(0, pi/2, length = 20)),
           72 + 28*sin(seq(pi/2, 0, length = 20)),
           -24),
         col = dark_rink_color, lwd = stdlwd)
  
  lines2(c(-42.5, # outer edge, bottom
           -42.5 + 28 - 28*cos(seq(0, pi/2, length = 20)),
           42.5 - 28 + 28*cos(seq(pi/2, 0, length = 20)),
           42.5),
         c(-24,
           -72 - 28*sin(seq(0, pi/2, length = 20)),
           -72 - 28*sin(seq(pi/2, 0, length = 20)),
           -24),
         col = dark_rink_color, lwd = stdlwd)
  
  faceoff_dot(0, 0, .5, blue)
}

stdlwd <- .5

p <- ggplot()
full_rink(color = TRUE)
rink <- p