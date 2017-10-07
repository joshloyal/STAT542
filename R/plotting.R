library(lazyeval)

# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
      ncol = cols, nrow = ceiling(numPlots/cols))
  }

  if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
        layout.pos.col = matchidx$col))
    }
  }
}


target_scatter <- function(data, x, y) {
  ggplot(data = data, aes_string(x = x, y = y)) +
    geom_point() +
    geom_smooth()
}

..all.. <- '_all_variables_'

target_grid <- function(..., data, y, cols = 1) {
  variables <- c(list(...))

  if ( length(variables) == 1 && variables[[i]] == ..all..) {
    print('hi')
  }
  plots <- vector('list', length(variables))
  for (i in seq_along(variables)) {
    plots[[i]] <- target_scatter(data, x = variables[[i]], y = y)
  }

  plots['cols'] = cols
  do.call(multiplot, plots)
}