multiplot <- function(..., plotlist=NULL, file, cols=1, plot_layout=NULL, plot_title = NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If plot_layout is NULL, then use 'cols' to determine plot_layout
  if (is.null(plot_layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    # if plot_title argument given, add an extra row here
    if(is.null(plot_title)){
      plot_layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                       ncol = cols, nrow = ceiling(numPlots/cols), byrow = TRUE)
    } else {
      plot_layout <- matrix(c(rep(0, cols), seq(1, cols * (ceiling(numPlots/cols)))),
                       ncol = cols, nrow = (ceiling(numPlots/cols)+1), byrow = TRUE)
    }
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()

    # Make each plot, in the correct location
    if(is.null(plot_title)){
      pushViewport(viewport(layout = grid.layout(nrow(plot_layout), ncol(plot_layout))))
      
      for(i in 1:numPlots) {
        # Get the i,j matrix positions of the regions that contain this subplot
        matchidx <- as.data.frame(which(plot_layout == i, arr.ind = TRUE))
        
        print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                        layout.pos.col = matchidx$col))
      }
    } else {
      pushViewport(viewport(layout = grid.layout(nrow(plot_layout), ncol(plot_layout), 
                                                 heights = unit(c(1, rep(4, nrow(plot_layout)-1)), "null"))))
      
      grid.text(plot_title, gp = gpar(fontsize = 20, fontface = "bold"),
                vp = viewport(layout.pos.row = 1, layout.pos.col = c(1:ncol(plot_layout))))
      
      for(i in 1:numPlots) {
        # Get the i,j matrix positions of the regions that contain this subplot
        matchidx <- as.data.frame(which(plot_layout == i, arr.ind = TRUE))
        
        print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                        layout.pos.col = matchidx$col))
      }
    }
  }
}