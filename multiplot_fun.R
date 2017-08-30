multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL, title = NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    # if title argument given, add an extra row here
    if(is.null(title)){
      layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                       ncol = cols, nrow = ceiling(numPlots/cols), byrow = TRUE)
    } else {
      layout <- matrix(c(rep(0, cols), seq(1, cols * (ceiling(numPlots/cols)))),
                       ncol = cols, nrow = (ceiling(numPlots/cols)+1), byrow = TRUE)
    }
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout), 
                                               heights = unit(c(1, rep(4, nrow(layout)-1)), "null"))))
    
    # Make each plot, in the correct location
    if(is.null(title)){
      
      for(i in 1:numPlots) {
        # Get the i,j matrix positions of the regions that contain this subplot
        matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
        
        print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                        layout.pos.col = matchidx$col))
      }
    } else {
      grid.text(title, gp = gpar(fontsize = 20, fontface = "bold"),
                vp = viewport(layout.pos.row = 1, layout.pos.col = c(1:ncol(layout))))
      
      for(i in 1:numPlots) {
        # Get the i,j matrix positions of the regions that contain this subplot
        matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
        
        print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                        layout.pos.col = matchidx$col))
      }
    }
  }
}