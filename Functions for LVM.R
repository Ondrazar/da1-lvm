# Disable scientific notation
options(scipen = 999)

#Libraries used...
library(scales)
library(reshape2)
library(ggplot2)
library(MVN) # install.packages("MVN",dependencies = TRUE) and install.packages("mgcv",dependencies = TRUE)
library(grid)
library(stringr)
library(RColorBrewer)
library(sp)


require(AID)

#Histograms for Distributions --> compare them with Germany data
hist.data <- function(data, xname){
  Data.hist <- ggplot(data.frame(data), aes(x=data))
  Data.hist + geom_histogram(fill="chartreuse3") +
    stat_bin(geom="text", color="black", size=3.5,
             aes(label=..count..), vjust= -1) +
    xlab(xname) +
    ylab("Frequency")
}

#hist. with Normal distr. curve
hist.norm <- function(data, xname){
  Data.hist <- ggplot(data.frame(data), aes(x=data))
  Data.hist + geom_histogram(fill="chartreuse3", aes(y=..density..)) +
    stat_function(fun=dnorm,
                  color="red",
                  args=list(mean=mean(data), 
                            sd=sd(data))) + 
    xlab(xname) +
    ylab("Density") +
    xlim(0,max(data))
}

#with % frequencies
hist.data.perc <- function(data1, xname1){
  Data.hist1 <- ggplot(data.frame(data1), aes(x=data1))
  Data.hist1 + geom_histogram(fill="chartreuse3") +
    geom_text(color="black", size=3.5, stat="bin",
              aes(y=(..count..),
                  label=scales::percent((..count..)/sum(..count..)), vjust= -1))+
    xlab(xname1) +
    ylab("Frequency")
}


#Steps for building a correlation heatmap:
#1.Reorder the correlation matrix working only with the upper part of the triangle:

# Get upper triangle of the correlation matrix
get_upper_tri <- function(x){
  x[lower.tri(x)]<- NA
  return(x)
}

# Based on distance using the correlations as a measure.
reorder_cormat <- function(x){
  dd <- as.dist((1-x)/2)
  hc <- hclust(dd)
  x <- x[hc$order, hc$order]
}
# Reordered upper part of the correlat matrix.
reorder_correlation <- function(x){
  x <- reorder_cormat(x)
  y <- get_upper_tri(x)
  return(y)
}

#2.Create a ggheatmap with coeff. on it
ggheatmap <- function(x, main){
  ggplot(x, aes(Var2, Var1, fill = value))+
    geom_tile(color = "white")+
    scale_fill_gradient2(low = "brown2", high = "chartreuse3", mid = "yellow", 
                         midpoint = 0, limit = c(-1,1), space = "Lab", 
                         name="Correlation Coefficient") +
    theme_minimal()+ # minimal theme
    theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                     size = 12, hjust = 1))+
    coord_fixed()+ # putting the correlat. on the map
    geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      panel.grid.major = element_blank(),
      panel.border = element_blank(),
      panel.background = element_blank(),
      axis.ticks = element_blank(),
      legend.justification = c(1, 0),
      legend.position = c(0.6, 0.7),
      legend.direction = "horizontal")+
    guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                                 title.position = "top", title.hjust = 0.5))+
    # adding main tittle to the map
    ggtitle(main) + theme(plot.title = element_text(lineheight=0.8, face="bold"))
}

#3.Final function which includes the previously defined steps
heatmap <- function(x, type, main){
  # selection of correlation method to use
  y <- round(cor(x, method=type),2)
  # melting function to stack correlations in a column, while removing NA values from the
  # lower half of the prev. generated corr. matrix
  y <- melt(reorder_correlation(y), na.rm = T)
  ggheatmap(y, main)
}


#PieChart with percetanges function
piecharts <- function(vector, vectlbls, name){
  slices <- vector
  lbls <- vectlbls
  pct <- round(slices/sum(slices)*100, 2)
  lbls <- paste(lbls,pct)
  lbls <- paste(lbls, "%", sep="")
  pie(slices, labels=lbls, col=terrain.colors(length(lbls)), main= name)
}

#Setting a multiplot function

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
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
