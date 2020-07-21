
#  PLOT_PERFORMANCE_PV.R
#
#  Plots ALL the PV[f, ] using `colors` and `Flabel`(s) if supplied.
#  In addition, if outputFile is non-empty, write a PDF file containing
#  the plots.

plot_performance_PV <- function(percs, P, 
                                    roundTo=2, 
                                    title="",
                                    color="#000000", Flabel="F function", outputFile="", 
                                    legendPos="bottomright",
                                    xlab="percentage of edges", 
                                    ylab="fraction of relevant edges") {
  
  # Plots Progressive Comparison Performance for N>1 Fs
  #
  # Args:
  #   percs:      array   x-axis percentages of each percentage-view.
  #               We assume the same percentages for each F.
  #   PV:         matrix  dimension: N x length(percs)
  #                       each row:  y-axis performances of each percentage-view
  #   roundTo:    how many decimal digits?
  #   title:      main title, could be for example 'STATIC' or 'DYNAMIC'
  #   addYTicks:  TRUE if we want to add bolded ticks in the y-axis for pf
  #   color(s):   (optional) ordered list of colors to be assigned to Fs
  #   Flabel:     (optional) ordered list of names to be assigned to Fs
  #   outputFile  (optional) output file path where to print the plot. 
  #               Default is "" and standard output is used to show the graph.
  #   legendPos:  legend position, possible values: bottomright, upperleft
  #   xlab:       x-axis label
  #   ylab:       y-axis label
  # 
  # Returns: 
  #   Nothing.
  
  # Error handling
  # Code
  
  if (outputFile != "")   
    pdf(file=outputFile, height=7.2, width=7)
  
  if (is.vector(P))
    P = matrix(P, nrow=1, ncol=length(P))
  
  N = nrow(P);                   # number of functions to be plotted
  
  P = round(P, digits = roundTo)
  
  #nc = cbind(0, nc)
  
  percs = c(percs, 100)           # adjusting percentage breaks ticks (plot)
  xLabels <- as.character(percs*100)
  
  if (N == 1) {
    for (i in 1:length(xLabels)) {# TODO: add nc *onto* the points
      #      if (i==1)
      #        xLabels[i] = sprintf("0%%")
      #      else
      xLabels[i] = sprintf("%s%s", xLabels[i], "%")
    }
  }
  
  plot(P[1, ], type="o", pch=20, col=color[1], ylim=c(0.0, 1.0), axes=FALSE, ann=T, xlab=xlab, ylab=ylab, main = title, lwd=2)
  axis(1, at=1:length(percs), labels=as.character(xLabels), cex.axis=0.7)
  axis(2, las=1, at=seq(0, 1, by=0.1))

  text(P[1, ], labels=P[1, ], cex= 0.6, pos=3, col = color[1])
  
  if (N > 1)
    for (i in 2:N) {
      lines(P[i, ], type="o", pch=20, col=color[i], lwd=2)
      text( P[i, ], labels=P[i, ], cex= 0.6, pos=3, col = color[i])
#      print(P[i, ])
    }
  
  if (N > 1) {
    #print(color[1:N])
    legend(legendPos, as.vector(Flabel), cex=0.7, col=as.vector(color[1:N]), 
           pch=20, lty=1, lwd=2, bty="n", x.intersp = 1.2 );
  } else {
    legend(legendPos, as.vector(Flabel[1]), cex=0.7, col=as.vector(color[1]), 
           pch=20, lty=1, lwd=2, bty="n", x.intersp = 1.2 );
  }
  
  if (outputFile != "")
    dev.off()
  
}
