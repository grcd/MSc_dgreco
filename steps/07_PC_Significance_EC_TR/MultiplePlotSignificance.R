
MultiplePlotSignificance <- function(S, PF,
                                     roundTo=2, 
                                     title="", 
                                     addYTicks=FALSE,
                                     color="#CCCCCC", # lines
                                     Flabel="F function", 
                                     outputFile="", 
                                     legendPos="bottomright",
                                     xlab="percentage of edges", 
                                     ylab="fraction of relevant edges") {
  
  # Plots Progressive Comparison Performance for N>1 Fs
  #
  # Args:
  #   percs:      array   x-axis percentages of each percentage-view.
  #               We assume the same percentages for each F.
  #   nc:         matrix  dimension: N x length(percs) 
  #                       each row:  number of classes for each percentage-view
  #   PF:         matrix  dimension: N x length(percs)
  #                       each row:  y-axis performances of each percentage-view
  #   roundTo:    how many decimal digits?
  #   title:      main title, could be for example 'STATIC' or 'DYNAMIC'
  #   addYTicks:  TRUE if we want to add bolded ticks in the y-axis for PF
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
  
  PF = as.matrix(PF)
  percs = as.numeric(colnames(PF))
  xLabels <- as.character(percs*100)
  
  if (is.vector(S))
    S = matrix(S, nrow=1, ncol=length(S))
  
  if (outputFile != "")   
    pdf(file=outputFile, height=7.2, width=7)
  
  N = nrow(PF);                       # number of functions to be plotted
  PF = round(PF, digits = roundTo)
  
  for (i in 1:length(xLabels)) {
    xLabels[i] = sprintf("%s%s", xLabels[i], "%")
  }
  
  # color[ S[1, ] + 1 ]:  if    S[1, i] == 0, then col=1, which means non-significant
  #                       else  S[1, i] == 1, then col=2, which means significant
  points_type = c(15,17)  # 15=nonsignificant, 17=significant
  points_color = c("#CCCCCC", "#FF0000")
  
  if (N == 1) {  # SINGLE PLOT SECTION

    plot(PF[1, ], type="p", pch=points_type[ (S[1,]+1)  ], col=points_color[ (S[1,]+1) ], ylim=c(0.0, 1.0), axes=FALSE, ann=F, xlab=xlab, ylab=ylab, main = title, lwd=2)
    lines(PF[1, ], type="c", lwd=2, col="#CCCCCC", main=title)
    
    axis(1, at=1:length(percs), labels=as.character(xLabels), cex.axis=0.7)
    axis(2, las=1, at=seq(0, 1, by=0.1))
    
    if (addYTicks)
      axis(2, at=as.double(PF), labels=as.character(PF), col.ticks="#000000", lwd.ticks = 2, las=2)
    
    text(PF[1, ], labels=PF[1, ], cex= 0.6, pos=3, col = points_color[ (S[1,]+1) ])
    title(main = title)
    box()
    
  } else {  # MULTIPLE PLOT SECTION
    points_type = c(20,17)  # 15=nonsignificant, 17=significant
    
    plot(PF[1, ], type="o", pch=points_type[ (S[1,]+1)  ], col=color[1], ylim=c(0.0, 1.0), axes=FALSE, ann=T, xlab=xlab, ylab=ylab, main = title, lwd=2)
    lines(PF[1, ], type="c", col=color[1], lwd=2)
    
    if (addYTicks)
      axis(2, at=as.double(PF), labels=as.character(PF), col.ticks="#000000", lwd.ticks = 2, las=2)
    
    for (i in 2:N) {
      lines(PF[i, ], type="o", pch=points_type[ (S[1,]+1)  ], col=color[i], lwd=2)
      #text( PF[i, ], labels=PF[i, ], cex= 0.6, pos=3, col = color[ (S[1,]+1) ])
    }
    
    axis(1, at=1:length(percs), labels=as.character(xLabels), cex.axis=0.7)
    axis(2, las=1, at=seq(0, 1, by=0.1))
    box()
    
  }
  
  if (N == 1) {
    legend(legendPos, as.vector(Flabel[1]), cex=0.7, col="#CCCCCC", 
           pch=20, lty=1, lwd=2, bty="n", x.intersp = 1.2 );
  } else {
    #print(color[1:N])
    legend(legendPos, as.vector(Flabel), cex=0.7, col=as.vector(color[1:N]), 
           pch=20, lty=1, lwd=2, bty="n", x.intersp = 1.2 );
  }
  
  if (outputFile != "")
    dev.off()
  
}
