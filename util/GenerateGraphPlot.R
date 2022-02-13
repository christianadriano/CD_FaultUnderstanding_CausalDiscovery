"
Generate a plot and save to file
"



save_bayesian_net_plot <- function(bayesian_net,outcome_node,plot_title, 
                                   file_name,folder){
  gR <- graphviz.plot(bn,render = FALSE,
                      main=plot_title,
                      shape=c("ellipse"));
  gR = layoutGraph(gR, attrs = list(graph = list(rankdir = "LR")))
  
  graph.par(list(nodes=list(col="black", lty="solid", 
                            lwd=1, fontsize=14),
                 graph=list(cex.main=0.8)
  ))
  
  nodeRenderInfo(gR)$fill[c(outcome_node)]="lightblue"
  
  png(filename=paste0(folder,file_name,".png"))
  #,
  #    height=200,width=600,antialias = "cleartype",
  #    bg = "transparent");
  renderGraph(gR);
  dev.off();
}