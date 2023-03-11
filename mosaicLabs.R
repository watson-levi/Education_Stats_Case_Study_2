compt_perc=function(x){
  d=c(x,1)-c(0,x)
  d[-length(d)]
}

mosaicLabs=function(p1){
  p1d<- ggplot_build(p1)$data %>% as.data.frame() %>% filter(.wt > 0)
  x=tapply(p1d$ymax,factor(p1d$fill,levels=unique(p1d$fill)),compt_perc)
  x=unlist(x)

  p1d$percentage=paste0(round(100*x,2),"%")

  p2<-p1 + 
    geom_label(data = p1d, 
              aes(x = (xmin + xmax)/2, 
                  y = (ymin + ymax)/2, 
                  label = percentage))
  p2<-p2+theme(legend.position="none")
  return(p2)
}