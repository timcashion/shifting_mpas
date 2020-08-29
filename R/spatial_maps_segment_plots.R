spatial_maps_segment_plots <- function(df=NA, MPA_design=NA, type_measure=NA, RCP_scenario=NA){
  p_df <- df %>% 
    filter(RCP==RCP_scenario) %>% 
    filter(type==type_measure) %>% 
    filter(design==MPA_design)
  p <- p_df %>%  
    ggplot() + 
    geom_tile(aes(x=col_num, y=rev(row_num), fill=value)) +
    scale_fill_gradientn(colours = zissou_continuous, limits=c(min_value, max_value))  + 
    ggtitle(label= " ", subtitle = MPA_design) + 
    #Make MPA borders
    geom_segment(data=mpa_annotation_df %>% filter(design==MPA_design), aes(x = mx1, xend=mx1, y=my1, yend=my2), color="black") + 
    geom_segment(data=mpa_annotation_df %>% filter(design==MPA_design), aes(x = mx1, xend=mx2, y=my1, yend=my1), color="black") + 
    geom_segment(data=mpa_annotation_df %>% filter(design==MPA_design), aes(x = mx2, xend=mx2, y=my1, yend=my2), color="black") + 
    geom_segment(data=mpa_annotation_df %>% filter(design==MPA_design), aes(x = mx1, xend=mx2, y=my2, yend=my2), color="black") + 
    
    #Make Adjacent borders
    geom_segment(data=mpa_annotation_df %>% filter(design==MPA_design), aes(x = ax1, xend=ax1, y=ay1, yend=ay2), color="grey50", lty="dashed") + 
    geom_segment(data=mpa_annotation_df %>% filter(design==MPA_design), aes(x = ax1, xend=ax2, y=ay1, yend=ay1), color="grey50", lty="dashed") + 
    geom_segment(data=mpa_annotation_df %>% filter(design==MPA_design), aes(x = ax2, xend=ax2, y=ay1, yend=ay2), color="grey50", lty="dashed") + 
    geom_segment(data=mpa_annotation_df %>% filter(design==MPA_design), aes(x = ax1, xend=ax2, y=ay2, yend=ay2), color="grey50", lty="dashed") + 
    scale_x_continuous(expand=c(0,0)) + 
    scale_y_continuous(expand=c(0,0)) +
    xlab("") +
    ylab("") + 
    #labs(fill=expression("Biomass t/km"^2, sep="")) + 
    theme(legend.position = "none") + 
    NULL
  
  # if(MPA_design == "No MPA"){
  #   if(RCP_scenario=="RCP8.5"){
  #     p <- p +
  #       ylab("4 warming")
  #   } else {
  #     p <- p +
  #       ylab("2 degrees warming")
  #   }
  # }
  return(p)
}