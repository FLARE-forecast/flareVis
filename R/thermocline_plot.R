thermocline_plot <- function(td_past, td_future){

p <- ggplot(td_future, aes(DateTime,td_calc)) +
  geom_line() +
  geom_point(data=td_past, aes(DateTime,td_calc)) +
  #ylim(c(0,100)) +
  #scale_y_reverse(limits = c(min(depth_calc),max(depth_calc))) +
  scale_y_reverse( lim=c(100,0)) +
  geom_vline(xintercept =  as.POSIXct('2020-06-10'), linetype = 'dashed') +
  ylab('Lake Depth (ft)') +
  xlab('Date') +
  ggtitle('What Depth Is The Cold Layer?') +
  annotate("text", x=as.POSIXct('2020-06-05'), y=15, label= "Past", size = 8) +
  annotate("text", x=as.POSIXct('2020-06-16'), y=15, label= "Future", size = 8) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.title.x = element_text(size=12)) +
  theme(axis.title.y = element_text(size=12))

return(p)

}



