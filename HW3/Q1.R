library('ggplot2')

url <- 'https://hastie.su.domains/ElemStatLearn/datasets/bone.data'
download.file(url,destfile='bone.data')
bone<-read.table('bone.data',head=TRUE)
bone_m <- bone[which(bone$gender=='male'),]
bone_f <- bone[which(bone$gender=='female'),]

sp_m<- smooth.spline(x=bone_m$age,y=bone_m$spnbmd,lambda=0.00022,df=12)
sp_f<- smooth.spline(x=bone_f$age,y=bone_f$spnbmd,lambda=0.00022,df=12)


ggplot() + geom_point(data=bone_m ,aes(x=age,y=spnbmd),colour = "blue",size=1,show.legend = FALSE)+
  geom_hline(aes(yintercept=0.0),linetype=2)+
  geom_point(data=bone_f ,aes(x=age,y=spnbmd),colour = "red",size=1,show.legend = FALSE)+
  geom_line(aes(x=sp_f$x,y=sp_f$y,colour = "Female"),size=0.7,show.legend=TRUE)+
  geom_line(aes(x=sp_m$x,y=sp_m$y,colour = "Male"),size=0.7,show.legend=TRUE)+
  scale_color_manual(values=c("red","blue"))+
  ylab('Relative Change in Spinal BMD')+
  xlab('Age')+ylim(-0.07,0.25)+
  guides(color=guide_legend(title=NULL,reverse=TRUE))+
  theme_bw() + 
  theme(panel.grid=element_blank())+
  theme(legend.box.background = element_rect(),
        legend.position = c(.85, .85),
        legend.justification = c("right", "top"),
        legend.box.margin = margin(0.5, 30, 0.5, 30))
