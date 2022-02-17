####Prey Switehing Model####
###originally created MAY 2018, version from Jan 2022
###Authors C. Prokopenko, T Avgar

################################################
#####Terminology notes:
#####through revision some parameter names have changes
###prey x in  code is prey i  (vulnerable) in Manuscript, prey y in code is prey j  (costly) in Mnauscript
###chase (c) in code is attack (a) in MS, handling (h) in code is consume (b) in MS

###################################################

#RUN MODEL CODE FIRST#
##Check density (N) values for each plot

library(data.table)
library(ggplot2)
library(grid)
library(gridExtra)
library(lattice)

###see appendix S3 for calculations
halffe = 166
halffy = .166
halffeN= .67
halffyN= .67

###MULTIPREY RESULTS##############
###Nx and Ny varies, 
### Use Ny: L116 in 01 model code. Nx: 124
###Cost dissimilarity is single value
# consumption rate in prey units#
p2a <- ggplot()  + ggtitle("a.")
p2a <- p2a + geom_segment(aes(x = 10, y = halffy, xend = 0, yend = halffy), arrow = NULL,linetype = "dashed", colour  = "black", size= 1)
p2a <- p2a + geom_line(data = setDT(FRDF)[Nx==min(Nx)],aes(x=Ny, y=fy_null, linetype = "1"), colour = "black", size = 1.5, alpha = .9)
p2a <- p2a + geom_line(data = setDT(FRDF)[Nx==min(Nx)],aes(x=Ny, y=fy_s,colour = "a"),size = 1.5)
#p2a <- p2a + geom_line(data = setDT(FRDF)[Nx==min(Nx)],aes(x=Ny, y=fy_es,colour = ""),size = 1)
p2a <- p2a + geom_line(data = setDT(FRDF)[Nx==min(Nx)],aes(x=Ny, y=fy_ec+.002,colour = "b"),size = 1.5,linetype = "dashed", alpha = 1)
p2a <- p2a + geom_line(data = setDT(FRDF)[Nx==min(Nx)],aes(x=Ny, y=fy_tc+.002,colour = "c"),size = 1.5, alpha =1)
p2a <- p2a + geom_line(data = setDT(FRDF)[Nx==min(Nx)],aes(x=Ny, y=fy_pk,colour = "d"),size = 1.5)
p2a <- p2a + geom_line(data = setDT(FRDF)[Nx==min(Nx)],aes(x=Ny, y=fy_eh-0.002,colour = "e"),linetype = "dashed",size = 1.5, alpha = 1)
p2a <- p2a + geom_line(data = setDT(FRDF)[Nx==min(Nx)],aes(x=Ny, y=fy_th,colour = "f"),linetype = "dashed",size = 1.5)
#p2a <- p2a + geom_line(data = setDT(FRDF)[Nx==min(Nx)],aes(x=Ny, y=fy_E,colour = ""),size = 1)
p2a <- p2a + ylim(0,.315) +xlim(0,10) 
#p2a <- p2a + ylim(0, 0.05) +xlim(0,10) 
p2a <- p2a + xlab("") + ylab(bquote('Prey Consumed per Time'))
p2a <- p2a + theme(text = element_text(size = 20),
                 axis.line.y = element_line(color = "black"),
                 axis.ticks.y= element_line(color = "black"),
                 axis.text.y =element_text(colour = "black"),
                 axis.line.x = element_line(color = "grey30"),
                 axis.ticks.x = element_line(color = "grey30"),
                 axis.text.x = element_text(color = "grey30"),
                 axis.line = element_line(size=.8),
                 panel.background = element_rect(fill = "#eef6ec"),#FAF6FB
                 panel.grid.major =element_blank(),
                 panel.grid.minor = element_blank(),
                 plot.background = element_rect(fill = "white",colour = "black",size =1), #EEF0EF
                 plot.title = element_text(size = 20, colour = "grey30"),
                 legend.title = element_text(size = 20, colour = "grey30"),
                 legend.text = element_text(size=18, colour = "grey30"),
                 legend.key = element_blank(),
                 legend.justification=c(0,1), 
                 legend.position="none",
                 legend.background = element_blank())
p2a <- p2a + scale_linetype_manual(name = "Multiprey",
                                 values=c( "1" = "solid"),
                                 labels = c("Similar"))
p2a <- p2a + scale_colour_manual(name = "Dissimilar cost in",
                               values=c( "a" = "#CC79A7", "b" = "#E69F00", "c" =  "#F0E442", "d"="#009E73", "e" = "#0072B2","f" = "#56B4E9"),
                               labels = c("Search Rate", "Attack Energy", "Attack Time", "Kill Probability", "Consume Energy", "Consume Time"))
p2a <- p2a + guides(color = guide_legend(order = 2),
                          linetype = guide_legend(order = 1))

p2a


p2c <- ggplot() + ggtitle("c.") 
p2c <- p2c + geom_segment(aes(x = 10, y = halffy, xend = 0, yend = halffy), arrow = NULL,linetype = "dashed", colour  = "black", size= 1) 
p2c <- p2c + geom_line(data = setDT(FRDF)[Nx==max(Nx)],aes(x=Ny, y=fy_s,colour = "a"),size = 1.5)
p2c <- p2c + geom_line(data = setDT(FRDF)[Nx==max(Nx)],aes(x=Ny, y=fy_ec+.002,colour = "b"),size = 1.5)
p2c <- p2c + geom_line(data = setDT(FRDF)[Nx==max(Nx)],aes(x=Ny, y=fy_tc+.002,colour = "c"),size = 1.5 )
p2c <- p2c + geom_line(data = setDT(FRDF)[Nx==max(Nx)],aes(x=Ny, y=fy_pk,colour = "d"),size = 1.5)
p2c <- p2c + geom_line(data = setDT(FRDF)[Nx==max(Nx)],aes(x=Ny, y=fy_eh,colour = "e"),linetype = "dashed",size = 1.5)
p2c <- p2c + geom_line(data = setDT(FRDF)[Nx==max(Nx)],aes(x=Ny, y=fy_th,colour = "f"),linetype = "dashed",size = 1.5)
p2c <- p2c + geom_line(data = setDT(FRDF)[Nx==max(Nx)],aes(x=Ny, y=fy_null, linetype = "1"),colour = "black", size = 1.5 , alpha = .9)
p2c <- p2c + ylim(0,.315) +xlim(0,10) 
#p2c <- p2c + ylim(0,.05) +xlim(0,10) 
p2c <- p2c + xlab("Prey Density") + ylab(bquote('Prey Consumed per Time'))
p2c <- p2c + theme(text = element_text(size = 20),
                 axis.line.y = element_line(color = "black"),
                 axis.ticks.y= element_line(color = "black"),
                 axis.text.y =element_text(colour = "black"),
                 axis.line.x = element_line(color = "grey30"),
                 axis.ticks.x = element_line(color = "grey30"),
                 axis.text.x = element_text(color = "grey30"),
                 axis.line = element_line(size=.8),
                 panel.background = element_rect(fill = "#eef6ec"),#FAF6FB
                 panel.grid.major =element_blank(),
                 panel.grid.minor = element_blank(),
                 plot.background = element_rect(fill = "#EEF0EF", colour = "black",size =1), 
                 plot.title = element_text(size = 20, colour = "grey30"),
                 legend.title = element_text(size = 20, colour = "grey30"),
                 legend.text = element_text(size=18, colour = "grey30"),
                 legend.key = element_blank(),
                 legend.justification=c(0,1), 
                 legend.position="none",
                 legend.background = element_blank())
p2c <- p2c + scale_linetype_manual(name = "Multiprey",
                                 values=c( "1" = "solid"),
                                 labels = c("Similar"))
p2c <- p2c + scale_colour_manual(name = "Dissimilar cost in",
                               values=c( "a" = "#CC79A7", "b" = "#E69F00", "c" =  "#F0E442", "d"="#009E73", "e" = "#0072B2","f" = "#56B4E9"),
                               labels = c("Search Rate", "Attack Energy", "Attack Time", "Kill Probability", "Consume Energy", "Consume Time"))
p2c <- p2c + guides(color = guide_legend(order = 2),
                  linetype = guide_legend(order = 1))
p2c


###consumption rates
p2b <- ggplot() + ggtitle("b.") 
p2b <- p2b +geom_segment(aes(x = 10, y = halffy, xend = 0, yend = halffy), arrow = NULL,linetype = "dashed", colour  = "black", size= 1)
p2b <- p2b + geom_line(data = setDT(FRDF)[Ny==min(Ny)],aes(x=Nx, y=fx_null,linetype = "1"),colour = "black", size = 1, alpha = .9)
p2b <- p2b + geom_line(data = setDT(FRDF)[Ny==min(Ny)],aes(x=Nx+0.02, y=fx_s,colour = "a"),linetype = "dashed",size = 1.5)
#p2b <- p2b + geom_line(data = setDT(FRDF)[Ny==min(Ny)],aes(x=Nx, y=fx_es,colour = ""),size = 1)
p2b <- p2b + geom_line(data = setDT(FRDF)[Ny==min(Ny)],aes(x=Nx+0.02, y=fx_ec+0.002,colour = "b"),linetype = "dashed",size = 1.5, alpha = 1)
p2b <- p2b + geom_line(data = setDT(FRDF)[Ny==min(Ny)],aes(x=Nx+0.02, y=fx_tc+0.001,colour = "c"),linetype = "dashed",size = 1.5, alpha = 1)
p2b <- p2b + geom_line(data = setDT(FRDF)[Ny==min(Ny)],aes(x=Nx, y=fx_pk,colour = "d"),linetype = "dashed",size = 1.5)
p2b <- p2b + geom_line(data = setDT(FRDF)[Ny==min(Ny)],aes(x=Nx, y=fx_eh-0.002,colour = "e"),linetype = "dashed",size = 1.5, alpha = 1)
p2b <- p2b + geom_line(data = setDT(FRDF)[Ny==min(Ny)],aes(x=Nx, y=fx_th-0.001,colour = "f"),linetype = "dashed",size = 1.5)
#p2b <- p2b + geom_line(data = setDT(FRDF)[Ny==min(Ny)],aes(x=Nx, y=fx_E,colour = "a"),size = 1)
p2b <- p2b + ylim(0,.315) +xlim(0,10) 
#p2b <- p2b + ylim(0,.05) +xlim(0,10) 
p2b <- p2b + xlab("") + ylab(bquote(''))
p2b <- p2b + theme(text = element_text(size = 20),
                 axis.line.y = element_line(color = "black"),
                 axis.ticks.y= element_line(color = "black"),
                 axis.text.y =element_text(colour = "black"),
                 axis.line.x = element_line(color = "grey30"),
                 axis.ticks.x = element_line(color = "grey30"),
                 axis.text.x = element_text(color = "grey30"),
                 axis.line = element_line(size=.8),
                 panel.background = element_rect(fill = "#F4ECF6"),
                 panel.grid.major =element_blank(),
                 panel.grid.minor = element_blank(),
                 plot.background = element_rect(fill = "white",colour = "black",size =1), #EEF0EF
                 plot.title = element_text(size = 20, colour = "grey30"),
                 legend.title = element_text(size = 20, colour = "grey30"),
                 legend.text = element_text(size=18, colour = "grey30"),
                 legend.key = element_blank(),
                 legend.justification=c(0,1), 
                 legend.position=c(.6,.6),
                 legend.background = element_rect(fill = "white"))
p2b <- p2b + scale_linetype_manual(name = "Multiprey",
                                 values=c( "1" = "solid"),
                                 labels = c("Similar"))
p2b <- p2b + scale_colour_manual(name = "Dissimilar cost in",
                               values=c( "a" = "#CC79A7", "b" = "#E69F00", "c" =  "#F0E442", "d"="#009E73", "e" = "#0072B2","f" = "#56B4E9"),
                               labels = c("Search Rate", "Attack Energy", "Attack Time", "Kill Probability", "Consume Energy", "Consume Time"))
p2b <- p2b + guides(color = guide_legend(order = 2),
                  linetype = guide_legend(order = 1))
p2b

p2d <- ggplot() +  ggtitle("d.")
p2d <- p2d + geom_segment(aes(x = 10, y = halffy, xend = 0, yend = halffy), arrow = NULL,linetype = "dashed", colour  = "black", size= 1) 
p2d <- p2d + geom_line(data = setDT(FRDF)[Ny==max(Ny)],aes(x=Nx, y=fx_s,colour = "a"),size = 1)
#p2d <- p2d + geom_line(data = setDT(FRDF)[Ny==max(Ny)],aes(x=Nx, y=fx_es,colour = ""),size = 1)
p2d <- p2d + geom_line(data = setDT(FRDF)[Ny==max(Ny)],aes(x=Nx, y=fx_ec+.002,colour = "b"), size = 1.5 , alpha = 1)
p2d <- p2d + geom_line(data = setDT(FRDF)[Ny==max(Ny)],aes(x=Nx, y=fx_tc+.002,colour = "c"),size = 1.5 , alpha = 1)
p2d <- p2d + geom_line(data = setDT(FRDF)[Ny==max(Ny)],aes(x=Nx, y=fx_pk,colour = "d"),size = 1.5)
p2d <- p2d + geom_line(data = setDT(FRDF)[Ny==max(Ny)],aes(x=Nx, y=fx_eh,colour = "e"),linetype = "dashed",size = 1.5 , alpha = 1)
p2d <- p2d + geom_line(data = setDT(FRDF)[Ny==max(Ny)],aes(x=Nx, y=fx_th,colour = "f"),linetype = "dashed",size = 1.5)
p2d <- p2d + geom_line(data = setDT(FRDF)[Ny==max(Ny)],aes(x=Nx, y=fx_null,linetype = "1"),colour = "black",size = 1.5 , alpha = .9)
p2d <- p2d + ylim(0,.315) +xlim(0,10) 
#p2d <- p2d + ylim(0,.05) +xlim(0,10) 
p2d <- p2d + xlab("Prey Density") + ylab(bquote(''))
p2d <- p2d + theme(text = element_text(size=20),
                axis.line.y = element_line(color = "black"),
                axis.ticks.y= element_line(color = "black"),
                axis.text.y =element_text(colour = "black"),
                axis.line.x = element_line(color = "grey30"),
                axis.ticks.x = element_line(color = "grey30"),
                axis.text.x = element_text(color = "grey30"),
                axis.line = element_line(size=.8),
                panel.background = element_rect(fill = "#F4ECF6"),
                panel.grid.major =element_blank(),
                panel.grid.minor = element_blank(),
                plot.background = element_rect(fill = "#EEF0EF", colour = "black", size =1), 
                plot.title = element_text(size = 20, colour = "grey30"),
                legend.title = element_text(size=20, colour = "grey30"),
                legend.text = element_text(size=18, colour = "grey30"),
                legend.key = element_blank(),
                legend.justification=c(0,1), 
                legend.position="none",
                legend.background = element_blank())
p2d <- p2d + scale_linetype_manual(name = "Multiprey",
                                    values=c( "1" = "solid"),
                                    labels = c("Similar"))
p2d <- p2d + scale_colour_manual(name = "Dissimilar cost in",
                                  values=c( "a" = "#CC79A7", "b" = "#E69F00", "c" =  "#F0E442", "d"="#009E73", "e" = "#0072B2","f" = "#56B4E9"),
                                  labels = c("Search Rate", "Attack Energy", "Attack Time", "Kill Probability", "Consume Energy", "Consume Time"))
p2d <- p2d + guides(color = guide_legend(order = 2),
                  linetype = guide_legend(order = 1))

p2d


r <- rectGrob(gp=gpar(fill="white", col = 'white'))

c <- grobTree(rectGrob(gp=gpar(fill="#eef6ec", col = "white")), textGrob("Costly Prey", gp = gpar(fontsize = 25))) 
v <- grobTree(rectGrob(gp=gpar(fill="#F4ECF6", col = "white")), textGrob("Vulnerable Prey", gp = gpar(fontsize = 25, fill = "#F4ECF6"))) 
l <- grobTree(rectGrob(gp=gpar(fill="white", col = "white")), textGrob("Low Alternative Prey Density", rot = 90, gp = gpar(fontsize = 25))) 
h <- grobTree(rectGrob(gp=gpar(fill = "#EEF0EF", col = "white")), textGrob("High Alternative Prey Density", rot = 90, gp = gpar(fontsize = 25))) 


png("Fig2-2prey.tiff", width = 16000, height = 14000, res=1000, units="px")
grid.arrange(r,c,v,
            l,p2a,p2b,
            h,p2c,p2d,
            ncol=3,
            nrow=3,
            widths = c(1,6,6),
            heights = c(1,6,6),
            top = textGrob("Functional Response", just = .43, gp=gpar(fontsize=30))
            #left = textGrob("Alternative Prey",rot = 90,just = .7, gp=gpar(fontsize=25))
            )
dev.off()

###Figure A4.2###
#predation rates#
pA4.a <- ggplot()  + ggtitle("a.")
pA4.a <- pA4.a + geom_line(data = setDT(FRDF)[Nx==min(Nx)],aes(x=Ny, y=fy_s/Ny,colour = "a"),size = 1.5)
pA4.a <- pA4.a + geom_line(data = setDT(FRDF)[Nx==min(Nx)],aes(x=Ny, y=fy_null/Ny, linetype = "1"),colour = "black",size = 1.5, alpha = .9)
pA4.a <- pA4.a + geom_line(data = setDT(FRDF)[Nx==min(Nx)],aes(x=Ny+0.02, y=(fy_ec/Ny)+.002,colour = "b"),linetype = "dashed",size = 1.5, alpha = 1)
pA4.a <- pA4.a + geom_line(data = setDT(FRDF)[Nx==min(Nx)],aes(x=Ny, y=(fy_tc/Ny)+.002,colour = "c"),size = 1.5, alpha =1)
pA4.a <- pA4.a + geom_line(data = setDT(FRDF)[Nx==min(Nx)],aes(x=Ny, y=fy_pk/Ny,colour = "d"),size = 1.5)
pA4.a <- pA4.a + geom_line(data = setDT(FRDF)[Nx==min(Nx)],aes(x=Ny, y=(fy_eh/Ny),colour = "e"),linetype = "dashed",size = 1.5, alpha = 1)
pA4.a <- pA4.a + geom_line(data = setDT(FRDF)[Nx==min(Nx)],aes(x=Ny, y=(fy_th/Ny),colour = "f"),linetype = "dashed",size = 1.5)
pA4.a <- pA4.a + ylim(0,.4) +xlim(0,10) 
pA4.a <- pA4.a + xlab("") + ylab('Per Capita Predation')
pA4.a <- pA4.a + theme(text = element_text(size = 20),
                   axis.line.y = element_line(color = "black"),
                   axis.ticks.y= element_line(color = "black"),
                   axis.text.y =element_text(colour = "black"),
                   axis.line.x = element_line(color = "grey30"),
                   axis.ticks.x = element_line(color = "grey30"),
                   axis.text.x = element_text(color = "grey30"),
                   axis.line = element_line(size=.8),
                   panel.background = element_rect(fill = "#eef6ec"),#FAF6FB
                   panel.grid.major =element_blank(),
                   panel.grid.minor = element_blank(),
                   plot.background = element_rect(fill = "white", colour = "black", size =1), #EEF0EF
                   plot.title = element_text(size = 20, colour = "grey30"),
                   legend.title = element_text(size = 20, colour = "grey30"),
                   legend.text = element_text(size=18, colour = "grey30"),
                   legend.key = element_blank(),
                   legend.justification=c(0,1), 
                   legend.position="none",
                   legend.background = element_blank())
pA4.a <- pA4.a +  scale_linetype_manual(name = "Multiprey",
                                   values=c( "1" = "solid"),
                                   labels = c("Similar"))
pA4.a <- pA4.a +  scale_colour_manual(name = "Dissimilar cost in",
                                 values=c( "a" = "#CC79A7", "b" = "#E69F00", "c" =  "#F0E442", "d"="#009E73", "e" = "#0072B2","f" = "#56B4E9"),
                                 labels = c("Search Rate", "Attack Energy", "Attack Time", "Kill Probability", "Consume Energy", "Consume Time"))
pA4.a <- pA4.a +  guides(color = guide_legend(order = 2),
                     linetype = guide_legend(order = 1))
pA4.a


pA4.2c <- ggplot() + ggtitle("c.") 
pA4.2c <- pA4.2c + geom_line(data = setDT(FRDF)[Nx==max(Nx)],aes(x=Ny, y=fy_s/Ny,colour = "a"),size = 1.5)
#pA4.2c <- pA4.2c + geom_line(data = setDT(FRDF)[Nx==max(Nx)],aes(x=Ny, y=fy_es,colour = ""),size = 1)
pA4.2c <- pA4.2c + geom_line(data = setDT(FRDF)[Nx==max(Nx)],aes(x=Ny, y=(fy_ec/Ny)+.0002,colour = "b"), size = 1.5)
pA4.2c <- pA4.2c + geom_line(data = setDT(FRDF)[Nx==max(Nx)],aes(x=Ny, y=(fy_tc/Ny)+.0002,colour = "c"),size = 1.5)
pA4.2c <- pA4.2c + geom_line(data = setDT(FRDF)[Nx==max(Nx)],aes(x=Ny, y=fy_pk/Ny,colour = "d"),size = 1.5)
pA4.2c <- pA4.2c + geom_line(data = setDT(FRDF)[Nx==max(Nx)],aes(x=Ny, y=(fy_eh/Ny),colour = "e"),linetype = "dashed",size = 1.5)
pA4.2c <- pA4.2c + geom_line(data = setDT(FRDF)[Nx==max(Nx)],aes(x=Ny, y=(fy_th/Ny),colour = "f"),linetype = "dashed",size = 1.5)
pA4.2c <- pA4.2c + geom_line(data = setDT(FRDF)[Nx==max(Nx)],aes(x=Ny, y=fy_null/Ny, linetype = "1"),colour = "black",size = 1.5 , alpha = .9)
pA4.2c <- pA4.2c + ylim(0,.04) +xlim(0,10) 
pA4.2c <- pA4.2c + xlab("Prey Density") + ylab(bquote('Per Capita Predation'))
pA4.2c <- pA4.2c + theme(text = element_text(size = 20),
                   axis.line.y = element_line(color = "black"),
                   axis.ticks.y= element_line(color = "black"),
                   axis.text.y =element_text(colour = "black"),
                   axis.line.x = element_line(color = "grey30"),
                   axis.ticks.x = element_line(color = "grey30"),
                   axis.text.x = element_text(color = "grey30"),
                   axis.line = element_line(size=.8),
                   panel.background = element_rect(fill = "#eef6ec"),#FAF6FB
                   panel.grid.major =element_blank(),
                   panel.grid.minor = element_blank(),
                   plot.background = element_rect(fill = "#EEF0EF", colour = "black", size =1), 
                   plot.title = element_text(size = 20, colour = "grey30"),
                   legend.title = element_text(size = 20, colour = "grey30"),
                   legend.text = element_text(size=18, colour = "grey30"),
                   legend.key = element_blank(),
                   legend.justification=c(0,1), 
                   legend.position="none",
                   legend.background = element_blank())
pA4.2c <- pA4.2c + scale_linetype_manual(name = "Multiprey",
                                   values=c( "1" = "solid"),
                                   labels = c("Similar"))
pA4.2c <- pA4.2c + scale_colour_manual(name = "Dissimilar cost in",
                                 values=c( "a" = "#CC79A7", "b" = "#E69F00", "c" =  "#F0E442", "d"="#009E73", "e" = "#0072B2","f" = "#56B4E9"),
                                 labels = c("Search Rate", "Attack Energy", "Attack Time", "Kill Probability", "Consume Energy", "Consume Time"))
pA4.2c <- pA4.2c +  guides(color = guide_legend(order = 2),
                     linetype = guide_legend(order = 1))
pA4.2c


pA4.2b <- ggplot() + ggtitle("b.") 
pA4.2b <- pA4.2b + geom_line(data = setDT(FRDF)[Ny==min(Ny)],aes(x=Nx, y=fx_null/Nx, linetype = "1"),colour = "black",size = 1.5, alpha = .9)
pA4.2b <- pA4.2b + geom_line(data = setDT(FRDF)[Ny==min(Ny)],aes(x=Nx+0.001, y=(fx_s/Nx),colour = "a"),linetype = "dashed",size = 1.5)
pA4.2b <- pA4.2b + geom_line(data = setDT(FRDF)[Ny==min(Ny)],aes(x=Nx+0.001, y=(fx_ec/Nx)+.002,colour = "b"),linetype = "dashed",size = 1.5, alpha = 1)
pA4.2b <- pA4.2b + geom_line(data = setDT(FRDF)[Ny==min(Ny)],aes(x=Nx, y=(fx_tc/Nx)+0.001,colour = "c"),linetype = "dashed",size = 1.5, alpha = 1)
pA4.2b <- pA4.2b + geom_line(data = setDT(FRDF)[Ny==min(Ny)],aes(x=Nx, y=fx_pk/Nx,colour = "d"),linetype = "dashed",size = 1.5)
pA4.2b <- pA4.2b + geom_line(data = setDT(FRDF)[Ny==min(Ny)],aes(x=Nx+0.001, y=(fx_eh/Nx)+.002,colour = "e"),linetype = "dashed",size = 1, alpha = 1)
pA4.2b <- pA4.2b + geom_line(data = setDT(FRDF)[Ny==min(Ny)],aes(x=Nx, y=(fx_th/Nx)+.001,colour = "f"),linetype = "dashed",size = 1)
pA4.2b <- pA4.2b + ylim(0,.4) +xlim(0,10) 
pA4.2b <- pA4.2b + xlab("") + ylab('')
pA4.2b <- pA4.2b + theme(text = element_text(size = 20),
                  axis.line.y = element_line(color = "black"),
                  axis.ticks.y= element_line(color = "black"),
                  axis.text.y =element_text(colour = "black"),
                  axis.line.x = element_line(color = "grey30"),
                  axis.ticks.x = element_line(color = "grey30"),
                  axis.text.x = element_text(color = "grey30"),
                  axis.line = element_line(size=.8),
                  panel.background = element_rect(fill = "#F4ECF6"),
                  panel.grid.major =element_blank(),
                  panel.grid.minor = element_blank(),
                  plot.background = element_rect(fill = "white", colour = "black", size =1), #EEF0EF
                  plot.title = element_text(size = 20, colour = "grey30"),
                  legend.title = element_text(size = 20, colour = "grey30"),
                  legend.text = element_text(size=18, colour = "grey30"),
                  legend.key = element_blank(),
                  legend.justification=c(0,1), 
                  legend.position=c(.6,.95),
                  legend.background = element_rect(fill = "white"))
pA4.2b <- pA4.2b + scale_linetype_manual(name = "Multiprey",
                                    values=c( "1" = "solid"),
                                    labels = c("Similar"))
pA4.2b <- pA4.2b + scale_colour_manual(name = "Dissimilar cost in",
                                 values=c( "a" = "#CC79A7", "b" = "#E69F00", "c" =  "#F0E442", "d"="#009E73", "e" = "#0072B2","f" = "#56B4E9"),
                                 labels = c("Search Rate", "Attack Energy", "Attack Time", "Kill Probability", "Consume Energy", "Consume Time"))
pA4.2b <- pA4.2b +  guides(color = guide_legend(order = 2),
                     linetype = guide_legend(order = 1))



pA4.2b

pA4.2d<- ggplot() + ggtitle("d.")
pA4.2d<- pA4.2d+ geom_line(data = setDT(FRDF)[Ny==max(Ny)],aes(x=Nx, y=fx_s/Nx,colour = "a"),size = 1.5)
pA4.2d<- pA4.2d+ geom_line(data = setDT(FRDF)[Ny==max(Ny)],aes(x=Nx, y=fx_null/Nx, linetype = "1"),colour ="black",size = 1.5 , alpha = .9)
pA4.2d<- pA4.2d+ geom_line(data = setDT(FRDF)[Ny==max(Ny)],aes(x=Nx, y=(fx_ec/Nx)+.0002,colour = "b"),size = 1.5 , alpha = 1)
pA4.2d<- pA4.2d+ geom_line(data = setDT(FRDF)[Ny==max(Ny)],aes(x=Nx, y=(fx_tc/Nx)+.0002,colour = "c"),size = 1.5, alpha = 1)
pA4.2d<- pA4.2d+ geom_line(data = setDT(FRDF)[Ny==max(Ny)],aes(x=Nx, y=(fx_pk/Nx),colour = "d"),size = 1.5)
pA4.2d<- pA4.2d+ geom_line(data = setDT(FRDF)[Ny==max(Ny)],aes(x=Nx, y=(fx_eh/Nx),colour = "e"),linetype = "dashed",size = 1.5 , alpha = 1)
pA4.2d<- pA4.2d+ geom_line(data = setDT(FRDF)[Ny==max(Ny)],aes(x=Nx, y=(fx_th/Nx),colour = "f"),linetype = "dashed",size = 1.5)
pA4.2d<- pA4.2d+ ylim(0,.04) +xlim(0,10) 
pA4.2d<- pA4.2d+ xlab("Prey Density") + ylab(bquote(''))
pA4.2d<- pA4.2d+ theme(text = element_text(size = 20),
                  axis.line.y = element_line(color = "black"),
                  axis.ticks.y= element_line(color = "black"),
                  axis.text.y =element_text(colour = "black"),
                  axis.line.x = element_line(color = "grey30"),
                  axis.ticks.x = element_line(color = "grey30"),
                  axis.text.x = element_text(color = "grey30"),
                  axis.line = element_line(size=.8),
                  panel.background = element_rect(fill = "#F4ECF6"),
                  panel.grid.major =element_blank(),
                  panel.grid.minor = element_blank(),
                  plot.background = element_rect(fill = "#EEF0EF", colour = "black", size =1), 
                  plot.title = element_text(size = 20, colour = "grey30"),
                  legend.title = element_text(size = 20, colour = "grey30"),
                  legend.text = element_text(size=18, colour = "grey30"),
                  legend.key = element_blank(),
                  legend.justification=c(0,1), 
                  legend.position="none",
                  legend.background = element_blank())
pA4.2d<- pA4.2d+  scale_linetype_manual(name = "Multiprey",
                                  values=c( "1" = "solid"),
                                  labels = c("Similar"))
pA4.2d<- pA4.2d+ scale_colour_manual(name = "Dissimilar cost in",
                               values=c( "a" = "#CC79A7", "b" = "#E69F00", "c" =  "#F0E442", "d"="#009E73", "e" = "#0072B2","f" = "#56B4E9"),
                               labels = c("Search Rate", "Attack Energy", "Attack Time", "Kill Probability", "Consume Energy", "Consume Time"))
pA4.2d<- pA4.2d+  guides(color = guide_legend(order = 2),
                   linetype = guide_legend(order = 1))

pA4.2d


png("FigA4.1-pr2prey.tiff", width = 16000, height = 14000, res=1000, units="px")
grid.arrange(r,c,v,
             l,pA4.a,pA4.2b,
             h,pA4.2c,pA4.2d,
             ncol=3,
             nrow=3,
             widths = c(1,6,6),
             heights = c(1,6,6),
             top = textGrob("Predation Risk", just = .3, gp=gpar(fontsize=30))
)
dev.off()


###switching plot#####
##Figure 3 Main Text ##
## total density is constant, Ny: L116 rep is 1, Nx: L125.
p3a <- ggplot() + ggtitle("a.")
p3a <- p3a + geom_line(data = setDT(FRDF),aes(x=Ny/Nx, y=fy_null/fx_null, linetype = "1"),colour = "black",size = 1.5)
p3a <- p3a + geom_line(data = setDT(FRDF),aes(x=Ny/Nx, y=fy_s/fx_s,colour = "a"),size = 1.5)
p3a <- p3a + geom_line(data = setDT(FRDF),aes(x=Ny/Nx, y=(fy_ec/fx_ec),colour = "b"),size = 1.5)
p3a <- p3a + geom_line(data = setDT(FRDF),aes(x=Ny/Nx, y=(fy_tc/fx_tc),colour = "c"),size = 1.5)
p3a <- p3a + geom_line(data = setDT(FRDF),aes(x=Ny/Nx, y=fy_pk/fx_pk,colour = "d"),size = 1.5)
p3a <- p3a + geom_line(data = setDT(FRDF),aes(x=Ny/Nx, y=fy_eh/fx_eh,colour = "e"),linetype = "dashed",size = 1.5)
p3a <- p3a + geom_line(data = setDT(FRDF),aes(x=Ny/Nx, y=fy_th/fx_th,colour = "f"),linetype = "dashed",size = 1.5)
#p3a <- p3a + geom_line(data = setDT(FRDF),aes(x=Ny/Nx, y=fy_E/fx_E,colour = ""),size = 1)
p3a<- p3a + ylim(0,10) +xlim(0,10) 
p3a <- p3a + xlab("Relative Density") + ylab("Relative Consumption Rate")
p3a <- p3a + theme(text = element_text(size = 20),
                 axis.line.y = element_line(color = "grey30"),
                 axis.ticks.y= element_line(color = "grey30"),
                 axis.text.y =element_text(colour = "grey30"),
                 axis.line.x = element_line(color = "grey50"),
                 axis.ticks.x = element_line(color = "grey50"),
                 axis.text.x = element_text(color = "grey50"),
                 axis.line = element_line(size=.8),
                 panel.background  = element_blank(),
                 panel.grid.major = element_blank(),
                 panel.grid.minor = element_blank(),
                 plot.background = element_blank(),
                 plot.title = element_text(size = 20, colour = "grey30"),
                 legend.title = element_text(size = 20, colour = "grey30"),
                 legend.text = element_text(size=18, colour = "grey30"),
                 legend.key = element_blank(),
                 legend.justification=c(0,1), 
                 legend.position=c(.05,.99),
                 legend.background = element_blank())
p3a <- p3a + scale_linetype_manual(name = "Multiprey",
                                 values=c( "1" = "solid"),
                                 labels = c("Similar"))
p3a <- p3a +  scale_colour_manual(name = "Dissimilar cost in",
                                values=c( "a" = "#CC79A7", "b" = "#E69F00", "c" =  "#F0E442", "d"="#009E73", "e" = "#0072B2","f" = "#56B4E9"),
                                labels = c("Search Rate", "Attack Energy", "Attack Time", "Kill Probability", "Consume Energy", "Consume Time"))
p3a <- p3a + guides(color = guide_legend(order = 2),
                  linetype = guide_legend(order = 1))

p3a



png("Fig3a-switch.tiff", width = 8000, height = 7000, res=1000, units="px")
p3a
dev.off()

## absolute density changes
##relative density constant, Ny: L116, rep 1, Nx: L126 Nx=Ny
p3b <- ggplot() + ggtitle("b.")
p3b <- p3b + geom_line(data = setDT(FRDF),aes(x=Ny+Nx, y=fy_null/fx_null, linetype =  '1'),colour = "black",size = 1.5)
p3b <- p3b + geom_line(data = setDT(FRDF),aes(x=Ny+Nx, y=fy_s/fx_s,colour = "a"),size = 1.5)
p3b <- p3b + geom_line(data = setDT(FRDF),aes(x=Ny+Nx, y=(fy_ec/fx_ec)+0.002,colour = "b"),size = 1.5)
p3b <- p3b + geom_line(data = setDT(FRDF),aes(x=Ny+Nx, y=(fy_tc/fx_tc)+0.002,colour = "c"),size = 1.5)
p3b <- p3b + geom_line(data = setDT(FRDF),aes(x=Ny+Nx, y=fy_pk/fx_pk,colour = "d"),size = 1.5)
p3b <- p3b + geom_line(data = setDT(FRDF),aes(x=Ny+Nx, y=fy_eh/fx_eh,colour = "e"),linetype = "dashed",size = 1.5)
p3b <- p3b + geom_line(data = setDT(FRDF),aes(x=Ny+Nx, y=fy_th/fx_th,colour = "f"),linetype = "dashed",size = 1.5)
p3b<- p3b  +xlim(0,10) 
p3b <- p3b + xlab("Absolute Density") + ylab("")
p3b <- p3b + theme(text = element_text(size = 20),
                 axis.line.y = element_line(color = "grey30"),
                 axis.ticks.y= element_line(color = "grey30"),
                 axis.text.y =element_text(colour = "grey30"),
                 axis.line.x = element_line(color = "grey10"),
                 axis.ticks.x = element_line(color = "grey10"),
                 axis.text.x = element_text(color = "grey10"),
                 axis.line = element_line(size=.8),
                 panel.background  = element_blank(),
                 panel.grid.major = element_blank(),
                 panel.grid.minor = element_blank(),
                 plot.background = element_blank(), 
                 plot.title = element_text(size = 20, colour = "grey30"),
                 legend.title = element_text(size = 20, colour = "grey30"),
                 legend.text = element_text(size=18, colour = "grey30"),
                 legend.key = element_blank(),
                 legend.justification=c(0,1),  
                 legend.position="none",
                 legend.background = element_blank())
p3b <- p3b + scale_linetype_manual(name = "Multiprey",
                                 values=c( "1" = "solid"),
                                 labels = c("Similar"))
p3b <- p3b +  scale_colour_manual(name = "Dissimilar cost in",
                                values=c( "a" = "#CC79A7", "b" = "#E69F00", "c" =  "#F0E442", "d"="#009E73", "e" = "#0072B2","f" = "#56B4E9"),
                                labels = c("Search Rate", "Attack Energy", "Attack Time", "Kill Probability", "Consume Energy", "Consume Time"))
p3b <- p3b + guides(color = guide_legend(order = 2),
                  linetype = guide_legend(order = 1))

p3b

png("Fig3b-switch.tiff", width = 8000, height = 7000, res=1000, units="px")
p3b
dev.off()

####SINGLE PREY RESULTS#########
##Appendix 4###
##Nx = 0
## Cost diss is single value
###combined######
pA4.1a <- ggplot()  + ggtitle("a.")
pA4.1a <- pA4.1a + geom_segment(aes(x = 10, y = halffy, xend = 0, yend = halffy), arrow = NULL, colour  = "#999999", size= 1)
pA4.1a <- pA4.1a + geom_segment(aes(x = 10, y = halffy, xend = 0, yend = halffy), arrow = NULL,linetype = "dashed", colour  = "black", size= 1)
pA4.1a  <- pA4.1a + geom_segment(aes(x = .67, y = halffy, xend = .67, yend = .005), arrow = NULL, colour = "grey30", size= 1)
pA4.1a  <- pA4.1a + annotate("text", x = .7, y = 0, label = "N = .67", size = 7, colour = "grey30")
pA4.1a <- pA4.1a + geom_line(aes(x = Ny, y = fy_null),colour = "white", size= 1) 
pA4.1a <- pA4.1a + geom_line(aes(Ny,fe_null/1000), size= 1)
pA4.1a <- pA4.1a +  scale_y_continuous(sec.axis = sec_axis(~.*1000, name = 'Energy Consumed per Time'))
pA4.1a <- pA4.1a + xlab("Prey Density") + ylab('Prey Consumed per Time')
pA4.1a<- pA4.1a  +theme(text = element_text(size = 20),
                      axis.line.y.left = element_line(color = "black"),
                      axis.ticks.y.left = element_line(color = "black"),
                      axis.text.y.left =element_text(colour = "black"),
                      axis.line.y.right = element_line(color = "#999999"),
                      axis.ticks.y.right = element_line(color = "#999999"),
                      axis.text.y.right =element_text(colour = "#999999"),
                      axis.line.x = element_line(color = "grey30"),
                      axis.ticks.x = element_line(color = "grey30"),
                      axis.text.x = element_text(color = "grey30"),
                      axis.line = element_line(size=.8),
                      panel.background=element_blank(),
                      plot.background = element_blank(),
                      plot.title = element_text(size = 20, colour = "grey30"),
                      legend.position="none",
                      legend.background = element_blank())

pA4.1a

# png("Fig2aFRnull", width = 8000, height = 7000, res=1000, units="px")
# pA4.1a
# dev.off()

###prey units######

pA4.1c  <- ggplot() + ggtitle("c.")
pA4.1c <- pA4.1c + geom_segment(aes(x = 10, y = halffy, xend = 0, yend = halffy), arrow = NULL,linetype = "dashed", colour  =  "black", size= 1)
pA4.1c <- pA4.1c + geom_line(aes(Ny,fy_null, linetype = "1"),colour = "black", size= 1.5,alpha = .9) 
pA4.1c  <- pA4.1c + geom_line(aes(Ny,fy_s,colour = "a"), size= 1.5, alpha = 1) 
pA4.1c <- pA4.1c + geom_line(aes(Ny,fy_ec+.002,colour = "b"),linetype="dashed", size=1.5, alpha = 1) 
pA4.1c <- pA4.1c + geom_line(aes(Ny,fy_tc+.002,colour = "c"), size= 1.5, alpha = 1) 
pA4.1c  <- pA4.1c + geom_line(aes(Ny,fy_pk,colour = "d"), size= 1.5, alpha = 1) 
pA4.1c  <- pA4.1c + geom_line(aes(Ny,fy_eh-0.002,colour = "e"), size= 1.5,linetype="dashed",alpha = 1) 
pA4.1c  <- pA4.1c + geom_line(aes(Ny,fy_th,colour = "f"), size=1.5,linetype="dashed",alpha = 1) 
#pA4.1c  <- pA4.1c +ylim(0,.315) +xlim(0,10) 
pA4.1c  <- pA4.1c + xlab("Prey Density")  + ylab(bquote('Prey Consumed per Time'))
pA4.1c<- pA4.1c  +theme(text = element_text(size = 20),
                        axis.line.y = element_line(color = "black"),
                        axis.ticks.y= element_line(color = "black"),
                        axis.text.y =element_text(colour = "black"),
                        axis.line.x = element_line(color = "grey30"),
                        axis.ticks.x = element_line(color = "grey30"),
                        axis.text.x = element_text(color = "grey30"),
                        axis.line = element_line(size=.8),
                        panel.background=element_blank(), 
                        plot.title = element_text(size = 20, colour = "grey30"),
                        legend.title = element_text(size=20, colour = "grey30"),
                        legend.text = element_text(size=16, colour = "grey30"),
                        legend.key = element_blank(),
                        legend.justification=c(0,1),
                        legend.position= "none", # c(.05,.95),
                        legend.background = element_blank())
pA4.1c = pA4.1c + scale_linetype_manual(name = "Single prey",
                                        values=c( "1" = "solid"),
                                        labels = c("Vulnerable"))
pA4.1c = pA4.1c +   scale_colour_manual(name = "Costly in",
                                        values=c( "a" = "#CC79A7", "b" = "#E69F00", "c" =  "#F0E442", "d"="#009E73", "e" = "#0072B2","f" = "#56B4E9"),
                                        labels = c("Search Rate", "Attack Energy", "Attack Time", "Kill Probability", "Consume Energy", "Consume Time"))
pA4.1c = pA4.1c +  guides(color = guide_legend(order = 2),
                          linetype = guide_legend(order = 1))

pA4.1c 

# png("Fig4.1cFRnull.tiff", width = 8000, height = 7000, res=1000, units="px")
# pA4.1c
# dev.off()

## variation in Cost dissimilarity###########################################
###plots for Costs and half null values
#### L115: Ny = 10. L121: Nx = 0
pA4.1b = ggplot() + ggtitle("b.")
pA4.1b = pA4.1b + geom_hline(yintercept = halffe, linetype = "dashed", size = 1, colour = "grey30") 
pA4.1b = pA4.1b + geom_line(aes(dec[Ny==max(Ny)],fe_null[Ny==max(Ny)],linetype = "1"),colour = "black",  size = 1.5)
pA4.1b = pA4.1b + geom_line(aes(dec[Ny==max(Ny)],fe_ec[Ny==max(Ny)],colour = "b"), size = 1.5)
pA4.1b = pA4.1b + geom_line(aes(dtc[Ny==max(Ny)],fe_tc[Ny==max(Ny)],colour = "c"), size = 1.5)
pA4.1b = pA4.1b + geom_line(aes(dpk[Ny==max(Ny)],fe_pk[Ny==max(Ny)],colour = "d"), size = 1.5)
pA4.1b = pA4.1b + geom_line(aes(deh[Ny==max(Ny)],fe_eh[Ny==max(Ny)],colour = "e"), size = 1.5)
pA4.1b = pA4.1b + geom_line(aes(dth[Ny==max(Ny)],fe_th[Ny==max(Ny)],colour = "f"), size = 1.5)
pA4.1b = pA4.1b + geom_line(aes(1/ds[Ny==max(Ny)],fe_s[Ny==max(Ny)],colour = "a"), size = 1.5)
pA4.1b = pA4.1b + xlim(0,800) + ylim(0,315) 
pA4.1b = pA4.1b + xlab("Cost Dissimilarity")  + ylab(bquote('Energy Consumed per Time'))
pA4.1b= pA4.1b+ theme(text = element_text(size = 20),
                          axis.line.y = element_line(color =  "#999999"),
                          axis.ticks.y= element_line(color =  "#999999"),
                          axis.text.y =element_text(colour =  "#999999"),
                          axis.line.x = element_line(color = "grey30"),
                          axis.ticks.x = element_line(color = "grey30"),
                          axis.text.x = element_text(color = "grey30"),
                          axis.line = element_line(size=.8),
                          panel.background  = element_blank(),
                          panel.grid.major = element_blank(),
                          panel.grid.minor = element_blank(),
                          plot.background = element_blank(),
                          plot.title = element_text(size = 20, colour = "grey30"),
                          legend.title = element_text(size = 20, colour = "grey30"),
                          legend.text = element_text(size=18, colour = "grey30"),
                          legend.key = element_blank(),
                          legend.justification=c(0,1), 
                          legend.position=c(.65,.95),
                          legend.background = element_blank())
pA4.1b= pA4.1b+ scale_linetype_manual(name = "Single prey",
                                          values=c( "1" = "solid"),
                                          labels = c("Vulnerable"))
pA4.1b= pA4.1b+   scale_colour_manual(name = "Costly in",
                                          values=c( "a" = "#CC79A7", "b" = "#E69F00", "c" =  "#F0E442", "d"="#009E73", "e" = "#0072B2","f" = "#56B4E9"),
                                          labels = c("Search Rate", "Attack Energy", "Attack Time", "Kill Probability", "Consume Energy", "Consume Time"))
pA4.1b= pA4.1b+  guides(color = guide_legend(order = 2),
                            linetype = guide_legend(order = 1))

pA4.1b

 png("FigA4.1b-NRGdiss.tiff", width = 8000, height = 7000, res=1000, units="px")
 pA4.1b
 dev.off()


###energy units######
pA4.1d <- ggplot() + ggtitle("d.")
pA4.1d = pA4.1d + xlab("Prey Density") + ylab(bquote('Energy Consumed per Time'))
pA4.1d  = pA4.1d + geom_segment(aes(x = 10, y = halffe, xend = 0, yend = halffe), arrow = NULL,linetype = "dashed", colour = "#999999", size= 1)
pA4.1d  <- pA4.1d + geom_line(aes(Ny,fe_s,colour = "a"), size= 1.5, alpha = 1) 
pA4.1d <- pA4.1d + geom_line(aes(Ny,fe_ec+1,colour = "b"), size=1.5, alpha = 1) 
pA4.1d <- pA4.1d + geom_line(aes(Ny,fe_tc+1,colour = "c"), size= 1.5, alpha = 1) 
pA4.1d  <- pA4.1d + geom_line(aes(Ny,fe_pk,colour = "d"), size= 1.5, alpha = 1) 
pA4.1d  <- pA4.1d + geom_line(aes(Ny,fe_eh-1,colour = "e"),linetype="dashed", size= 1.5,alpha = 1) 
pA4.1d  <- pA4.1d + geom_line(aes(Ny,fe_th,colour = "f"),linetype="dashed", size= 1.5,alpha = 1) 
pA4.1d <- pA4.1d + geom_line(aes(Ny,fe_null, linetype = "1"),colour = "black", size= 1.5,alpha = .9)
pA4.1d = pA4.1d + theme(text = element_text(size = 20),
                        axis.line.y = element_line(color = "#999999"),
                        axis.ticks.y= element_line(color = "#999999"),
                        axis.text.y =element_text(colour = "#999999"),
                        axis.line.x = element_line(color = "grey30"),
                        axis.ticks.x = element_line(color = "grey30"),
                        axis.text.x = element_text(color = "grey30"),
                        axis.line = element_line(size=.8),
                        panel.background = element_blank(),
                        panel.grid.major =element_blank(),
                        panel.grid.minor = element_blank(),
                        plot.background = element_blank(),
                        plot.title = element_text(size = 20, colour = "grey30"),
                        legend.position="none",
                        legend.background = element_blank())
pA4.1d = pA4.1d + scale_linetype_manual(name = "Single prey",
                                        values=c( "1" = "solid"),
                                        labels = c("Vulnerable"))
pA4.1d = pA4.1d +   scale_colour_manual(name = "Costly in",
                                        values=c( "a" = "#CC79A7", "b" = "#E69F00", "c" =  "#F0E442", "d"="#009E73", "e" = "#0072B2","f" = "#56B4E9"),
                                        labels = c("Search Rate", "Attack Energy", "Attack Time", "Kill Probability", "Consume Energy", "Consume Time"))
pA4.1d = pA4.1d +  guides(color = guide_legend(order = 2),
                          linetype = guide_legend(order = 1))
pA4.1d 

# png("Fig4.1dFRnull", width = 8000, height = 7000, res=1000, units="px")
# pA4.1d
# dev.off()





############## Other plots ##################
####### consumption in energy units####
####Nx  = 0.1, Ny is range

###Cost diss is single value
p01 <- ggplot() + geom_segment(aes(x = 10, y = halffe, xend = 0, yend = halffe), arrow = NULL,linetype = "dashed", colour  = "black", size= 1) + ggtitle("a.")
p01 <- p01 + geom_line(data = setDT(FRDF),aes(x=Ny, y=fe_null, linetype="1"),colour = "black",size = 1.5, alpha = .9)
p01 <- p01 + geom_line(data = setDT(FRDF),aes(x=Ny, y=fe_s,colour = "a"),size = 1.5)
p01 <- p01 + geom_line(data = setDT(FRDF),aes(x=Ny, y=fe_ec+1,colour = "b"),size = 1.5, alpha = 1)
p01 <- p01 + geom_line(data = setDT(FRDF),aes(x=Ny, y=fe_tc+1,colour = "c"),size = 1.5, alpha =1)
p01 <- p01 + geom_line(data = setDT(FRDF),aes(x=Ny, y=fe_pk,colour = "d"),size = 1.5)
p01 <- p01 + geom_line(data = setDT(FRDF),aes(x=Ny, y=fe_eh-1,colour = "e"),linetype = "dashed", size = 1.5, alpha = 1)
p01 <- p01 + geom_line(data = setDT(FRDF),aes(x=Ny, y=fe_th-1,colour = "f"),linetype = "dashed",size = 1.5)
#p01 <- p01 + ylim(0,.315) +xlim(0,10) 
p01 <- p01 + xlab("Prey Density") + ylab(bquote('Energy Consumed per Time'))
p01 <- p01 + theme(text = element_text(size = 20),
                 axis.line.y = element_line(color = "#999999"),
                 axis.ticks.y= element_line(color = "#999999"),
                 axis.text.y =element_text(colour = "#999999"),
                 axis.line.x = element_line(color = "grey30"),
                 axis.ticks.x = element_line(color = "grey30"),
                 axis.text.x = element_text(color = "grey30"),
                 axis.line = element_line(size=.8),
                 panel.background = element_rect(fill = "#eef6ec"),#FAF6FB
                 panel.grid.major =element_blank(),
                 panel.grid.minor = element_blank(),
                 plot.background = element_rect(fill = "white"), #EEF0EF
                 plot.title = element_text(size = 20, colour = "grey30"),
                 legend.title = element_text(size = 20, colour = "grey30"),
                 legend.text = element_text(size=18, colour = "grey30"),
                 legend.key = element_blank(),
                 legend.justification=c(0,1), 
                 legend.position="none",
                 legend.background = element_blank())
p01 <- p01 +  scale_linetype_manual(name = "Multiprey",
                                   values=c( "1" = "solid"),
                                   labels = c("Similar"))
p01 <- p01 + scale_colour_manual(name = "Dissimilar cost in",
                                 values=c( "a" = "#CC79A7", "b" = "#E69F00", "c" =  "#F0E442", "d"="#009E73", "e" = "#0072B2","f" = "#56B4E9"),
                                 labels = c("Search Rate", "Attack Energy", "Attack Time", "Kill Probability", "Consume Energy", "Consume Time"))
p01 <- p01 +  guides(color = guide_legend(order = 2),
                    linetype = guide_legend(order = 1))


p01

# png("frNRG_minNx", width = 8000, height = 7000, res=1000, units="px")
# p01
# dev.off()





####Nx  = 10
p02 <- ggplot() + geom_segment(aes(x = 10, y = halffe, xend = 0, yend = halffe), arrow = NULL,linetype = "dashed", colour  = "black", size= 1) + ggtitle("c.") 
p02 <- p02 + geom_line(data = setDT(FRDF),aes(x=Ny, y=fe_null, linetype = "1"),colour = "black",size = 1.5 , alpha = .9)
p02 <- p02 + geom_line(data = setDT(FRDF),aes(x=Ny, y=fe_s,colour = "a"),size = 1.5)
p02 <- p02 + geom_line(data = setDT(FRDF),aes(x=Ny, y=fe_ec+2,colour = "b"),size = 1.5)
p02 <- p02 + geom_line(data = setDT(FRDF),aes(x=Ny, y=fe_tc+2,colour = "c"),size = 1.5 )
p02 <- p02 + geom_line(data = setDT(FRDF),aes(x=Ny, y=fe_pk,colour = "d"),size = 1.5)
p02 <- p02 + geom_line(data = setDT(FRDF),aes(x=Ny, y=fe_eh,colour = "e"),linetype = "dashed",size = 1.5)
p02 <- p02 + geom_line(data = setDT(FRDF),aes(x=Ny, y=fe_th,colour = "f"),linetype = "dashed",size = 1.5)
p02 <- p02 + ylim(0,325) +xlim(0,10) 
p02 <- p02 + xlab("Prey Density") + ylab(bquote('Energy Consumed per Time'))
p02 <- p02 + theme(text = element_text(size = 20),
                   axis.line.y = element_line(color = "#999999"),
                   axis.ticks.y= element_line(color = "#999999"),
                   axis.text.y =element_text(colour = "#999999"),
                   axis.line.x = element_line(color = "grey30"),
                   axis.ticks.x = element_line(color = "grey30"),
                   axis.text.x = element_text(color = "grey30"),
                   axis.line = element_line(size=.8),
                   panel.background = element_rect(fill = "#eef6ec"),#FAF6FB
                   panel.grid.major =element_blank(),
                   panel.grid.minor = element_blank(),
                   plot.background = element_rect(fill = "#EEF0EF"), 
                   plot.title = element_text(size = 20, colour = "grey30"),
                   legend.title = element_text(size = 20, colour = "grey30"),
                   legend.text = element_text(size=18, colour = "grey30"),
                   legend.key = element_blank(),
                   legend.justification=c(0,1), 
                   legend.position="none",
                   legend.background = element_blank())
p02 <- p02 + scale_linetype_manual(name = "Multiprey",
                                 values=c( "1" = "solid"),
                                 labels = c("Similar"))
p02 <- p02 + scale_colour_manual(name = "Dissimilar cost in",
                                values=c( "a" = "#CC79A7", "b" = "#E69F00", "c" =  "#F0E442", "d"="#009E73", "e" = "#0072B2","f" = "#56B4E9"),
                                labels = c("Search Rate", "Attack Energy", "Attack Time", "Kill Probability", "Consume Energy", "Consume Time"))
p02 <- p02 + guides(color = guide_legend(order = 2),
                  linetype = guide_legend(order = 1))
p02

# png("frNRG_maxNx", width = 8000, height = 7000, res=1000, units="px")
# p02
# dev.off()

####Ny = 0.1
p02 <- ggplot() + geom_segment(aes(x = 10, y = halffe, xend = 0, yend = halffe), arrow = NULL,linetype = "dashed", colour  = "black", size= 1) + ggtitle("b.") 
p02 <- p02 + geom_line(data = setDT(FRDF),aes(x=Nx, y=fe_null,linetype = "1"),colour = "black",size = 1.5, alpha = .9)
p02 <- p02 + geom_line(data = setDT(FRDF),aes(x=Nx, y=fe_s+5,colour = "a"), linetype = "dashed",size = 1.5)
p02 <- p02 + geom_line(data = setDT(FRDF),aes(x=Nx, y=fe_ec+5,colour = "b"),linetype = "dashed",size = 1.5, alpha = 1)
p02 <- p02 + geom_line(data = setDT(FRDF),aes(x=Nx+0.01, y=fe_tc+5,colour = "c"),linetype = "dashed",size = 1.5, alpha = 1)
p02 <- p02 + geom_line(data = setDT(FRDF),aes(x=Nx+.01, y=fe_pk,colour = "d"),linetype = "dashed",size = 1.5)
p02 <- p02 + geom_line(data = setDT(FRDF),aes(x=Nx, y=fe_eh-2,colour = "e"),linetype = "dashed",size = 1.5, alpha = 1)
p02 <- p02 + geom_line(data = setDT(FRDF),aes(x=Nx+.01, y=fe_th-2,colour = "f"),linetype = "dashed",size = 1.5)
p02 <- p02 + ylim(0,325) +xlim(0,10) 
p02 <- p02 + xlab("Prey Density") + ylab('Energy Consumed per Time')
p02 <- p02 + theme(text = element_text(size = 20),
                 axis.line.y = element_line(color = "#999999"),
                 axis.ticks.y= element_line(color = "#999999"),
                 axis.text.y =element_text(colour = "#999999"),
                 axis.line.x = element_line(color = "grey30"),
                 axis.ticks.x = element_line(color = "grey30"),
                 axis.text.x = element_text(color = "grey30"),
                 axis.line = element_line(size=.8),
                 panel.background = element_rect(fill = "#F4ECF6"),
                 panel.grid.major =element_blank(),
                 panel.grid.minor = element_blank(),
                 plot.background = element_rect(fill = "white"), #EEF0EF
                 plot.title = element_text(size = 20, colour = "grey30"),
                 legend.title = element_text(size = 20, colour = "grey30"),
                 legend.text = element_text(size=18, colour = "grey30"),
                 legend.key = element_blank(),
                 legend.justification=c(0,1), 
                 legend.position=c(.65,.9),
                 legend.background = element_blank())
p02 <- p02 + scale_linetype_manual(name = "Multiprey",
                                 values=c( "1" = "solid"),
                                 labels = c("Similar"))
p02 <- p02 +  scale_colour_manual(name = "Dissimilar cost in",
                                values=c( "a" = "#CC79A7", "b" = "#E69F00", "c" =  "#F0E442", "d"="#009E73", "e" = "#0072B2","f" = "#56B4E9"),
                                labels = c("Search Rate", "Attack Energy", "Attack Time", "Kill Probability", "Consume Energy", "Consume Time"))
p02 <- p02 + guides(color = guide_legend(order = 2),
                  linetype = guide_legend(order = 1))
p02

#  png("frNRG_minNy", width = 8000, height = 7000, res=1000, units="px")
#  p02
# dev.off()

####Ny  = 10
p03 <- ggplot() + geom_segment(aes(x = 10, y = halffe, xend = 0, yend = halffe), arrow = NULL,linetype = "dashed", colour  = "black", size= 1) + ggtitle("d.")
p03 <- p03 + geom_line(data = setDT(FRDF),aes(x=Nx, y=fe_null, linetype = "1"),colour ="black",size = 1.5, alpha = .9)
p03 <- p03 + geom_line(data = setDT(FRDF),aes(x=Nx, y=fe_s,colour = "a"),size = 1.5)
p03 <- p03 + geom_line(data = setDT(FRDF),aes(x=Nx, y=fe_ec+2,colour = "b"),size = 1.5, alpha = 1)
p03 <- p03 + geom_line(data = setDT(FRDF),aes(x=Nx, y=fe_tc+2,colour = "c"),size = 1.5, alpha = 1)
p03 <- p03 + geom_line(data = setDT(FRDF),aes(x=Nx, y=fe_pk,colour = "d"),size = 1.5)
p03 <- p03 + geom_line(data = setDT(FRDF),aes(x=Nx, y=fe_eh,colour = "e"),linetype = "dashed", size = 1.5, alpha = 1)
p03 <- p03 + geom_line(data = setDT(FRDF),aes(x=Nx, y=fe_th,colour = "f"),linetype = "dashed",size = 1.5)
p03 <- p03 + ylim(0,325) +xlim(0,10) 
p03 <- p03 + xlab("Prey Density") + ylab(bquote('Energy Consumed per Time'))
p03 <- p03 + theme(text = element_text(size = 20),
                  axis.line.y = element_line(color = "#999999"),
                  axis.ticks.y= element_line(color = "#999999"),
                  axis.text.y =element_text(colour = "#999999"),
                  axis.line.x = element_line(color = "grey30"),
                  axis.ticks.x = element_line(color = "grey30"),
                  axis.text.x = element_text(color = "grey30"),
                  axis.line = element_line(size=.8),
                  panel.background = element_rect(fill = "#F4ECF6"),
                  panel.grid.major =element_blank(),
                  panel.grid.minor = element_blank(),
                  plot.background = element_rect(fill = "#EEF0EF"), 
                  plot.title = element_text(size = 20, colour = "grey30"),
                  legend.title = element_text(size = 20, colour = "grey30"),
                  legend.text = element_text(size=18, colour = "grey30"),
                  legend.key = element_blank(),
                  legend.justification=c(0,1), 
                  legend.position="none",
                  legend.background = element_blank())

p03 <- p03 + scale_linetype_manual(name = "Multiprey",
                                 values=c( "1" = "solid"),
                                 labels = c("Similar"))
p03 <- p03 +scale_colour_manual(name = "Dissimilar cost in",
                                values=c( "a" = "#CC79A7", "b" = "#E69F00", "c" =  "#F0E442", "d"="#009E73", "e" = "#0072B2","f" = "#56B4E9"),
                                labels = c("Search Rate", "Attack Energy", "Attack Time", "Kill Probability", "Consume Energy", "Consume Time"))
p03 <- p03 + guides(color = guide_legend(order = 2),
                 linetype = guide_legend(order = 1))
p03

# png("frNRG_maxNy", width = 8000, height = 7000, res=1000, units="px")
# p03
# dev.off()





#####preference and cost dissimilarity
pd <- ggplot() 
pd <- pd + geom_hline(yintercept=1, colour = "black", size = 1)
pd <- pd + geom_line(data = setDT(FRDF),aes(x=1/ds, y=fy_s/fx_s,colour = "a"),size = 1.5)
#pd <- pd + geom_line(data = setDT(FRDF),aes(x=ds, y=fy_es/fx_es,colour = ""),size = 1)
pd <- pd + geom_line(data = setDT(FRDF),aes(x=dec, y=fy_ec/fx_ec,colour = "b"),size = 1.5)
pd <- pd + geom_line(data = setDT(FRDF),aes(x=dtc, y=fy_tc/fx_tc,colour = "c"),linetype = "dashed",size = 1.5)
pd <- pd + geom_line(data = setDT(FRDF),aes(x=1/dpk, y=fy_pk/fx_pk,colour = "d"),linetype = "dashed",size = 1.5)
pd <- pd + geom_line(data = setDT(FRDF),aes(x=deh, y=fy_eh/fx_eh,colour = "e"),size = 1.5)
pd <- pd + geom_line(data = setDT(FRDF),aes(x=dth, y=fy_th/fx_th,colour = "f"),linetype = "dashed",size = 1.5)
pd <- pd + xlab("Cost Dissimilarity") + ylab("")
pd <- pd +  ylim(0,1) +xlim(0,1000)
pd <- pd + theme(text = element_text(size=20),
                 panel.background=element_blank(), plot.title = element_text(size = 20, colour = "grey30"),
                 axis.line = element_line(colour = "black",size=.8),
                 legend.title = element_text(size=20, colour = "grey30"),
                 legend.text = element_text(size=16, colour = "grey30"),
                 legend.key = element_blank(),
                 legend.justification=c(0,1),
                 legend.position="none",
                 legend.background = element_blank())
pd <- pd  +  scale_colour_manual(name = "Antipredator cost\ninfluences",
                                 values=c( "a" = "#CC79A7", "b" = "#E69F00", "c" =  "#F0E442", "d"="#009E73", "e" = "#0072B2","f" = "#56B4E9"),
                                 labels = c("Search Rate", "Attack Energy", "Attack Time", "Kill Probability", "Consume Energy", "Consume Time"))
pd <- pd +  annotate("text", x = 850, y = .97 , label = "Costly:Vulnerable", size=6)
pd


png("SwitchDiss", width = 8000, height = 7000, res=1000, units="px")
grid.arrange(td)
dev.off()

#### get legend seperately
# Extract the legend. Returns a gtable
leg <- get_legend(p1)
grid.newpage()
grid.draw(leg)


#################### checks#######################################


###probability of engrafement pe plots
### looks fine
pe <- ggplot() 
pe <- pe + geom_line(data = setDT(FRDF),aes(x=Nx, y=pey_null),colour = "black",size = 2)
pe <- pe + geom_point(data = setDT(FRDF),aes(x=Nx, y=pey_s-0.002,colour = "a"),size = 2)
pe <- pe + geom_point(data = setDT(FRDF),aes(x=Nx, y=pey_ec+0.002,colour = "b"),size = 2)
pe <- pe + geom_point(data = setDT(FRDF),aes(x=Nx, y=pey_tc+0.002,colour = "c"),size = 2)
pe <- pe + geom_point(data = setDT(FRDF),aes(x=Nx,y=pey_pk,colour = "d"),size = 2)
pe <- pe + geom_point(data = setDT(FRDF),aes(x=Nx, y=pey_eh,colour = "e"),size = 2)
pe <- pe + geom_point(data = setDT(FRDF),aes(x=Nx,y=pey_th,colour = "f"),size = 2)
pe <- pe + xlab("Vulnerable Prey Density") + ylab("Engagement with Costly Prey")
pe <- pe + theme(text = element_text(size = 20),
                 panel.background=element_blank(), plot.title = element_text(hjust=-0.06),
                 axis.line = element_line(colour = "black",size=.8),
                 legend.title = element_text(size=20, colour = "grey30"),
                 legend.text = element_text(size=16, colour = "grey30"),
                 legend.key = element_blank(),
                 legend.justification=c(0,1),
                 legend.position=c(0.65, .9),
                 legend.background = element_blank())
pe <- pe  +  scale_colour_manual(name = "Foraging cost\ninfluences",
                                 values=c( "a" = "#CC79A7", "b" = "#E69F00", "c" =  "#F0E442", "d"="#009E73", "e" = "#0072B2","f" = "#56B4E9"),
                                 labels = c("Search Rate", "Attack Energy", "Attack Time", "Kill Probability", "Consume Energy", "Consume Time"))
pe


# png("pe_prob2pry", width = 8500, height = 7000, res=1000, units="px")
# pe
# dev.off()

####always engages with the vulnerable prey
pe2 <- ggplot() 
pe2 <- pe2 + geom_line(data = setDT(FRDF),aes(x=Ny, y=pex_s,colour = "a"),size = 1)
#pe2 <- pe2 + geom_line(data = setDT(FRDF),aes(x=Ny, y=pex_es,colour = ""),size = 1)
pe2 <- pe2 + geom_line(data = setDT(FRDF),aes(x=Ny, y=pex_ec,colour = "b"),size = 1)
pe2 <- pe2 + geom_line(data = setDT(FRDF),aes(x=Ny, y=pex_tc,colour = "c"),size = 1)
pe2 <- pe2 + geom_line(data = setDT(FRDF),aes(x=Ny,y=pex_th,colour = "d"),size = 1)
pe2 <- pe2 + geom_line(data = setDT(FRDF),aes(x=Ny, y=pex_eh,colour = "e"),size = 1)
pe2 <- pe2 + geom_line(data = setDT(FRDF),aes(x=Ny,y=pex_th,colour = "f"),size = 1)
#pe2 <- pe2 + geom_line(data = setDT(FRDF),aes(x=Ny, y=pex_E,colour = ""),size = 1)
pe2 <- pe2 + geom_line(data = setDT(FRDF),aes(x=Ny, y=pex_null),colour = "#999999",size = 1)
pe2 <- pe2  +  scale_colour_manual(name = "Foraging cost\ninfluences",
                                   values=c( "a" = "#CC79A7", "b" = "#E69F00", "c" =  "#F0E442", "d"="#009E73", "e" = "#0072B2","f" = "#56B4E9"),
                                   labels = c("Search Rate", "Attack Energy", "Attack Time", "Kill Probability", "Consume Energy", "Consume Time"))

pe2


###energetic consumption check
p3 <- ggplot() +ggtitle("b. Deterministic")
p3 <- p3 + geom_point(data = setDT(FRDF),aes(x=Nx, y=fe_null),colour = "black",size = 1)
p3 <- p3 + geom_point(data = setDT(FRDF),aes(x=Nx, y=fe_s,colour = "a"),size = 1)
p3 <- p3 + geom_point(data = setDT(FRDF),aes(x=Nx+0.1, y=fe_ec+1,colour = "b"),size = 1)
p3 <- p3 + geom_point(data = setDT(FRDF),aes(x=Nx, y=fe_tc+1,colour = "c"),size = 1)
p3 <- p3 + geom_point(data = setDT(FRDF),aes(x=Nx, y=fe_pk,colour = "d"),size = 1)
p3 <- p3 + geom_point(data = setDT(FRDF),aes(x=Nx+0.1, y=fe_eh-1,colour = "e"),size = 1)
p3 <- p3 + geom_point(data = setDT(FRDF),aes(x=Nx, y=fe_th-1,colour = "f"),size = 1)
p3<- p3 + xlab("Vulnerable Prey Density") + ylab(bquote('Consumption Rate (energy*time'^-1*')'))
p3<- p3 + theme(text = element_text(size = 20),
                panel.background=element_blank(), plot.title = element_text(hjust=-0.06),
                axis.line = element_line(colour = "black",size=.8),
                legend.title = element_text(size=20, colour = "grey30"),
                legend.text = element_text(size=16, colour = "grey30"),
                legend.key = element_blank(),
                legend.justification=c(0,1),
                legend.position=c(0.65, .8),
                legend.background = element_blank())
p3 <- p3  +  scale_colour_manual(name = "Foraging cost\ninfluences",
                                 values=c( "a" = "#CC79A7", "b" = "#E69F00", "c" =  "#F0E442", "d"="#009E73", "e" = "#0072B2","f" = "#56B4E9"),
                                 labels = c("Search Rate", "Attack Energy", "Attack Time", "Kill Probability", "Consume Energy", "Consume Time"))

p3 

# png("NRG_check-pedet", width = 8000, height = 7000, res=1000, units="px")
# p3
# dev.off()
