#Create a walk using the digits of pi to decide the direction to step into
#Created by Nadieh Bremer - VisualCinnamon.com

library(ggplot2)
library(fields)

#Read in pi string, remove point and turn into vector
piChar <- read.table("https://raw.githubusercontent.com/nbremer/artinpi/master/data/PI_1000000.txt", stringsAsFactors=F, colClasses = c("character"))[1,1]
# piChar <- gsub(".","",piChar)
piVec <- as.numeric(strsplit(piChar, "")[[1]])
piVec <- piVec[1:10000]
#Starting at number 3 at position 0,0
x <- y <- rep(NULL, length(piVec))
x[1] <- 0
y[1] <- 0

#Calculate new position for each digit, based on the position of the old digit and the 
#angle is determined by the digit itself
for (i in 2:length(piVec)){
  x[i] <- x[(i-1)] + sin((pi*2)*(piVec[i]/10))
  y[i] <- y[(i-1)] + cos((pi*2)*(piVec[i]/10))  
}#for i


#Several color schemes
colors <- c("#790c40","#c82f41","#e86c3c","#e6b25d","#e8ecb8","#abdc6b","#60ce43","#1cb53b","#008c43","#005540")
cubeColors <- c("#6d3fa9","#bf3cae","#fe4b82","#ff7746","#e2b72e","#afef5a","#52f566","#1ddea3","#23aad8","#4c6edb","#6d3fa9")
heatColors <- c("#790c40","#c82f41","#e86c3c","#e6b25d")
rainbowCol <- c("#6363FF", "#6373FF", "#63A3FF", "#63E3FF", "#63FFFB", "#63FFCB","#63FF9B", "#63FF6B", "#7BFF63", "#BBFF63", 
                "#DBFF63", "#FBFF63", "#FFD363", "#FFB363", "#FF8363", "#FF7363", "#FF6364")
greenColors <- c("#8be990","#64bf8b","#349584","#206a81","#263c81","#000080")
blueColors <- c("#a8ddb5","#7bccc4","#4eb3d3","#2b8cbe","#0868ac","#084081")
rainbowColDark <- c("#EFB605","#EB8612","#DD0030","#B3005A","#80348F","#3365AA","#07A071","#7EB852")
izzycols <- c("#97C7FF","#FCE9E1","#f0b754","#ECACFC","#A8FFF1")
izzycolsmedium <- c("#97C7FF","#DADEEB")
izzycolsshort <- c("#97C7FF", "#9ECAFD")
rainbowColDark <- designer.colors(n=10 , col=izzycolsshort)


#To loop over a color scheme every Ncols digits
Ncols <- 500
ColRamp <- designer.colors(n=Ncols, col=cubeColors)

#Save all information in a data frame for plotting with ggplot2
Pi.frame <- data.frame(PI=piVec[-1], x=x[-length(x)], y=y[-length(y)], 
                       ID=1:(length(x)-1), ColID = 1:(length(x)-1)%%Ncols, stringsAsFactors=F)


#Color the line according to the number in pi, starting with 1 (3 = (0,0))
N <- 100000
piPlot <- ggplot(Pi.frame[1:N,], aes(x=x, y=y, group="1")) +
  geom_path(aes(color = factor(Pi.frame$PI[1:N])), size=0.7) + 
  scale_colour_manual(values = rainbowColDark) +
  #scale_colour_gradientn(colours = rainbowColDark) +
  #geom_point(aes(x=0, y=0), size=4, color="black") +
  #geom_point(aes(x=x[length(x)], y=y[length(y)]), size=4, color="black") +
  coord_fixed(ratio = 1) + 
  theme_bw() +
  theme(line = element_blank(),
        text = element_blank(),
        # line = element_blank(),
        title = element_blank(),
        legend.position="none",
        panel.border = element_blank(),
        panel.background = element_blank()) +
  theme(panel.background = element_rect(fill = 'black', colour = 'black'))

plot(piPlot)
ggsave(plot=piPlot, filename=paste("piPlot_",N,"_ColorRainbowDark.pdf",sep=""))
#ggsave(plot=piPlot, filename="piPlot_1e6_IDColor_Big_Thick.png",
#       scale=4, units="cm")

#Color according to position in pi, thus looping throug the chosen colors once
N <- 10000
piPlot <- ggplot(Pi.frame[1:N,], aes(x=x, y=y, group="1")) +
  geom_path(aes(color = ID), size=1.5) + 
  scale_colour_gradientn(colours = rainbowColDark) +
  #geom_point(aes(x=0, y=0), size=4, color="black") +
  #geom_point(aes(x=x[length(x)], y=y[length(y)]), size=4, color="black") +
  coord_fixed(ratio = 1) + 
  theme_bw() +
  theme(line = element_blank(),
        text = element_blank(),
        # line = element_blank(),
        title = element_blank(),
        legend.position="none",
        panel.border = element_blank(),
        panel.background = element_blank())
  #theme(panel.background = element_rect(fill = 'black', colour = 'black'))
plot(piPlot)
ggsave(plot=piPlot, filename=paste("piPlot_",N,"_IDColorRainbowDark_Smooth.pdf",sep=""))
#ggsave(plot=piPlot, filename=paste("piPlot_",N,"_IDColorRainbowDark_Smooth.jpeg",sep=""),
#       scale=2, units="cm")

#Loop through colors every Ncol positions
piPlot <- ggplot(Pi.frame, aes(x=x, y=y)) +
  geom_path(aes(color = ColID), size=0.25) +
  scale_colour_gradientn(colours = ColRamp) +
  geom_point(aes(x=0, y=0), size=4, color="black") +
  geom_point(aes(x=x[length(x)], y=y[length(y)]), size=4, color="black") +
  coord_fixed(ratio = 1) + 
  theme_bw() +
  theme(line = element_blank(),
        text = element_blank(),
        # line = element_blank(),
        title = element_blank(),
        legend.position="none",
        panel.border = element_blank(),
        panel.background = element_blank())
plot(piPlot)
ggsave(plot=piPlot, filename="piPlot_1e6_IDColorRainbow_Cube.pdf")

#Color the graph by using location in pi and x or y position - Loop through once
piPlot <- ggplot(Pi.frame, aes(x=x, y=ID)) +
  geom_path(aes(color = ID), size=0.25) +
  scale_colour_gradientn(colours = cubeColors) +
  theme_bw() +
  theme(line = element_blank(),
        text = element_blank(),
        # line = element_blank(),
        title = element_blank(),
        legend.position="none",
        panel.border = element_blank(),
        panel.background = element_blank())
plot(piPlot)
ggsave(plot=piPlot, filename="piPlot_1e6_IDColorRainbow_Cube_xID_Smooth.pdf")

#Color the graph by using location in pi and x or y position - Loop through every Ncol positions
piPlot <- ggplot(Pi.frame, aes(x=y, y=ID)) +
  geom_path(aes(color = ColID), size=0.25) +
  scale_colour_gradientn(colours = ColRamp) +
  theme_bw() +
  theme(line = element_blank(),
        text = element_blank(),
        # line = element_blank(),
        title = element_blank(),
        legend.position="none",
        panel.border = element_blank(),
        panel.background = element_blank())
plot(piPlot)
ggsave(plot=piPlot, filename="piPlot_1e6_IDColorRainbow_Cube_yID.pdf")

#Plot position and number 
piPlot <- ggplot(Pi.frame[1:1000,], aes(x=PI, y=ID)) +
  geom_point(aes(color = factor(PI)), size=2) +
  #geom_path(aes(color = "grey"), size=0.5) +
  #scale_colour_gradientn(colours = ColRamp) +
  theme_bw() +
  theme(line = element_blank(),
        text = element_blank(),
        # line = element_blank(),
        title = element_blank(),
        legend.position="none",
        panel.border = element_blank(),
        panel.background = element_blank())
plot(piPlot)
