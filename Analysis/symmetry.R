symmetry<-function(X){
#this routine segments the image into 16 regions and calculates symmetry 
library(dplyr)
rudy<-magick::image_read(images$local_path)
ZZZZ<-imager::magick2cimg(rudy)
ZZZZZ<-as.data.frame(ZZZZ)
ZZZZZ<-ZZZZZ%>%
  mutate(color=value*255)
#segmentation task
Q<-max(ZZZZZ$y)
P<-max(ZZZZZ$x)
#select quarter regions
#regions start in the upper left and head for bottom right
#select regions
#TOP ROW
R1<-ZZZZZ%>%
  filter(x >0 & x < .25*Q)%>%
  filter(y >0 & y < .25*P)

R2<-ZZZZZ%>%
  filter(x >.25*Q & x < .5*Q)%>%
  filter(y >0 & y < .25*P)

R3<-ZZZZZ%>%
  filter(x >.5*Q & x < .75*Q)%>%
  filter(y >0 & y < .25*P)

R4<-ZZZZZ%>%
  filter(x >.75*Q & x < Q)%>%
  filter(y >0 & y < .25*P)

#UPPER MIDDLE ROW
R5<-ZZZZZ%>%
  filter(x >0 & x < .25*Q)%>%
  filter(y >.25*P & y < .5*P)

R6<-ZZZZZ%>%
  filter(x >.25*Q & x < .5*Q)%>%
  filter(y >.25*P & y < .5*P)

R7<-ZZZZZ%>%
  filter(x >.5*Q & x < .75*Q)%>%
  filter(y >.25*P & y < .5*P)

R8<-ZZZZZ%>%
  filter(x >.75*Q & x < Q)%>%
  filter(y >.25*P & y < .5*P)

#LOWER MIDDLE ROW
R9<-ZZZZZ%>%
  filter(x >0 & x < .25*Q)%>%
  filter(y >.5*P & y < .75*P)

R10<-ZZZZZ%>%
  filter(x >.25*Q & x < .5*Q)%>%
  filter(y >.5*P & y < .75*P)

R11<-ZZZZZ%>%
  filter(x >.5*Q & x < .75*Q)%>%
  filter(y >.5*P & y < .75*P)

R12<-ZZZZZ%>%
  filter(x >.75*Q & x < Q)%>%
  filter(y >.5*P & y < .75*P)

#bottom row
R13<-ZZZZZ%>%
  filter(x >0 & x < .25*Q)%>%
  filter(y >.75*P & y < P)

R14<-ZZZZZ%>%
  filter(x >.25*Q & x < .5*Q)%>%
  filter(y >.75*P & y < P)

R15<-ZZZZZ%>%
  filter(x >.5*Q & x < .75*Q)%>%
  filter(y >.75*P & y < P)

R16<-ZZZZZ%>%
  filter(x >.75*Q & x < Q)%>%
  filter(y >.75*P & y < P)


#color function
hor_sym1<-R1$color-R4$color
hor_sym2<-R2$color-R3$color
hor_sym3<-R5$color-R8$color
hor_sym4<-R6$color-R7$color
hor_sym5<-R9$color-R12$color
hor_sym6<-R10$color-R11$color
hor_sym7<-R13$color-R16$color
hor_sym8<-R14$color-R15$color

ver_sym1<-R1$color-R13$color
ver_sym2<-R2$color-R14$color
ver_sym3<-R3$color-R15$color
ver_sym4<-R4$color-R16$color
ver_sym5<-R5$color-R9$color
ver_sym6<-R6$color-R10$color
ver_sym7<-R7$color-R11$color
ver_sym8<-R8$color-R12$color

images_symmetry<<-data.frame(images, mean(hor_sym1),
kurtosis(hor_sym1),
skewness(hor_sym1),
mean(hor_sym2),
kurtosis(hor_sym2),
skewness(hor_sym2),
mean(hor_sym3),
kurtosis(hor_sym3),
skewness(hor_sym3),
mean(hor_sym4),
kurtosis(hor_sym4),
skewness(hor_sym4),
mean(hor_sym5),
kurtosis(hor_sym5),
skewness(hor_sym5),
mean(hor_sym6),
kurtosis(hor_sym6),
skewness(hor_sym6),
mean(hor_sym7),
kurtosis(hor_sym7),
skewness(hor_sym7),
mean(hor_sym8),
kurtosis(hor_sym8),
skewness(hor_sym8),
mean(ver_sym1),
kurtosis(ver_sym1),
skewness(ver_sym1),
mean(ver_sym2),
kurtosis(ver_sym2),
skewness(ver_sym2),
mean(ver_sym3),
kurtosis(ver_sym3),
skewness(ver_sym3),
mean(ver_sym4),
kurtosis(ver_sym4),
skewness(ver_sym4),
mean(ver_sym5),
kurtosis(ver_sym5),
skewness(ver_sym5),
mean(ver_sym6),
kurtosis(ver_sym6),
skewness(ver_sym6),
mean(ver_sym7),
kurtosis(ver_sym7),
skewness(ver_sym7),
mean(ver_sym8),
kurtosis(ver_sym8),
skewness(ver_sym8),
exterior_vert_sym=(mean((ver_sym1+ver_sym2+ver_sym3+ver_sym4)/4)),
interior_vert_sym=(mean((ver_sym5+ver_sym6+ver_sym7+ver_sym8)/4)),
interior_horiz_sym=(mean((hor_sym2+hor_sym4+hor_sym6+hor_sym8)/4)),
exterior_horiz_sym=(mean((hor_sym1+hor_sym3+hor_sym5+hor_sym7)/4)))
}



