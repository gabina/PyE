anorexia <- read.table(file = "anorexia.data", sep = "\t", header = TRUE)

names(anorexia)[4] = "Visitas"
#getwd()
#setwd("/home/gabina/Facultad/Probabilidad y Estadísitca/2020")
#read.table("anorexia.data",sep="\t")

#tabla1 <- table(anorexia$Signo,anorexia$Sexo)
#sum(tabla1)
attach(anorexia)

t <- rbind(table(Signo,Sexo),apply(tabla1, 2,FUN = sum))
cbind(t, apply(t,1,FUN=sum))

vis <- table(Visitas)
vis <- as.data.frame(vis)
vis[3] = round(vis[2] / 59,digits=4)
vis[4] <- cumsum(vis[2])
vis[5] <- round(vis[4] / 59, digits= 4)
names(vis)[3] = "Freq Relativa"
names(vis)[2] = "Freq Absoluta"
names(vis)[4] = "Freq Absoluta Acumulada"
names(vis)[5] = "Freq Relativa Acumulada"

ed <- as.data.frame(table(cut(Edad, breaks= seq(11,36,by=3),right=FALSE)))

barplot(table(Signo),xlab = "Número de pacientes", ylab = "Signo", horiz = TRUE, main = "Principal signo visible en pacientes con anorexia\n Argentina, octubre 2012")

s <- as.factor(Signo)
levels(s) = c("Dieta severa", "Hiperactividad", "Uso de laxantes", "Uso de ropa holgada")

SignoOrdered <- factor(s, levels=levels(s)[order(table(s),decreasing = TRUE)])

#pieplot
sig <- table(SignoOrdered)
sig <- as.data.frame(sig)
sig[3] = round(sig[2] / 59,digits=4)*100
l <- paste(levels(SignoOrdered),"\n",format(sig[3]$Freq.1,nsmall=2),"%")
#par(mar=c(0.5,0,2,0))
pie(table(SignoOrdered),labels = l, cex = 0.7,init.angle = 30, clockwise = TRUE)
mtext(side=3,"PRINCIPAL SIGNO VISIBLE EN PACIENTES CON ANOREXIA\n ARGENTINA, OCTUBRE 2012",line = -0.5, cex = 0.8, font = 2)
mtext(side=1,"Fuente: Asociación de Lucha contra la Bulimia y la Anorexia" , line = -1, at=-0.8, cex = 0.6)

#barplot
#par(mar=c(3.5,8,2.5,4))
barplot(table(SignoOrdered),xlab = "Número de pacientes", horiz = TRUE,xlim = c(0,20),las = 1, adj = 0.5, cex.main = 0.8,cex.axis = 0.8, cex.lab = 0.8,mgp = c(1.5,0,0.5), tck = 0.05, cex.names = 0.7, font.lab=2, col = "thistle",width = c(0.01,0.01,0.01,0.01))
mtext(side = 3, text = "PRINCIPAL SIGNO VISIBLE EN PACIENTES CON ANOREXIA\n ARGENTINA, OCTUBRE 2012", line = 0.25, font = 2, cex = 0.8, at=7)
mtext(side = 2, text = "Signo", line = 6.5, font = 2, cex = 0.8)
mtext(side=1,"Fuente: Asociación de Lucha contra la Bulimia y la Anorexia" , line = 2.5, at=-1, cex = 0.6)

#barplot comparativo
par(mar=c(4,7,3,3))
barplot(table(Sexo,SignoOrdered), beside= TRUE, horiz = TRUE,xlab = "Número de pacientes", las = 1, legend.text = TRUE, col = c("springgreen4","violetred3"), cex.lab = 0.8, font.lab =2, cex.names = 0.7, mgp=c(1.5,0.5,0))
mtext(side = 3, text = "PRINCIPAL SIGNO VISIBLE EN PACIENTES CON ANOREXIA SEGÚN SEXO\n ARGENTINA, OCTUBRE 2012", line = 0.25, font = 2, cex = .75, at=5)
mtext(side=1,"Fuente: Asociación de Lucha contra la Bulimia y la Anorexia" , line = 2.7, at=0, cex = 0.6)


#gráfico de bastones para visitas
par(mar=c(6,5,4,4), cex = 0.75)
v <- c(0,as.vector(table(Visitas)),0)
plot(0:6,v, "h", ylim = c(0,25), ylab = "Frecuencia absoluta", xlab = "Número de visitas", main = "NÚMERO DE VISITAS POR PACIENTE \n ARGENTINA, OCTUBRE 2012", cex.main = 1, cex.lab = 1, font.lab = 2,
     las = 1)
mtext(side=1,"Fuente: Asociación de Lucha contra la Bulimia y la Anorexia" , line = 4.5, at=1.5, cex = 0.7)

#gráfico escalonado para visitas
vcum <- cumsum(v)/59
plot(0:6,vcum, "s", ylim = c(0,1), ylab = "", xlab = "Número de visitas", main = "NÚMERO DE VISITAS POR PACIENTE \n ARGENTINA, OCTUBRE 2012", 
     cex.main = 1,
     font.lab = 2,
     las = 1)
abline(h = seq(0,1,by = 0.2), col = "gray67", lty = "dotted")
mtext(side = 2, "Frecuencia relativa acumulada", font = 2, cex = 0.8, line = 2.5)
mtext(side=1,"Fuente: Asociación de Lucha contra la Bulimia y la Anorexia" , line = 4.5, at=1.5, cex = 0.7)

#histograma para edad
#png("hist.png")
par(mar=c(4,4,4,2),mgp=c(1.8,0.6,0))
hist(Edad,breaks= seq(11,36,by=3), xlim = c(11,36), right = FALSE, axes = FALSE,
ylab = "Frecuencia absoluta", xlab = "Edad", font.lab = 2, col = "lightpink", 
cex.lab = 0.8, main = NULL)
axis(side = 1, at =seq(11,36,by=3), cex.axis = 0.8, lwd.ticks = 1, lwd = 1)   
axis(side = 2, at= seq(0,20, by =5), cex.axis = 0.8,
las = 1, lwd.ticks = 1, lwd = 1) 
mtext(side = 3, 
"EDAD DE LOS PACIENTES CON ANOREXIA\n ARGENTINA, OCTUBRE 2012",
cex = 0.8, font = 2, line = 1, at = 22)
mtext(side=1,"Fuente: Asociación de Lucha contra la Bulimia y la Anorexia" , line = 3, 
at=16, cex = 0.7)
#dev.off()

#polígono de frecuencias para edad
#png("frecuencia.png")
e <- as.vector(t(ed[2]))
par(mar=c(3.5,5,4,2),mgp=c(2,0.6,0))
plot(seq(11,36,by=3),c(0,e)/59,"l",axes=FALSE, xlab = "", ylab = "")
axis(side = 1, at =seq(11,36,by=3),cex.axis = 0.8, padj = -0.5 )   
axis(side = 2, at= seq(0,0.35, by =0.05),cex.axis = 0.8,
las = 1, hadj = 1)
abline(h = seq(0,0.35, by =0.05), col = "gray67", lty = "dotted")
mtext(side = 1, "Edad", font = 2, cex = 0.8, line = 1.5)
mtext(side = 2, "Frecuencia relativa", font = 2, cex = 0.8, line = 2.5)
mtext(side = 3, 
"EDAD DE LOS PACIENTES CON ANOREXIA\n ARGENTINA, OCTUBRE 2012",
cex = 0.8, font = 2, line = 1, at = 22)
mtext(side=1,"Fuente: Asociación de Lucha contra la Bulimia y la Anorexia" , line = 2.5, 
at=16, cex = 0.7)
#dev.off()

#acumulativo para edad
plot(seq(11,36,by=3),c(0,cumsum(e)/59),"l",axes=FALSE, xlab = "", ylab = "")
axis(side = 1, at =seq(11,36,by=3), cex.axis = 0.8, 
     padj = -0.5)
axis(side = 2, at= seq(0,1, by =0.2), las = 1, cex.axis = 0.8,
     hadj = 1)
abline(h = seq(0,1, by =0.2), col = "gray67", lty = "dotted")
mtext(side = 1, "Edad", font = 2, cex = 0.8, line = 1.5)
mtext(side = 2, "Frecuencia relativa acumulada", font = 2, cex = 0.8, line = 2.5)
mtext(side = 3, 
      "EDAD DE LOS PACIENTES CON ANOREXIA\n ARGENTINA, OCTUBRE 2012",
      cex = 0.8, font = 2, line = 1, at = 22)
mtext(side=1,"Fuente: Asociación de Lucha contra la Bulimia y la Anorexia" , line = 2.5, 
      at=16, cex = 0.7)

#boxplot

mean(Edad)
median(Edad)
min(Edad)
max(Edad)
summary(Edad)
summary(Sexo)
summary(Visitas)
summary(Signo)

par(font.lab = 2)
boxplot(x = Edad, horizontal = TRUE, xlab = "Edad",
        ylim = c(10,35),
        right = FALSE,
        font.lab =2, cex.lab = 1, col = "moccasin")
par(font.lab = 1)
mtext(side = 3, 
      "EDAD DE LOS PACIENTES CON ANOREXIA\n ARGENTINA, OCTUBRE 2012",
      cex = 0.8, font = 2, line = 1, at = 22)
mtext(side=1,"Fuente: Asociación de Lucha contra la Bulimia y la Anorexia" , line = 4, 
      at=16, cex = 0.7)

#boxplot comparativo
par(font.lab = 2)
boxplot(formula = Edad~Sexo, xlab = "Sexo",
        ylab = "",
        ylim = c(10,35),
        right = FALSE,
        boxwex = 0.4,
        font.lab =2, cex.lab = 1, col = "moccasin",
        las = 1,
        main =  "EDAD SEGÚN SEXO DE LOS PACIENTES CON ANOREXIA\n ARGENTINA, OCTUBRE 2012",
        cex.main = 1)
par(font.lab = 1)
mtext(side = 2, 
      "Edad",
      cex = 0.8, font = 2, line = 2.5, at = 22)
mtext(side=1,"Fuente: Asociación de Lucha contra la Bulimia y la Anorexia" , line = 4, 
      at=1, cex = 0.7)
