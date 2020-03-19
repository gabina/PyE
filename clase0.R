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
plot(0:6,v, "h", ylim = c(0,25), ylab = "Frecuencia absoluta", xlab = "Número de visitas", main = "NÚMERO DE VISITAS POR PACIENTE \n ARGENTINA, OCTUBRE 2012")
mtext(side=1,"Fuente: Asociación de Lucha contra la Bulimia y la Anorexia" , line = 4.5, at=1., cex = 0.7)

#gráfico escalonado para visitas
vcum <- cumsum(v)/59
plot(0:6,vcum, "s", ylim = c(0,1), ylab = "Frecuencia relativa acumulada", xlab = "Número de visitas", main = "NÚMERO DE VISITAS POR PACIENTE \n ARGENTINA, OCTUBRE 2012")
mtext(side=1,"Fuente: Asociación de Lucha contra la Bulimia y la Anorexia" , line = 4.5, at=1., cex = 0.7)

#histograma para edad
png("hist.png")
par(mar=c(6,5,5,5))
hist(Edad,breaks= seq(11,36,by=3), xlim = c(11,36), right = FALSE, axes = FALSE,
ylab = "Frecuencia absoluta", xlab = "Edad", font.lab = 2,
mgp=c(2.5,2,0), col = "lightpink",
main = "EDAD DE LOS PACIENTES CON ANOREXIA\n ARGENTINA, OCTUBRE 2012")
axis(side = 1, at =seq(11,36,by=3) )   
axis(side = 2, at= seq(0,20, by =5)) 
mtext(side=1,"Fuente: Asociación de Lucha contra la Bulimia y la Anorexia" , line = 4, 
at=14, cex = 0.7)
dev.off()

#polígono de frecuencias para edad
png("frecuencia.png")
e <- as.vector(t(ed[2]))
plot(seq(11,36,by=3),c(0,e)/59,"l",axes=FALSE, xlab = "Edad", ylab = "Frecuencia relativa")
axis(side = 1, at =seq(11,36,by=3) )   
axis(side = 2, at= seq(0,0.5, by =0.1)) 
dev.off()

#acumulativo para edad
plot(seq(11,36,by=3),c(0,cumsum(e)/59),"l",axes=FALSE, xlab = "Edad", ylab = "Frecuencia relativa acumulada")
axis(side = 1, at =seq(11,36,by=3) )   
axis(side = 2, at= seq(0,1, by =0.2)) 

