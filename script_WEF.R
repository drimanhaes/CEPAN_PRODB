setwd("C:/Users/Usuario/Dropbox/SigNexus_Adriana/CEPAN/ProdutoB/WEF_Security")

library(rgdal)
library(raster)
library(foreign)


#### Mapas de priorização do Zonation ####
ISH_zonation<-raster("C:/Users/Usuario/Dropbox/SigNexus_Adriana/CEPAN/ProdutoB/WEF_Security/Resultados/Zonation/RST/ISH_Zonation.tif")
ISA_zonation<-raster("C:/Users/Usuario/Dropbox/SigNexus_Adriana/CEPAN/ProdutoB/WEF_Security/Resultados/Zonation/RST/ISA_Zonation.tif")
ISE_zonation<-raster("C:/Users/Usuario/Dropbox/SigNexus_Adriana/CEPAN/ProdutoB/WEF_Security/Resultados/Zonation/RST/ISE_Zonation.tif")
plot(ISE_zonation)


windows()
par(mfrow=c(1,3)) 
par(mar = c(4,4,4,6))

breaks = seq(0, 1, by=0.2)
cols <- colorRampPalette(c("lightgreen", "yellow", "red"))(length(breaks)-1)

plot(ISH_zonation, main="Insegurança hídrica", col=cols)
plot(ISA_zonation, main="Insegurança alimentar",col=cols)
plot(ISE_zonation, main="Insegurança energética", col=cols)


#### CORRELAÇÕES ####
# entre mapas de priorização
cor.test(ISH_zonation[],ISA_zonation[], method= "pearson")# 0.056 hídrica x alimentar
cor.test(ISH_zonation[],ISE_zonation[], method= "pearson")# 0.314 hídrica x energética
cor.test(ISE_zonation[],ISA_zonation[], method= "pearson")# 0.334 energética x alimentar

### entre todas as variáveis de oferta e demanda
NEXUS<-read.table("C:/Users/Usuario/Dropbox/SigNexus_Adriana/CEPAN/ProdutoB/WEF_Security/dados_WEF/NEXUS_database.txt",head=T,dec=".",sep="\t" , quote = "", row.names = NULL,  stringsAsFactors = FALSE)
head(NEXUS)
names(NEXUS)

cor_table<-round(cor(NEXUS[,c(5:22)]),3)
write.table(cor_table,"Resultados/Correlação/Tabela_cor_NEXUS.txt")


#### VARIÁVEIS SOCIOAMBINETAIS >> IDH & desmatamento ####
IDH<-raster("analises_R/IDH.asc")
min(na.omit(IDH[]))

IDNH<-IDH # indice de não desenvolvimento humano 
IDNH[]<-1 - IDNH[]
plot(IDNH)

deforest<-raster("analises_R/desmat_mun.asc")
deforest[]<-deforest[]/100
min(na.omit(deforest[]))
max(na.omit(deforest[]))

### ANÁLISES ####

# 1ST STEP >> MÉDIA DE IDH + DEFOREST + PRIORITY AREAS 
priority_water<-mean(IDNH + deforest + ISH_zonation)
priority_food<-mean(IDNH + deforest + ISA_zonation)
priority_energy<-mean(IDNH + deforest + ISE_zonation)

##### MAPAS #####

# BIDH_DESMAT
windows()
par(mfrow=c(1,2)) 
par(mar = c(4,4,4,6))

breaks = seq(0, 1, by=0.2)
cols <- colorRampPalette(c("lightgreen", "yellow", "red"))(length(breaks)-1)

plot(IDNH, main = "Baixo IDH (B.IDH)", col=cols)
plot(deforest, main= "Desmatamento (DESM)", col=cols)

# Medias_BIDH_DESM_WEF
windows()
par(mfrow=c(1,3)) 
par(mar = c(4,4,4,6))

plot(priority_water,main="Média (B.IDH + DESM + ÁGUA)",col=cols)
plot(priority_food,main="Média (B.IDH + DESM + ALIMENTO)",col=cols)
plot(priority_energy,main="Média (B.IDH + DESM + ENERGIA)",col=cols)

### areas_prioritarias_WEF
# modifica os valores contínuos em categóricos >>> 1= baixa; 2= média; 3=alta
priority_water[priority_water[]<= 1]<- 1
priority_water[priority_water[] > 1 & priority_water[] <= 2]<- 2
priority_water[priority_water[]> 2]<- 3

priority_food[priority_food[]<= 1]<- 1
priority_food[priority_food[] > 1 & priority_food[] <= 2]<- 2
priority_food[priority_food[]> 2]<- 3

priority_energy[priority_energy[]<= 1]<- 1
priority_energy[priority_energy[] > 1 & priority_energy[] <= 2]<- 2
priority_energy[priority_energy[]> 2]<- 3

cols <- colorRampPalette(c("lightgreen", "yellow", "red"))(4-1)

windows()
par(mfrow=c(1,3)) 
par(mar = c(4,4,4,6))

plot(priority_water,breaks = c(0,1,2,3),col=cols, main="Prioridade para segurança hídrica")
plot(priority_food,breaks = c(0,1,2,3),col=cols, main="Prioridade para segurança alimentar")
plot(priority_energy,breaks = c(0,1,2,3),col=cols, main="Prioridade para segurança energética ")

# MAPA FINAL DE ÁREA PRIORITÁRIA >> média dos tres mapas de insegurança WEF
priority_final<-(priority_water + priority_food + priority_energy)/3
# categoriza
priority_final[priority_final[]<= 1]<- 1
priority_final[priority_final[] > 1 & priority_final[] <= 2]<- 2
priority_final[priority_final[]> 2]<- 3

windows()
par(mar = c(4,4,4,6))
plot(priority_final,breaks = c(0,1,2,3),col=cols, main="Prioridade para segurança WEF")

municipios<-readOGR("C:/Users/Usuario/Dropbox/SigNexus_Adriana/CEPAN/ProdutoC/SHP/municipios_BR.shp")
estados<-readOGR("C:/Users/Usuario/Dropbox/SigNexus_Adriana/CEPAN/ProdutoB/WEF_Security/analises_R/Estados_BR.shp")

plot(municipios, add=T)
plot(estados, add=T,lwd=3)


##### RASTERIZAR ####
writeRaster(priority_water,"Resultados/RST/priority_water.asc")
writeRaster(priority_food,"Resultados/RST/priority_food.asc")
writeRaster(priority_energy,"Resultados/RST/priority_energy.asc")
writeRaster(priority_final,"Resultados/RST/priority_final_WEF.asc")


#### TRANSFORMAR EM TABELA #####
mun_cod<-municipios$CD_GEOC

### valores de priorização hídrica (zonation) por municipio #
ISH_zonation_1km<-disaggregate(ISH_zonation,10)
plot(ISH_zonation)

municipios$priority_water<-0
i=2403251
for(i in mun_cod[1:length(mun_cod)]){
  mun_shp_loop<-municipios[municipios$CD_GEOC==i,]
  mun.crop <- crop(ISH_zonation_1km, extent(mun_shp_loop), snap="out")
  crop1 <- setValues(mun.crop, NA)
  mun.r <- rasterize(mun_shp_loop, crop1) 
  mun_rst <- mask(x=mun.crop, mask=mun.r)

  if(any(is(mun_rst) == "NULL")) {
    ISH[which(i==mun_cod),"priority_water"]<-0
    next
  }
  
  a<-round(mean(mun_rst[!is.na(mun_rst[])]),2)
  municipios[which(i==mun_cod),"priority_water"]<-a
  cat(paste(which(i==mun_cod),"\n"))
} 

mun<-municipios@data
### valores de priorização alimentar (zonation) por municipio #
municipios$priority_food<-0

for(i in mun_cod[1:length(mun_cod)]){
  mun_shp_loop<-municipios[municipios$CD_GEOC==i,]
  mun.crop <- crop(ISA_zonation, extent(mun_shp_loop), snap="out")
  crop1 <- setValues(mun.crop, NA)
  mun.r <- rasterize(mun_shp_loop, crop1) 
  mun_rst <- mask(x=mun.crop, mask=mun.r)
  
  if(any(is(mun_rst) == "NULL")) {
    ISH[which(i==mun_cod),"priority_food"]<-0
    next
  }
  
  a<-round(mean(mun_rst[!is.na(mun_rst[])]),2)
  municipios[which(i==mun_cod),"priority_food"]<-a
  cat(paste(which(i==mun_cod),"\n"))
} 

### valores de priorização energetica (zonation) por municipio #
municipios$priority_energy<-0

for(i in mun_cod[1:length(mun_cod)]){
  mun_shp_loop<-municipios[municipios$CD_GEOC==i,]
  mun.crop <- crop(ISE_zonation, extent(mun_shp_loop), snap="out")
  crop1 <- setValues(mun.crop, NA)
  mun.r <- rasterize(mun_shp_loop, crop1) 
  mun_rst <- mask(x=mun.crop, mask=mun.r)
  
  if(any(is(mun_rst) == "NULL")) {
    ISH[which(i==mun_cod),"priority_energy"]<-0
    next
  }
  
  a<-round(mean(mun_rst[!is.na(mun_rst[])]),2)
  municipios[which(i==mun_cod),"priority_energy"]<-a
  cat(paste(which(i==mun_cod),"\n"))
} 

### valores de priorização final por municipio #
municipios$priority_final<-0

for(i in mun_cod[1:length(mun_cod)]){
  mun_shp_loop<-municipios[municipios$CD_GEOC==i,]
  mun.crop <- crop(priority_final, extent(mun_shp_loop), snap="out")
  crop1 <- setValues(mun.crop, NA)
  mun.r <- rasterize(mun_shp_loop, crop1) 
  mun_rst <- mask(x=mun.crop, mask=mun.r)
  
  if(any(is(mun_rst) == "NULL")) {
    ISH[which(i==mun_cod),"priority_final"]<-0
    next
  }
  
  a<-round(mean(mun_rst[!is.na(mun_rst[])]),2)
  municipios[which(i==mun_cod),"priority_final"]<-a
  cat(paste(which(i==mun_cod),"\n"))
} 

### reescrever a tabela dbf do shapefile ##

municipios@data
write.dbf(municipios,"C:/Users/Usuario/Dropbox/SigNexus_Adriana/CEPAN/ProdutoB/WEF_Security/analises_R/municipios_BR.dbf")
