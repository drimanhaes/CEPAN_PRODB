setwd("C:/Users/Usuario/Dropbox/SigNexus_Adriana/CEPAN/ProdutoB/Seguran�a_alimentar")

library(rgdal)
library(raster)
library(foreign)

#### DATABASE - a partir do shapefile dos munic�pios ####
ISA<-readOGR("dados/SHP/variaveis_seg_alimentar.shp")
head(ISA@data)
names(ISA) ## descri��o das vari�veis e fontes na pasta XLS

ISA_dbf<-read.dbf("dados/SHP/variaveis_seg_alimentar.dbf")
names(ISA_dbf)

##### CORRELA��O #####
correlation<-cor(ISA_dbf[,5:13])
write.table(correlation,"analises_R/cortest_ISA.txt")

### IVS x IDH altamente correlaciondao (-0.73), optou-se por selecionar somente IDH como variavel socioeconomica
plot(as.numeric(ISA_dbf$IVS), as.numeric(ISA_dbf$IDH),pch=16,xlab="�ndice Vulnerabilidade Social",ylab="�ndice Desenvolvimento Humano")


windows()
par(mfrow=c(1,3))

plot(ISA_dbf$renda_percap, ISA_dbf$IDH,pch=16,xlab="Renda per capita m�dia",ylab="�ndice de desenvolvimento Humano")
plot(ISA_dbf$renda_percap, ISA_dbf$per_fam_extpobr,pch=16,xlab="Renda per capita m�dia",ylab="Fam�lias em extrema pobreza (%)")
plot(ISA_dbf$IDH, ISA_dbf$per_fam_extpobr,pch=16,ylab="Fam�lias em extrema pobreza (%)",xlab="�ndice de desenvolvimento Humano")

##### RASTERIZAR DADOS ####
# transformar um raster para cada vari�vel se seg.alimentar na resolu��o de 10km - estes rasters s�o o input do Zonation
rst_10km<-raster("analises_R/veg_mask.asc")

#oferta
qtd_alim<-rasterize(ISA,rst_10km,field="qtd_alim")
writeRaster(qtd_alim,"dados/RST/qtd_alim.asc",overwrite=TRUE)
area_alim<-rasterize(ISA,rst_10km,field="area_alim")
writeRaster(area_alim,"dados/RST/area_alim.asc",overwrite=TRUE)
pec_cab<-rasterize(ISA,rst_10km,field="num_cab_pe")
writeRaster(pec_cab,"dados/RST/num_cab_pec.asc",overwrite=TRUE)

#demanda
renda_percap<-rasterize(ISA,rst_10km,field="Menor_rend")
writeRaster(renda_percap,"dados/RST/renda_percap.asc",overwrite=TRUE)
per_fam_extpobr<-rasterize(ISA,rst_10km,field="per_fam_ex")
writeRaster(per_fam_extpobr,"dados/RST/per_fam_pobr.asc",overwrite=TRUE)
vul_alim_nutri<-rasterize(ISA,rst_10km,field="vul_alim_n")
writeRaster(vul_alim_nutri,"dados/RST/vul_alim_nutri.asc",overwrite=TRUE)

#### MAPAS EM PDF ####
windows()
par(mfrow=c(2,3))
par(mar = c(4,4,4,6))

breaks <- seq(0, 1, by=0.2)
cols <- colorRampPalette(c("lightgray", "yellow", "red"))(length(breaks)-1)

plot(log(qtd_alim), main ="Log Quantidade produ��o agr�cola", col=cols)
plot(log(area_alim), main ="Log Area produ��o agr�cola", col=cols)
plot(log(pec_cab), main ="Log N�m.cabe�as pecu�ria", col=cols)

plot(renda_percap, main = "Pobreza (renda per capita)", col=cols)
plot(vul_alim_nutri, main= "Vulnerabilidade alimentar e nutricional", col=cols)
plot(per_fam_extpobr, main = "Fam�lias na extrema probreza", col=cols)

