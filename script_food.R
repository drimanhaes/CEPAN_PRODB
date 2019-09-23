setwd("C:/Users/Usuario/Dropbox/SigNexus_Adriana/CEPAN/ProdutoB/Segurança_alimentar")

library(rgdal)
library(raster)
library(foreign)

#### DATABASE - a partir do shapefile dos municípios ####
ISA<-readOGR("dados/SHP/variaveis_seg_alimentar.shp")
head(ISA@data)
names(ISA) ## descrição das variáveis e fontes na pasta XLS

ISA_dbf<-read.dbf("dados/SHP/variaveis_seg_alimentar.dbf")
names(ISA_dbf)

##### CORRELAÇÃO #####
correlation<-cor(ISA_dbf[,5:13])
write.table(correlation,"analises_R/cortest_ISA.txt")

### IVS x IDH altamente correlaciondao (-0.73), optou-se por selecionar somente IDH como variavel socioeconomica
plot(as.numeric(ISA_dbf$IVS), as.numeric(ISA_dbf$IDH),pch=16,xlab="Índice Vulnerabilidade Social",ylab="Índice Desenvolvimento Humano")


windows()
par(mfrow=c(1,3))

plot(ISA_dbf$renda_percap, ISA_dbf$IDH,pch=16,xlab="Renda per capita média",ylab="Índice de desenvolvimento Humano")
plot(ISA_dbf$renda_percap, ISA_dbf$per_fam_extpobr,pch=16,xlab="Renda per capita média",ylab="Famílias em extrema pobreza (%)")
plot(ISA_dbf$IDH, ISA_dbf$per_fam_extpobr,pch=16,ylab="Famílias em extrema pobreza (%)",xlab="Índice de desenvolvimento Humano")

##### RASTERIZAR DADOS ####
# transformar um raster para cada variável se seg.alimentar na resolução de 10km - estes rasters são o input do Zonation
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

plot(log(qtd_alim), main ="Log Quantidade produção agrícola", col=cols)
plot(log(area_alim), main ="Log Area produção agrícola", col=cols)
plot(log(pec_cab), main ="Log Núm.cabeças pecuária", col=cols)

plot(renda_percap, main = "Pobreza (renda per capita)", col=cols)
plot(vul_alim_nutri, main= "Vulnerabilidade alimentar e nutricional", col=cols)
plot(per_fam_extpobr, main = "Famílias na extrema probreza", col=cols)

