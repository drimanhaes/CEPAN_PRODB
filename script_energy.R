setwd("C:/Users/Usuario/Dropbox/SigNexus_Adriana/CEPAN/ProdutoB/Segurança_energetica")

library(rgdal)
library(raster)
library(foreign)

#### DATABASE - a partir do shapefile dos municípios ####
ISE<-readOGR("dados/SHP/variaveis_seg_energetica.shp")
head(ISE@data)
names(ISE) ## descrição das variáveis e fontes na pasta XLS

ISE_dbf<-read.dbf("dados/SHP/variaveis_seg_energetica.dbf") ### sem IVS
names(ISE_dbf)

##### CORRELAÇÃO #####
correlation<-cor(ISE_dbf[,5:10])
write.table(correlation,"analises_R/cortest_ISE.txt")

windows()
par(mfrow=c(1,2))

plot(log(ISE_dbf$caat_tMS_a), ISE_dbf$desmat,pch=16,xlab="Qtde massa seca caatinga para energia",ylab="Desmatamento (%)")
plot(log(ISE_dbf$prod_carva), ISE_dbf$desmat,pch=16,xlab="Produção carvão (m3)",ylab="Desmatamento (%)")


##### RASTERIZAR DADOS ####
# transformar um raster para cada variável se seg.alimentar na resolução de 10km - estes rasters são o input do Zonation
rst_10km<-raster("analises_R/veg_mask.asc")

#oferta
caat_tMS_ano<-rasterize(ISE,rst_10km,field="caat_tMS_a")
writeRaster(caat_tMS_ano,"dados/RST/caat_tMS_ano.asc",overwrite=TRUE)
eucalipto_tMS_ano<-rasterize(ISE,rst_10km,field="eucalipto_")
writeRaster(eucalipto_tMS_ano,"dados/RST/eucalipto_tMS_ano.asc",overwrite=TRUE)

#demanda
prod_carvao<-rasterize(ISE,rst_10km,field="prod_carva")
writeRaster(prod_carvao,"dados/RST/prod_carvao.asc",overwrite=TRUE)
prod_lenha<-rasterize(ISE,rst_10km,field="prod_lenha")
writeRaster(prod_lenha,"dados/RST/prod_lenha.asc",overwrite=TRUE)

#### MAPAS EM PDF ####
windows()
par(mfrow=c(2,2))
par(mar = c(4,4,4,8))

breaks <- seq(0, 1, by=0.2)
cols <- colorRampPalette(c("lightgray", "yellow", "red"))(length(breaks)-1)

plot(log(caat_tMS_ano), main ="Log massa seca da caatinga para energia",col=cols)
plot(eucalipto_tMS_ano, main = "Massa seca de eucalipto para energia",col=cols)
plot(prod_lenha,main="Produção de lenha",col=cols)
plot(prod_carvao, main ="Produção de carvão vegetal",col=cols)
