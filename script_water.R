setwd("C:/Users/Usuario/Dropbox/SigNexus_Adriana/CEPAN/ProdutoB/Segurança_hidrica")

library(rgdal)
library(raster)
library(foreign)

#### DATABASE - a partir do shapefile dos municípios ####
ISH<-readOGR("dados/SHP/variaveis_seg_hidrica.shp")
head(ISH@data)
names(ISH) ## descrição das variáveis e fontes na pasta XLS

ISH_dbf<-read.dbf("dados/SHP/variaveis_seg_hidrica.dbf")
names(ISH_dbf)

##### CORRELAÇÃO #####
correlation<-cor(ISH_dbf[,5:12])
write.table(correlation,"analises_R/cortest_ISH.txt") ### sem IVS

windows()
par(mfrow=c(1,3)) 

plot(ISH_dbf$qual_agua, ISH_dbf$perc_cisternas,pch=16,xlab="Qualidade da água",ylab="Fornecimento água (cisternas)")
plot(ISH_dbf$qual_agua, as.numeric(ISH_dbf$desmat),pch=16,xlab="Qualidade da água",ylab="Desmatamento (%)")
plot(ISH_dbf$num_pocos_, as.numeric(ISH_dbf$desmat),pch=16,xlab="Fornecimento água (poços)",ylab="Desmatamento (%)")


##### RASTERIZAR DADOS ####
# transformar um raster para cada variável se seg.alimentar na resolução de 10km - estes rasters são o input do Zonation
rst_10km<-raster("analises_R/veg_mask.asc")

# oferta
pocos_rst<-rasterize(ISH,rst_10km,field = ISH_dbf$num_pocos_)
writeRaster(pocos_rst,"dados/RST/num_pocos_hab.asc",overwrite=T)
nascentes_rst<-rasterize(ISH,rst_10km,field = ISH_dbf$perc_nasc_)
writeRaster(nascentes_rst,"dados/RST/perc_nasc_protegidas.asc",overwrite=T)
cob_hidrica_rst<-rasterize(ISH,rst_10km,field = ISH_dbf$perc_cob_h)
writeRaster(cob_hidrica_rst,"dados/RST/perc_cob_hidrica.asc",overwrite=T)

# demanda
cisternas_rst<-rasterize(ISH,rst_10km,field = ISH_dbf$perc_ciste)
writeRaster(cisternas_rst,"dados/RST/perc_cisternas.asc",overwrite=T)
agua_esgoto_rst<-rasterize(ISH,rst_10km,field = ISH_dbf$qual_agua)
writeRaster(agua_esgoto_rst,"dados/RST/qual_agua.asc",overwrite=T)
irrigacao_rst<-rasterize(ISH,rst_10km,field = ISH_dbf$perc_area[ISH_dbf$perc_area<=100])### valores abaixo de 100%
writeRaster(irrigacao_rst,"dados/RST/perc_area_irrigada.asc",overwrite=T)


#### MAPAS EM PDF ####
windows()
par(mfrow=c(2,3))
par(mar = c(4,4,4,6))

breaks <- seq(0, 1, by=0.2)
cols <- colorRampPalette(c("lightgray", "yellow", "red"))(length(breaks)-1)

plot(nascentes_rst, main ="Proteção de nascentes",col=cols)
plot(cob_hidrica_rst, main ="Cobertura da superfície hídrica",col=cols)
plot(pocos_rst, main ="Presença de poços artesianos",col=cols)

plot(cisternas_rst, main = "Presença de cisternas",col=cols)
plot(agua_esgoto_rst, main= "Saneamento de água e esgoto inadequado",col=cols)
plot(irrigacao_rst, main = "Uso da irrigação na agricultura",col=cols)
