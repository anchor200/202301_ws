
# 緯度経度をメッシュコードに変換（種数カウントを楽にするため）
OCC_DATA$mesh2 <- mesh2get(OCC_DATA$longitude, OCC_DATA$latitude)

obs_count <- aggregate(list("obs_n"=rep(1, nrow(OCC_DATA))), list("mesh2"=OCC_DATA$mesh2), sum)
obs_count$lat <- latget(obs_count$mesh2)
obs_count$lon <- longet(obs_count$mesh2)



OCC_DATA <- OCC_DATA[OCC_DATA$Category=="nat.",]
OCC_DATA_use <- OCC_DATA[, c("species", "longitude", "latitude", "mesh2")]
sp_list <- unique(OCC_DATA$species)
sp_list

# データの蓄積過程
decade_data <- colSums(OCC_DATA[, c("pre.1949", "X1950.1959","X1960.1969", "X1970.1979", "X1980.1989", "X1990.1999", "X2000.2009", "X2010.")], na.rm = T)
barplot(decade_data)
title("n of data per decade")


# 種数マップ（観測種数に基づく）の表示
ggplot() + geom_tile(data=obs_count, aes(x=lon, y=lat, color=obs_n, fill=obs_n)) +
theme_bw()+ theme(text = element_text(size = 15))+
scale_fill_gradientn(colors = pal, na.value = NA, name = "number of \nobserved species") +
scale_color_gradientn(colors = pal, na.value = NA, name = "number of \nobserved species")


records <- aggregate(list("data_n"=rowSums(OCC_DATA[, c("pre.1949", "X1950.1959","X1960.1969", "X1970.1979", "X1980.1989", "X1990.1999", "X2000.2009", "X2010.")], na.rm = T)),
                     list("mesh2"=OCC_DATA$mesh2), sum)

records$lat <- latget(records$mesh2)
records$lon <- longet(records$mesh2)

# データレコード数のプロット
ggplot() + geom_tile(data=records, aes(x=lon, y=lat, color=data_n, fill=data_n)) +
theme_bw()+ theme(text = element_text(size = 15))+
scale_fill_gradientn(colors = pal,
                          breaks=c(1, 10, 100, 1000, 2000),
                          limits=c(1, 3000), guide = "legend",  trans = "log") +
scale_color_gradientn(colors = pal,
                          breaks=c(1, 10, 100, 1000, 2000),
                          limits=c(1, 3000), guide = "legend",  trans = "log") 

# レコード数の分布
hist(log10(records$data_n))


# データレコード数と緯度の関係プロット
latpal<- c("#9eff17", "#9EBE91", "#220d3d")

records_bydec <- aggregate(list("obs_n"=rep(1, nrow(OCC_DATA)), "data_n"=rowSums(OCC_DATA[, c("pre.1949", "X1950.1959","X1960.1969", "X1970.1979", "X1980.1989", "X1990.1999", "X2000.2009", "X2010.")], na.rm = T)),
                     list("mesh2"=OCC_DATA$mesh2), sum)
records_bydec$lat <- latget(records_bydec$mesh2)
records_bydec$lon <- longet(records_bydec$mesh2)

ggplot(data=records_bydec, aes(y=obs_n, x=log10(data_n), color=lat)) + geom_point() + stat_smooth()+
scale_color_gradientn(colors = latpal,
                          breaks=seq(20,45, by=5),
                          limits=c(20, 45), name="Latitude")+xlab("log10(n of data records)")+ylab("observed richness") + theme_bw()

##### 種分布モデリング（多種）

data_use=OCC_DATA_use
sp_list=sp_list

# 回す
source("C:/Users/anchor200/Desktop/202301_ws/src/sdm_stacker.r")

SDM_stack <- list("sdm"=sdm_stack, "hsm"=shm_stack, "ths"=thss)
saveRDS(SDM_stack, "C:/Users/anchor200/Desktop/202301_ws/output/sdm_stack.rds")

# SDM_stack <- readRDS("C:/Users/anchor200/Desktop/202301_ws/output/sdm_stack.rds")

# 種多様性レイヤの作成
tr <- calc(SDM_stack$sdm, sum)
td <- as.data.frame(rasterToPoints(tr))
colnames(td) <- c("x", "y", "dummytaxon")

tdata_t <-env_data

# 種数レイヤ図示
plot(tr , col=colorRampPalette(pal)(50))
title(paste("dummy taxon alpha diversity (stacked sdm)"))


##### 種数パターンの図示

# データ数との関係
td$mesh2 <- as.integer(mesh2get(td$x, td$y))
tdata_t <- merge(tdata_t, td[,c(3,4)], by=c("mesh2"), all=T)

colnames(td) <- c("x", "y", "sr", "mesh2")
td$group <- "dummytaxon"

sr_data <- td

sdm_sr <- td
colnames(sdm_sr) <- c("x", "y", "sdm_richness", "mesh2")
sdm_sr$mesh2 <- mesh2get(sdm_sr$x, sdm_sr$y)
records_ <- merge(records, sdm_sr[, c(3,4)], by="mesh2")

ggplot(data=records_ , aes(y=sdm_richness, x=log10(data_n), color=lat)) + geom_point() + stat_smooth()+
scale_color_gradientn(colors = latpal,
                          breaks=seq(20,45, by=5),
                          limits=c(20, 45), name="Latitude")+xlab("log10(n of data records)")+ylab("sdm richness") + theme_bw()

# 地理情報のデータ
jp <- st_read("C:/Users/anchor200/Desktop/202301_ws/data/ne_10m_coastline/ne_10m_coastline.shp")

land_area <- aggregate(list("land_a"=tdata_t$land.area), list("y"=as.character(tdata_t$y)), sum)
land_area$y <- as.numeric(land_area$y)

elev <- aggregate(list("elev"=tdata_t$elevation), list("y"=as.character(tdata_t$y)), max)
elev$y <- as.numeric(elev$y)

temp <- aggregate(list("temperature"=tdata_t$bio_1), list("y"=as.character(tdata_t$y)), max)
temp$y <- as.numeric(temp$y)


area_stats <- merge(land_area, elev, by="y")
area_stats <- merge(area_stats, temp, by="y")

# 緯度勾配パターンの図示
g_sr <- sr_data %>%
ggplot(aes(x=y, y=log10(sr)), color="blue")  + theme_bw() + geom_point(color="lightgray") +
coord_flip(ylim=c(0,1.5))+
theme(legend.position=c(0.5,0.05), text = element_text(size = 12), legend.title=element_blank(), legend.box = "horizontal")+
scale_colour_discrete(guide = guide_legend(direction = "horizontal",
                             label.position="bottom", label.hjust = 0.5, label.vjust = 0.5,
                             label.theme = element_text(angle = 0), nrow=1, label.size=20)) + 
geom_text(aes(y=y-100), size=10, label="|") +
xlim(21.3, 48.2) +
geom_area(data=area_stats, aes(x=y, y=land_a/10000000000), color="black", fill="black")+
geom_area(data=area_stats, aes(x=y, y=elev/3000), color="grey", fill="grey", alpha=0.7)+
geom_segment(aes(x=25,xend=45,y=2000000000/10000000000,yend=2000000000/10000000000), color="black", size = 1)+
geom_segment(aes(x=25,xend=45,y=1000/3000,yend=1000/3000), color="grey40", size = 1)+
annotate("text", x=47,   y=2000000000/10000000000, label="sum\nland area\n2000 [km^2]") +
annotate("text", x=24,   y=1000/3000, label="maximum\nmean elevation\n1000 [m]", color="grey40") + 
geom_line(data=area_stats, aes(x=y, y=temperature/400), color="red", size = 1) +
geom_segment(aes(x=25,xend=45,y=200/400,yend=200/400), color="thistle4", size = 1)+
annotate("text", x=47,   y=200/400, label="annual mean\ntemperature\n20 [deg]", color="red") + 
ylab("log10 (species richness)")+xlab("Latitude")+ 
stat_smooth(se=T, show.legend=FALSE)

g_map <- ggplot()+
geom_tile(data=sdm_sr, aes(x=x, y=y, fill=sdm_richness	))+
scale_fill_gradientn(colors = pal, na.value = NA, name = "sdm_richness")+
geom_sf(data=jp[1], fill=NA, color=NA)+
coord_sf(xlim = c(122, 146), ylim=c(23, 46))+theme(legend.position=c(0.15,0.8))+
xlab("Longitude")+ylab("Latitude")

gridExtra::grid.arrange(g_map, g_sr,  nrow = 1)


##### 種数ドライバー解析

regr_df <- tdata_t[, c(-1,-2,-3, -5)]
regr_df$land.area.log <- log10(tdata_t$land.area)

regr_df <- as.data.frame(scale(regr_df ))
head(regr_df)


# 種数を説明変数の候補に対してプロット
# pairs(regr_df , panel = panel.smooth)

expv <- colnames(regr_df)
expv <- expv[expv!="dummytaxon"]

png("C:/Users/anchor200/Desktop/202301_ws/output/sr_exp_1.png", width = 800, height = 400)
par(mfrow = c(3, 6))
for(exp in expv[1:18]){
	plot(regr_df[, exp], regr_df$dummytaxon, xlab=exp, ylab="species richness", col=rgb(0.1,0.1,0.1, alpha=0.1))
}
dev.off()

png("C:/Users/anchor200/Desktop/202301_ws/output/sr_exp_2.png", width = 800, height = 400)
par(mfrow = c(3, 6))
for(exp in expv[19:36]){
	plot(regr_df[, exp], regr_df$dummytaxon, xlab=exp, ylab="species richness", col=rgb(0.1,0.1,0.1, alpha=0.1))
}
dev.off()

png("C:/Users/anchor200/Desktop/202301_ws/output/sr_exp_3.png", width = 800, height = 400)
par(mfrow = c(3, 6))
for(exp in expv[37:54]){
	plot(regr_df[, exp], regr_df$dummytaxon, xlab=exp, ylab="species richness", col=rgb(0.1,0.1,0.1, alpha=0.1))
}
dev.off()

png("C:/Users/anchor200/Desktop/202301_ws/output/sr_exp_4.png", width = 800, height = 400)
par(mfrow = c(3, 6))
for(exp in expv[54:67]){
	plot(regr_df[, exp], regr_df$dummytaxon, xlab=exp, ylab="species richness", col=rgb(0.1,0.1,0.1, alpha=0.1))
}
dev.off()


# 気候モデル
clim <- lm(dummytaxon ~ land.area.log  + bio_1 +  bio_4 + bio_12 + bio_15 + I(bio_1^2) +I(bio_12^2) + elevation^2, data=regr_df)
clim_coefs <- as.data.frame(summary(clim)$coefficients)

clim_coefs$val <- c("Intercept", "land area (log)", "annual temp", "temp seasonality", "annual prec", "prec seasonality", "temp^2", "prec^2", "elevation")
colnames(clim_coefs)[2] <- "Std.Error"
clim_coefs
data.frame("adj. R2"=summary(clim)$ adj.r.squared)
vif(clim)

## 係数
clim_coefs %>%
ggplot(aes(x=factor(val, levels=rev(c("annual temp", "temp^2", "temp seasonality", "annual prec", "prec^2", "prec seasonality", "land area (log)", "elevation", "Intercept"))), y=Estimate)) +
geom_bar(stat="identity")+
geom_errorbar(aes(ymin = Estimate - 1.96*Std.Error, ymax = Estimate + 1.96*Std.Error)) + ylab("partial regr coef estimate")+
xlab("explanatory variables")+theme_bw()+theme(axis.text.x = element_text(angle = 90, hjust = 1))+
coord_flip()

## 興味のある変数だけ
clim_coefs %>%
mutate("variables"=factor(val, levels=rev(c("annual temp", "temp^2", "temp seasonality", "annual prec", "prec^2", "prec seasonality", "land area (log)", "elevation", "Intercept")))) %>%
filter(variables!="land area (log)" & variables!="Intercept") %>%
ggplot(aes(x=variables, y=Estimate)) +
geom_bar(stat="identity")+
geom_errorbar(aes(ymin = Estimate - 1.96*Std.Error, ymax = Estimate + 1.96*Std.Error)) + ylab("partial regr coef estimate")+
xlab("explanatory variables")+theme_bw()+theme(axis.text.x = element_text(angle = 90, hjust = 1))+
coord_flip()

## 変数の説明力
partialR2 <- as.data.frame(partial_r2(clim))
colnames(partialR2)[1] <- c("partial_R2")
partialR2 %>%
mutate("variables"=rownames(partialR2)) %>%
ggplot(aes(x=variables, y=partial_R2)) + geom_bar(stat="identity") + theme_bw() 

## 興味のある変数だけ
temp_r2 <- partialR2 %>%
mutate("variables"=rownames(partialR2)) %>%
filter(variables!="(Intercept)" & variables!="land.area.log") %>%
ggplot(aes(x=variables, y=partial_R2)) + geom_bar(stat="identity") + theme_bw() 

###### かけがえのなさ度（仮）
sdm <- SDM_stack$sdm

# 総生息面積に対するメッシュ内の生息面積を算出する
for(i in 1:length(names(sdm))){
    sdm[[i]][] <- sdm[[i]][] / sum(sdm[[i]][], na.rm=T)
}
irrep <- sdm
irrep_sum <- calc(irrep, sum)

plot(irrep_sum , col=colorRampPalette(pal)(50))
title(paste("dummy taxon irreplaceable grid"))

# 種数とかけがえのなさ度の関係
irdf <- as.data.frame(rasterToPoints(irrep_sum))
irdf$mesh2 <- as.integer(mesh2get(irdf$x, irdf$y))
colnames(irdf)[3] <- "irreplaceability"
sdm_sr_irr <- merge(sdm_sr[,c(3,4)], irdf, by="mesh2")

sdm_sr_irr %>%
ggplot(aes(y=irreplaceability, x=sdm_richness, color=y)) + geom_point() + stat_smooth()+
scale_color_gradientn(colors = latpal,
                          breaks=seq(20,45, by=5),
                          limits=c(20, 45), name="Latitude")+xlab("sdm richness")+ylab("irreplaceability") + theme_bw()


