
###### データ準備
env_data <- fread("C:/Users/anchor200/Desktop/202301_ws/data/terrestrial_environment_grid10km.csv")
env_data <- data.frame(env_data)
tempdf <- data.frame("name"=NA, "isContinuousLikely"=NA)[-1,]
for (ii in 5:ncol(env_data)){
    tempdf <- rbind(tempdf, data.frame("name"=colnames(env_data)[ii], "isContinuousLikely"=(length(unique(env_data[,c(ii)]))/length(env_data[,c(ii)]) > 0.3)))
}
tempdf[tempdf$isContinuousLikely==FALSE,]
# 値のバリエーションが少ない変数は、カテゴリカル変数かも

table(env_data$beach_2014)


# 環境データのラスター化
for (ii in 4:ncol(env_data)){
    assign(colnames(env_data)[ii], rasterFromXYZ(env_data[,c(2,3,ii)]))
}

for(ii in 4:ncol(env_data)){
    if(ii == 4){
                xy2 = get(colnames(env_data)[ii])
            } else {
                xy2 = stack(xy2,get(colnames(env_data)[ii]))
    }      	
}

crs(xy2) = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"


# 環境データプロット
par(mar=c(5, 4, 4, 4) + 0.1)
par(mgp=c(2.5, 1, 0))

par(mfrow = c(2, 4))

for(i in 1:8){

    plot(xy2[[i]])
    title(main=names(xy2)[i], line=1)
}
par(mfrow = c(1, 1))



## 生物在データの読み込み
OCC_DATA <- read.csv("C:/Users/anchor200/Desktop/202301_ws/data/WS_grid10km.csv")
OCC_DATA <- OCC_DATA[OCC_DATA$Category=="nat.",]
sps_list <- unique(OCC_DATA$species)

data_use <-  OCC_DATA[, c("species", "longitude", "latitude", "mesh2")]

# 一種だけ抽出
taxon <- "sp59"
data_xy <- subset(data_use, data_use$species==taxon)
data_xy <- data_xy[!duplicated(data_xy$mesh2), c(2,3)]
colnames(data_xy) <- c("x", "y")

# 在データプロット
plot(y ~ x, data=env_data, col = "darkgrey", xlim=c(120,150), ylim=c(22,47), pch = 16)
par(new=T)
plot(y ~ x, data=data_xy, col = "orange", xlim=c(120,150), ylim=c(22,47), pch = 16)


###### MaxEnt実行
bg_xy <-sample_n(tbl = env_data[, c(2,3)], size = 1000)
colnames(bg_xy) <- c("x", "y")
data <- prepareSWD(species = taxon, p = data_xy, a = bg_xy, env = xy2)

# 分布モデルの学習
model <- train(method = "Maxent", data = data, iter=1000)

# 学習データ点での生息適度予測値を計算
pred <- predict(model, data = data, type = "cloglog")

# 生息適度予測値を全グリッドで計算
map <- predict(model, data = xy2, type = "cloglog")
names(map) <- taxon

#options(repr.plot.width=7, repr.plot.height=7)
plot(map, col=colorRampPalette(pal_bin)(50))
title(paste(taxon, "habitat suitability"))

# スキル統計量最大化による閾値選択
ths <- thresholds(model, type = "cloglog")[3,2]

binary_map <- map
names(binary_map) <- taxon

binary_map[binary_map[] < ths] <- 0
binary_map[binary_map[] >= ths] <- 1

# 二値化マップ（＝在不在ラスター）の図示
rasterVis::levelplot(binary_map,at=c(0,0.5,1),main=paste(taxon, "presence/absence prediction"),
                     margin=FALSE,colorkey=list(labels=list(at=c(0, 1), labels=c("predicted\nabsent", "predicted\npresent"))), col.regions=c("grey", "black"))


##### モデル評価
plotROC(model)

auc(model)

names(ths) <- "Threshold under max TSS"
ths

# 交差検証
folds <- randomFolds(data, k = 4, only_presence = TRUE, seed = 25)

cv_model <- train("Maxent", data = data, folds = folds)
cv_model

temp <- c(auc(cv_model), auc(cv_model, test = TRUE))
names(temp) <- c("Mean of AUC of training data across CV", "Mean of AUC of test data across CV")
temp




