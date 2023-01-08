
# データの加工用ライブラリ
library(stringr)
library(dplyr)
library(reshape2)
library(data.table)

#データ可視化ライブラリ
library(ggplot2)
library(gridExtra)

# ラスターの加工用ライブラリ
library(raster)
library(sf)
library(lattice)
library(rasterVis)
library(RColorBrewer)

# 種分布モデリング (MaxEnt) 実行用ライブラリ
## 実行にはパスを通すなどの処理が必要になる。
library(SDMtune)
library(rJava)
library(dismo)
library(plotROC)

# 分析用ライブラリ
library(car)
library(sensemakr)
library(vegan)

# 緯度経度とメッシュコードの変換
## メッシュコード: https://www.stat.go.jp/data/mesh/m_itiran.html

mesh2get <- function(x, y){
    p <- (y * 60) %/% 40
    a <- (y * 60) %% 40
    u <- (x - 100) %/% 1
    f <- (x - 100) %% 1
    q <- a %/% 5
    b <- a %% 5
    v <- (f * 60) %/% 7.5
    g <- (f * 60) %% 7.5

    p * 10000 + u * 100 + q * 10 + v * 1     
}

latget <- function(mesh2){
    trunc(mesh2 / 10000) * 2/3 + trunc(mesh2 %% 100 / 10) * 2/3 / 8
}
longet <- function(mesh2){
    100 + trunc((mesh2 - trunc(mesh2 / 10000)*10000) / 100) + mesh2 %% 10 / 8
}

pal<- c("#220d3d", "#729696", "#9EBE91", "#E4B80E", "#ff0000","#f20085")
pal_bin<- c("#220d3d", "#729696", "#9EBE91", "#E4B80E")


