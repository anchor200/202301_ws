layers <- 0
sdm_stacker <- NA
sdm_stacker <- NA
thss <- NA
sp.label <- NA

for (ii in 1:length(sp_list)){
    
    taxon <- sp_list[ii]

    data_temp <- subset(data_use, data_use$species==taxon)
    data_temp <- data_temp[!duplicated(data_temp$mesh2), c(2,3)]

    if (nrow(data_temp) < 6){
        print(c("data too few", taxon))
        next
    }
    
    colnames(data_temp) <- c("x", "y")
    bg_temp <-sample_n(tbl = env_data[, c(2,3)], size = 1000)
    colnames(bg_temp) <- c("x", "y")
    data <- prepareSWD(species = taxon, p = data_temp, a = bg_temp,env = xy2)

    model <- train(method = "Maxent", data = data, iter=1000)
    
    # 分布モデルの学習
    pred <- predict(model, data = data, type = "cloglog")

    # 分布予測
    map <- predict(model, data = xy2, type = "cloglog")
    names(map) <- taxon
    ths <- thresholds(model, type = "cloglog")[3,2]
    
    cont_map <- map
    names(cont_map) <- taxon
    
    map[map[] < ths] <- 0
    map[map[] >= ths] <- 1
    
    if(layers==0){
            shm_stacker  = cont_map
            sdm_stacker  = map
            thss <- data.frame("taxon"=taxon, "threshold"=ths)
        }else{
            shm_stacker  = c(shm_stacker, cont_map) #stack(c(shm_stacker, cont_map))
            sdm_stacker  = c(sdm_stacker, map) #stack(c(sdm_stacker, map))
            thss <- rbind(thss, data.frame("taxon"=taxon, "threshold"=ths))
    }
    layers <- layers + 1

}

sdm_stack <- stack(sdm_stacker)
shm_stack <- stack(shm_stacker)


    