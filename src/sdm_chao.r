OCC_DATA$mesh2 <- mesh2get(OCC_DATA$longitude, OCC_DATA$latitude)


# 観測回数をアバンダンスと解釈した、種数推定（種数蓄積曲線に基づく）
obs_abcount <- aggregate(list("obs_n"=rowSums(OCC_DATA[, c(9:17)], na.rm=T)), list("mesh2"=OCC_DATA$mesh2, "species"=OCC_DATA$species), sum)
obs_wide <-tidyr::spread(obs_abcount ,key=c("species"),value="obs_n")

obs_wide[is.na(obs_wide)] <- 0

sum_chao <- NA
for(id in 1:nrow(obs_wide)){

	temp <- data.frame("mesh"=obs_wide$mesh2[id])
	temp <- cbind(temp, data.frame(t(estimateR(obs_wide[id, c(2:ncol(obs_wide))]))))
	if(id == 1){
		sum_chao <- temp
	}else{
		sum_chao <- rbind(sum_chao, temp)
	}
	

}

plot(S.chao1 ~ S.obs, data=sum_chao)



        