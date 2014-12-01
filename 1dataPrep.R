oneDataPrep <- function(){
#create new columns for JH and NS
pg$V5 <- NA; pg$V6 <- NA; pg$V7 <- NA; pg$V8 <- NA; pg$V9 <- NA; pg$V10 <- NA; pg$V11 <- NA; pg$V12 <- NA; pg$V13 <- NA; pg$V14 <- NA; pg$V15 <- NA; pg$V16 <- NA; pg$V17 <- NA; pg$V18 <- NA; pg$V19 <- NA; pg$V20 <- NA

#write column names for JH and NS
colnames(pg) <- c("coder","NA","beats","hand","nearBeat","beatsOther", "dist", "distAbs", "distCat", "dist0f", "dist1f", "dist2f", "dist3f", "dist4f", "dist5f", "dist6f", "dist7f", "dist8f", "dist9f", "dist10f")

#set one vector to of each raters’ number observations
JHn <<- length (grep('JH', pg$coder))
NSn <<- length (grep('NS', pg$coder)) 

#write to columns “nearBeat” the value of the row number of the other rater value (“beat”) that is closest to the value of the jh rater of each row
pg$nearBeat <- sapply (1:(NSn+JHn), function(x) which(abs(pg$beats[(JHn+1):(NSn+JHn)]-pg$beats[x])==min(abs(pg$beats[(JHn+1):(NSn+JHn)]-pg$beats[x]))))

pg <<- pg
}