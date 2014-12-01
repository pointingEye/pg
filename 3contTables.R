threeContTables <- function (){

#prepare for interrater test by a 2x2 table

#input length of recording
#secCoded <- 105 #fill in the stretch of video coded (by all raters) in the imported data by duration in seconds
print(c("secCoded =", secCoded, "!!"))
  
print("0 FRAMES TOLERANCE")
#both yes
print(length(which(!is.na(pg$dist0f[1:JHn]))))
#JHyes+NSno
print(length(which(is.na(pg$dist0f[1:JHn]))))
#JHno+NSyes
print(NSn - length(which(!is.na(pg$dist0f[1:JHn]))))
#both no
print((secCoded*24)- (length(which(!is.na(pg$dist0f[1:JHn]))) + length(which(is.na(pg$dist0f[1:JHn]))) + (NSn - length(which(!is.na(pg$dist0f[1:JHn]))))))

print("1 FRAME TOLERANCE")
#both yes
print(length(which(!is.na(pg$dist1f[1:JHn]))))
#JHyes+NSno
print(length(which(is.na(pg$dist1f[1:JHn]))))
#JHno+NSyes
print(NSn - length(which(!is.na(pg$dist1f[1:JHn]))))
#both no
print((secCoded*24/3)- (length(which(!is.na(pg$dist1f[1:JHn]))) + length(which(is.na(pg$dist1f[1:JHn]))) + (NSn - length(which(!is.na(pg$dist1f[1:JHn]))))))

print("2 FRAMES TOLERANCE")
#both yes
print(length(which(!is.na(pg$dist2f[1:JHn]))))
#JHyes+NSno
print(length(which(is.na(pg$dist2f[1:JHn]))))
#JHno+NSyes
print(NSn - length(which(!is.na(pg$dist2f[1:JHn]))))
#both no
print((secCoded*24/5)- (length(which(!is.na(pg$dist2f[1:JHn]))) + length(which(is.na(pg$dist2f[1:JHn]))) + (NSn - length(which(!is.na(pg$dist2f[1:JHn]))))))

print("3 FRAMES TOLERANCE")
#both yes
print(length(which(!is.na(pg$dist3f[1:JHn]))))
#JHyes+NSno
print(length(which(is.na(pg$dist3f[1:JHn]))))
#JHno+NSyes
print(NSn - length(which(!is.na(pg$dist3f[1:JHn]))) )
#both no
print((secCoded*24/7)- (length(which(!is.na(pg$dist3f[1:JHn]))) + length(which(is.na(pg$dist3f[1:JHn]))) + (NSn - length(which(!is.na(pg$dist3f[1:JHn]))))))

}