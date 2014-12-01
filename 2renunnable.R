twoRerunnable <- function (){
  
#write to columns beatsOther the value of the other rater that is closest to the value of this rater of each row 
pg$beatsOther <- pg$beats[
  JHn + sapply (
    1:(JHn+NSn), function(x) 
      which (pg$nearBeat[(JHn+1):(JHn+NSn)]==pg$nearBeat[x])
  )
]

#compute difference between beats and beatsOhter
pg$dist <- pg$beats - pg$beatsOther

#compute absolute difference between beats and beatsOhter
pg$distAbs <- abs(pg$beats - pg$beatsOther)

#show whether it is perfect (0), within one to four frames tolerance (0/1/2/3/4/5/6/7/8/9/10/more)
pg$distCat <- sapply (1:(JHn+NSn), function(x) if(pg$distAbs[x]<0.003) "0" else if(pg$distAbs[x]<0.045) "1" else if(pg$distAbs[x]<0.087) "2" else if(pg$distAbs[x]<0.129) "3" else if(pg$distAbs[x]<0.171) "4" else if(pg$distAbs[x]<0.213) "5" else if(pg$distAbs[x]<0.255) "6" else if(pg$distAbs[x]<0.297) "7" else if(pg$distAbs[x]<0.339) "8" else if(pg$distAbs[x]<0.381) "9" else if(pg$distAbs[x]<0.423) "10" else "more")

#write a column that only shows the tolerated deviations
pg$dist0f <- sapply (1:(JHn+NSn), function(x) if(pg$distCat[x]=="0") pg$dist[x] else NA)

pg$dist1f <- sapply (1:(JHn+NSn), function(x) if(pg$distCat[x]=="0"|pg$distCat[x]=="1") pg$dist[x] else NA)

pg$dist2f <- sapply (1:(JHn+NSn), function(x) if(pg$distCat[x]=="0"|pg$distCat[x]=="1"|pg$distCat[x]=="2") pg$dist[x] else NA)

pg$dist3f <- sapply (1:(JHn+NSn), function(x) if(pg$distCat[x]=="0"|pg$distCat[x]=="1"|pg$distCat[x]=="2"|pg$distCat[x]=="3") pg$dist[x] else NA)

pg$dist4f <- sapply (1:(JHn+NSn), function(x) if(pg$distCat[x]=="0"|pg$distCat[x]=="1"|pg$distCat[x]=="2"|pg$distCat[x]=="3"|pg$distCat[x]=="4") pg$dist[x] else NA)

pg$dist5f <- sapply (1:(JHn+NSn), function(x) if(pg$distCat[x]=="0"|pg$distCat[x]=="1"|pg$distCat[x]=="2"|pg$distCat[x]=="3"|pg$distCat[x]=="4"|pg$distCat[x]=="5") pg$dist[x] else NA)

pg$dist6f <- sapply (1:(JHn+NSn), function(x) if(pg$distCat[x]=="0"|pg$distCat[x]=="1"|pg$distCat[x]=="2"|pg$distCat[x]=="3"|pg$distCat[x]=="4"|pg$distCat[x]=="5"|pg$distCat[x]=="6") pg$dist[x] else NA)

pg$dist7f <- sapply (1:(JHn+NSn), function(x) if(pg$distCat[x]=="0"|pg$distCat[x]=="1"|pg$distCat[x]=="2"|pg$distCat[x]=="3"|pg$distCat[x]=="4"|pg$distCat[x]=="5"|pg$distCat[x]=="6"|pg$distCat[x]=="7") pg$dist[x] else NA)

pg$dist8f <- sapply (1:(JHn+NSn), function(x) if(pg$distCat[x]=="0"|pg$distCat[x]=="1"|pg$distCat[x]=="2"|pg$distCat[x]=="3"|pg$distCat[x]=="4"|pg$distCat[x]=="5"|pg$distCat[x]=="6"|pg$distCat[x]=="7"|pg$distCat[x]=="8") pg$dist[x] else NA)

pg$dist9f <- sapply (1:(JHn+NSn), function(x) if(pg$distCat[x]=="0"|pg$distCat[x]=="1"|pg$distCat[x]=="2"|pg$distCat[x]=="3"|pg$distCat[x]=="4"|pg$distCat[x]=="5"|pg$distCat[x]=="6"|pg$distCat[x]=="7"|pg$distCat[x]=="8"|pg$distCat[x]=="9") pg$dist[x] else NA)

pg$dist10f <- sapply (1:(JHn+NSn), function(x) if(pg$distCat[x]=="0"|pg$distCat[x]=="1"|pg$distCat[x]=="2"|pg$distCat[x]=="3"|pg$distCat[x]=="4"|pg$distCat[x]=="5"|pg$distCat[x]=="6"|pg$distCat[x]=="7"|pg$distCat[x]=="8"|pg$distCat[x]=="9"|pg$distCat[x]=="10") pg$dist[x] else NA)


pg <<- pg


#calculate the standard deviation, ignore NAs
sd(pg$dist0f[1:JHn], na.rm=TRUE)
sd(pg$dist1f[1:JHn], na.rm=TRUE)
sd(pg$dist2f[1:JHn], na.rm=TRUE)
sd(pg$dist3f[1:JHn], na.rm=TRUE)
sd(pg$dist4f[1:JHn], na.rm=TRUE)
sd(pg$dist5f[1:JHn], na.rm=TRUE)
sd(pg$dist6f[1:JHn], na.rm=TRUE)
sd(pg$dist7f[1:JHn], na.rm=TRUE)
sd(pg$dist8f[1:JHn], na.rm=TRUE)
sd(pg$dist9f[1:JHn], na.rm=TRUE)
sd(pg$dist10f[1:JHn], na.rm=TRUE)

#show how many beats are in which confInt
table (pg$distCat[1:JHn])


# Now, resolve conflicts:

#  1.  
#a.  When nearBeat contains two values (“[2:3]”) AND
#b.	(looking into Elan) they are both either occupied or unoccupied by another neighbour (ELSE write the number of the unoccupied candidate into the nearBeat cell )
#replace the nearBeat value with a distant number (e.g. if it is 1, put 11; if it is 33, put 1). 

#  2.	
#a.	When nearBeat is equal for more than one row AND 
#b.	more than one of these rows are tolerated AND
#c.	two are equally the closest (else replace the farther) AND
#d.	they are both either occupied or unoccupied by another neighbour (ELSE replace only occupied), 
#replace the nearBeat value of all except the closest neighbour by a distant number (e.g. if it is 1, put 11; if it is 33, put 1). 

#Then rerun the commands beginning exactly with “#write to columns beatsOther …”
}