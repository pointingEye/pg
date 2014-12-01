
#short version: does not work
x <- pg[
  which(
    any( # not sure about this
      pg$beats>=x  & pg$beats<y
    )
  ) , 
  ]


#let's try to go from the which thing that I copied for the above script
sapply (1:(NSn+JHn), function(x) {
  which(
    abs(pg$beats[(JHn+1):(NSn+JHn)]-pg$beats[x])==min(
      abs(pg$beats[(JHn+1):(NSn+JHn)]-pg$beats[x]))
  )
})

x <- pg[
  function() {
    which(pg$beats>=180 & pg$beats>=195)
  },
  ]


x <- pg[which(pg$beats>=180),] #cannot open dataframe!

x <- pg[which(pg$beats==108.738),]#works!

x <- pg[which(pg$beats>=120  & pg$beats<135), ]   #cannot open dataframe!

x <- pg[pg@beats %in% (120:135),]   #doesn't work

x <- function(x) c( for (i in 0:10) i*60)  # doesn't work        