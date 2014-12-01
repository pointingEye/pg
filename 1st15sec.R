first15sec <- function (){
  
#long version; works but cannot display result via view()...
pg <<- pg[
  which(
    pg$beats>=0    & pg$beats<15  |
    pg$beats>=60   & pg$beats<75  |
    pg$beats>=120  & pg$beats<135 |
    pg$beats>=180  & pg$beats<195 |
    pg$beats>=240  & pg$beats<255 |
    pg$beats>=300  & pg$beats<315 |
    pg$beats>=360  & pg$beats<375 |
    pg$beats>=420  & pg$beats<435 |
    pg$beats>=480  & pg$beats<495 |
    pg$beats>=540  & pg$beats<555 |
    pg$beats>=600  & pg$beats<615 |
    pg$beats>=660  & pg$beats<675 |
    pg$beats>=720  & pg$beats<735 |
    pg$beats>=780  & pg$beats<795 |
    pg$beats>=840  & pg$beats<855 |
    pg$beats>=900  & pg$beats<915 |
    pg$beats>=960  & pg$beats<975 |
    pg$beats>=1020 & pg$beats<1035|
    pg$beats>=1080 & pg$beats<1095|
    pg$beats>=1140 & pg$beats<1155|
    pg$beats>=1200 & pg$beats<1215 
  ),
]

JHn <<- length (grep('JH', pg$coder))
NSn <<- length (grep('NS', pg$coder)) 

}

