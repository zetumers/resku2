library(diagram)
par(mfrow=c(1,1))
par(mar=c(0,0,0,0))
##initialize new grphics device
openplotmat()



##number of elements per row
elpos<-coordinates (c(1,1, 2, 2, 2, 3,2 ))
##draw arrows from each row to next row
#treearrow(from=elpos[1,],to=elpos[2,],lwd=6)  
treearrow(from=elpos[2,],to=elpos[3:4,],lwd=6) 
treearrow(from=elpos[3,],to=elpos[5,],lwd=6)  
treearrow(from=elpos[4,],to=elpos[6,],lwd=6)  

treearrow(from=elpos[5,],to=elpos[7:8,],lwd=6)  

treearrow(from=elpos[7,],to=elpos[9:10,],lwd=6)
treearrow(from=elpos[8,],to=elpos[11,],lwd=6)  

treearrow(from=elpos[9,],to=elpos[12,],lwd=6)  
treearrow(from=elpos[10,],to=elpos[13,],lwd=6)

##create a generic 3-lined label for each textbox
labels = vector(length=13)
for(i in 1:13) { 
        labels[i] = paste(c(sample(letters, 3), "\n", sample(letters, 3) , "\n", sample(letters, 3)), collapse="")
}
labels[12] = "Consistent with AAP\nguidelines"

labels[1] = paste("ReSKU patients \n n = ", length(unique(resku_df$record_id)))

##plot text boxes
for ( i in 1:13) textround(elpos[i,],radx=0.08,rady=0.05,lab=labels[i],  box)