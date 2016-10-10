library(visNetwork)


zoomedplot = function(g, lay, sec, std) {
dev = max(apply(lay, 2, sd))
#print(dev)
lines = seq(-std*dev,std*dev, length.out = sec-1)
abline(h=lines, v=lines, lty=3, xpd=FALSE)
line_df = data.frame(lower=c(-Inf,lines),upper=c(lines,Inf), horz=LETTERS[seq(1,sec)], vert = seq(sec,1))
return(line_df)
}

actor_position = function(lay, line_df) {
#line_df[,1:2] = line_df[,1:2]*1000
lay_ret = vector(mode='character', length = nrow(lay))
lay_ret2 = vector(mode='character', length = nrow(lay))



for (k in 1:nrow(lay) ) {
 for (i in 1:nrow(line_df)) {
  if (line_df$lower[i]<lay[k,1] & lay[k,1]<=line_df$upper[i]) {
   for (j in 1:nrow(line_df)) {     
    if (line_df$lower[j]<lay[k,2] & lay[k,2]<=line_df$upper[j]) {
     lay_ret[k] = paste(line_df$horz[i],as.character(line_df$vert[j]),sep='')
#     print(paste(line_df$lower[i],lay[k,1],line_df$upper[i],sep=';'))
#     print(paste(line_df$lower[j],lay[k,2],line_df$upper[j],sep=';'))
#     print(paste(k,line_df$horz[i],line_df$vert[j],sep='; '))
    }
   }
  }
 }
}
return(lay_ret)
}


normalize_layout = function(layout){
min = apply(layout, 2, min)
max = apply(layout, 2, max)
layout[,1] = (layout[,1] - min[1] ) / (max[1]-min[1])*2 - 1
layout[,2] = (layout[,2] - min[2] ) / (max[2]-min[2])*2 - 1
return(layout)
}

create_boxes = function(zb) {
df = data.frame(box=character(),
	xleft=numeric(), 
      ybottom=numeric(),
      xright=numeric(), 
	ytop=numeric(),
	stringsAsFactors=FALSE) 
count=0
for (i in 1:nrow(zb)) {
 for (j in nrow(zb):1) {
  count = count+1
  df[count,] = c( paste(zb$horz[i],zb$vert[j],sep='')
			,zb$lower[i]
			,zb$lower[j]
			,zb$upper[i]
			,zb$upper[j] )
 }
}
min_par = min(abs(par('usr')))
df$xleft[df$xleft==-Inf] = -min_par
df$xright[df$xright==Inf] = min_par
df$ybottom[df$ybottom==-Inf] = -min_par
df$ytop[df$ytop==Inf] = min_par
df$xleft = as.numeric(df$xleft)
df$xright = as.numeric(df$xright)
df$ybottom = as.numeric(df$ybottom)
df$ytop = as.numeric(df$ytop)
return(df)
}

recttext <- function(box, n) {
 text = box$box[n]
 xl = box$xleft[n]
 yb = box$ybottom[n]
 xr = box$xright[n] 
 yt = box$ytop[n]
 center <- c(mean(c(xl, xr)), mean(c(yb, yt)))  
 do.call('rect', c(list(xleft = xl, ybottom = yb, xright = xr, ytop = yt, col='black')))
 do.call('text', c(list(x = center[1], y = center[2], labels = text, bg='black', col='white')))
}

save_boxes = function(g, gl, clr, boxes, n, act) {
#dev.new()
par(mar = c(3,3,3,3),xpd=TRUE)
#par(din = c(7,7))
plot	( g , vertex.size = .5
	, vertex.color = clr
	, vertex.label.cex=1
	, vertex.label.color = 'black'
	, vertex.label.dist = -.005	
	, vertex.label = ifelse(clr=='blue' & act$box==boxes$box[n], get.vertex.attribute(g, "fullname"),NA)	
	, vertex.frame.color = NA
	,layout = gl
	,edge.width = 0.25
	,edge.color = "#A9A9A970"
	,xlim=c(boxes$xleft[n],boxes$xright[n]), ylim=c(boxes$ybottom[n],boxes$ytop[n])
)
#print(c(boxes$xleft[n],boxes$xright[n]))
#print(c(boxes$ybottom[n],boxes$ytop[n]))
rect(xleft=boxes$xleft[n], xright=boxes$xright[n]
	, ybottom=boxes$ybottom[n], ytop=boxes$ytop[n], lty = 3, border = 'black', xpd=FALSE)

}

overall.plot = function(gr,clr,lo) {
#dev.new(width=40,height=40)
plot	( gr, vertex.size = .5
	, vertex.color = clr
#	, vertex.label.cex=.125
#	, vertex.label.color = 'black'
#	, vertex.label.dist = -.005	
	, vertex.label = NA
	, vertex.frame.color = NA
	, edge.width = 0.25
	, edge.color = "#A9A9A970"
	,layout = lo	
) 
#dev.copy2pdf(file = paste(out,".pdf",sep=''), width=40, height=40, out.type = 'pdf')
}
zoom.plot = function(gr,clr,lo,xn,xx,yn,yx) {
  str(gr)
  head(gr)
  #dev.new(width=40,height=40)
  plot	( gr, vertex.size = .5
         , vertex.color = clr
         #	, vertex.label.cex=.125
         #	, vertex.label.color = 'black'
         #	, vertex.label.dist = -.005	
         , vertex.label = NA
         , vertex.frame.color = NA
         , edge.width = 0.25
         , edge.color = "#A9A9A970"
         , layout = lo	
         , xlim=c(xn,xx)
         , ylim=c(yn,yx)
  ) 
  #dev.copy2pdf(file = paste(out,".pdf",sep=''), width=40, height=40, out.type = 'pdf')
}


 watermarkBFW <- function(lab = "PROOF ONLY",y=-.75,x=0){

text(x,y,lab, cex=3, col='#D3D3D380', font=3)
}
