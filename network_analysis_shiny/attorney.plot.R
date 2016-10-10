#rm(attorney.plot)
attorney.plot = function(gr,att,clr,lo,out) {
dev.new(width=40,height=40)
plot	( gr, vertex.size = .5
	, vertex.color = clr
	, vertex.label.cex=.125
	, vertex.label.color = 'black'
	, vertex.label.dist = -.005	
	, vertex.label = ifelse(clr=='blue', get.vertex.attribute(gr, att),NA)	
	, vertex.frame.color = NA
	,layout = lo	
	,edge.width = .25
) 
dev.copy2pdf(file = paste(out,".pdf",sep=''), width=40, height=40, out.type = 'pdf')
return()
}

##	, vertex.label = ifelse(clr=='blue' & comm, get.vertex.attribute(g, 'fullname'),NA)	

#backup
#rm(attorney.comm.plot)
attorney.comm.plot = function(gr,sgr,att,clr,lo,out,comm,comm_num) {
dev.new(width=20,height=20)
plot	( gr %s% sgr, vertex.size = .5
	, vertex.color = clr
	, vertex.label.cex=.125
	, vertex.label.color = 'black'
	, vertex.label.dist = -.005	
	, vertex.label = ifelse(clr=='blue' & comm, get.vertex.attribute(gr, att),NA)	
	, vertex.frame.color = NA
	, layout = lo	
	, edge.width = .25
	, main = paste('Community Number:',comm_num)
	) 
dev.copy2pdf(file = paste(out,comm_num,".pdf",sep=''), width=20, height=20, out.type = 'pdf')
}