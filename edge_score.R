edge_score<-function(l, standard_inc=0.5){
	
	lenl<-length(l)
	
	if (lenl<2){return(0)}
	
	#sort the input l 
	l_sort<-sort(l)
	
	#find edge score for l
	edge<-sum(log10(l_sort[2:lenl]-l_sort[1:(lenl-1)]+1))/((lenl-1)*log10(standard_inc+1))/lenl
	
	return(10^edge)
	
	
}
