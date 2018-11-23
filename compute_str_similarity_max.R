##function compute structure similarity F5 F6
compute_str_similarity_max<-function(sub1){
  options(digits=4)
  
  score={}
  #sub1<-sub1[,which(colSums(sub1) > 0)] # remove cols with all zero
      
      both_zero=mapply(function(i) (sum(as.numeric(as.numeric(sub1[i,])+as.numeric(sub1[1,])==0))), i=2:nrow(sub1)) ##factor 1 or 2
      both_one=mapply(function(i) (sum(as.numeric(as.numeric(sub1[i,])+as.numeric(sub1[1,])==2))),i=2:nrow(sub1))
      both_diff=mapply(function(i) (sum(as.numeric(as.numeric(sub1[i,])+as.numeric(sub1[1,])==1))), i=2:nrow(sub1))
      
      denomi=both_diff+both_one
      
      for_calc<-data.frame(both_one, denomi)
      for_calc<-for_calc[for_calc$denomi!=0,]
      
      #      print(denomi)
      
      if(nrow(for_calc)!=0 ){  
        score<-c(score, mapply(function(i) (for_calc$both_one[i]/for_calc$denomi[i]),i=1:nrow(for_calc)))
      }else{
        score<-c(score,-10)
      }
    if(length(score)!=0){     
        F56=max(score)
    }else{
      message("something wrong")
    }   
  #print(F56)
  return(F56)
}
