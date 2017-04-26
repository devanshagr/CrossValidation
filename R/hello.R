cross_validate<-function(df,tree,n_iter,sr)
{
  df <- as.data.frame(df)
  library("rpart")
  #We will compare two models- predictor
  mean_subset<-c();
  mean_all<-c();
  contro=tree$control
  dep<-all.vars(terms(tree))[1]
  indep<-list()
  relation_all=as.formula(paste(dep,'.',sep="~"))
  i<-1
  while(i<length(all.vars(terms(tree)))){
    indep[[i]]<-all.vars(terms(tree))[i+1]
    i<-i+1
  }
  b <- paste(indep, collapse ="+")
  relation_subset<-as.formula(paste(dep,b,sep="~"))
  for(i in 1:n_iter){
  sample <- sample.int(n = nrow(df), size = floor(sr*nrow(df)), replace = F)
  train <- df[sample, ]
  testing  <- df[-sample, ]
  type = typeof(unlist(testing[dep])) 
  first.tree<-rpart(relation_subset, data=train, control=contro)
  second.tree<-rpart(relation_all, data=train)
  if(type=="double" | type=="integer"){
      pred1.tree<-predict(first.tree,newdata=testing)
      pred2.tree<-predict(second.tree, newdata=testing)
      mean1<-mean((as.numeric(pred1.tree)-testing[,dep])^2)
      mean2<-mean((as.numeric(pred2.tree)-testing[,dep])^2)
      mean_subset<-c(mean_subset,mean1)
      mean_all<-c(mean_all,mean2)
    }
  else{
  pred1.tree<-predict(first.tree,newdata=testing, type='class')
  pred2.tree<-predict(second.tree, newdata=testing, type='class')
  mean1<-mean(as.character(pred1.tree)==testing[,dep])
  mean2<-mean(as.character(pred2.tree)==testing[,dep])
  mean_subset<-c(mean_subset,mean1)
  mean_all<-c(mean_all,mean2)
    }
  } 
  return (data.frame(accuracy_subset=mean_subset,accuracy_all= mean_all))
}
