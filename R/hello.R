cross_validate<-function(df,tree,n_iter,sr)
{
  library("rpart")
  #We will compare two models- predictor
  mean_subset<-c();
  mean_all<-c();
  dep<-all.vars(terms(tree))[1]
  relation_all=as.formula(paste(dep,'.',sep="~"))
  for(i in 1:n_iter){
  sample <- sample.int(n = nrow(df), size = floor(sr*nrow(df)), replace = F)
  train <- df[sample, ]
  testing  <- df[-sample, ]
  second.tree<-rpart(relation_all, data=train)
  pred1.tree<-predict(tree,newdata=testing, type='class')
  pred2.tree<-predict(second.tree, newdata=testing, type='class')
  mean1<-mean(as.character(pred1.tree)==testing[,dep])
  mean2<-mean(as.character(pred2.tree)==testing[,dep])
  mean_subset<-c(mean_subset,mean1)
  mean_all<-c(mean_all,mean2)
  } 
  return (data.frame(mean_subset, mean_all))
}
