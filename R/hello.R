cross_validate<-function(df,dep,n_indep,n_iter,sr)
{
  library("caTools")
  library("rpart")
  #We will compare two models- predictor
  indep=list()
  print("Enter the independent variables")
  i<-1
  while(i<=n_indep){
    str=readline()
    indep[[i]]<-str
    i<-i+1
  }
  b <- paste(indep, collapse ="+")
  mean_subset<-c();
  mean_all<-c();
  relation_subset=as.formula(paste(dep,b,sep="~"))
  relation_all=as.formula(paste(dep,'.',sep="~"))
  for(i in 1:n_iter){
  spl <- sample.split(df[,dep], SplitRatio = sr)
  #Randomly splits into two subsets 0.7 and 0.3 of the original file
  #into training and testing
  train <- subset(df, spl == TRUE)
  testing <- subset(df, spl ==FALSE)
  first.tree<-rpart(relation_subset,data=train)
  second.tree<-rpart(relation_all, data=train)
  pred1.tree<-predict(first.tree,newdata=testing, type='class')
  pred2.tree<-predict(second.tree, newdata=testing, type='class')
  mean1<-mean(pred1.tree==testing[,dep])
  mean2<-mean(pred2.tree==testing[,dep])
  mean_subset<-c(mean_subset,mean1);
  mean_all<-c(mean_all,mean2);
  } 
  return (c(mean(mean_subset),mean(mean_all)))
}
