library(stringr)

load("../data/train_titleText.RData")
load("../data/test_titleText.RData")
load("../data/train_data.RData")
load("../data/test_data.RData")

#############################################
# Helper Functions
#############################################


createTupplesfromCharVec <- function(vec,d)
{
  indCols <- length(vec)
  indRows <- indCols-d+1
  if (indCols >= d){
  ind.mat <- matrix(rep(1:d,indRows),ncol=d,byrow=T)
  ind.mat <- ind.mat + matrix(rep((1:indRows)-1,each=d),ncol=d,byrow=T)
  res <- apply(ind.mat,1,function(x) {paste(vec[x],collapse=" ")})
  } else {res <- NA}
  return(res)
}

list.to.charmatrix <- function(data) {
  nCol <- max(vapply(data, length, 0))
  data <- lapply(data, function(row) c(row, rep(NA, nCol-length(row))))
  data <- matrix(unlist(data), nrow=length(data), ncol=nCol, byrow=TRUE)
}

# given a normalized location try to get the corresponding tree from loctree
getTreeFromLoc <- function(x,location.df)
{
  #print(x)
  tree.depth <- ncol(location.df)
  isInLoc <- (location.df==x)
  selected.loc <- location.df[rowSums(isInLoc,na.rm=T)>0,]
  
  if (length(selected.loc)>0){
    if (class(selected.loc)=="matrix"){
      deepestMatchedLevelinTree <- max(which(x==selected.loc[1,]))
      #discard all the entries below the deepest matched level
      res.loc.tree <- c(selected.loc[1,1:deepestMatchedLevelinTree],rep(NA,tree.depth-deepestMatchedLevelinTree))                                           
    }
    if (class(selected.loc)=="character"){
      deepestMatchedLevelinTree <- max(which(x==selected.loc))
      res.loc.tree <- c(selected.loc[1:deepestMatchedLevelinTree],rep(NA,tree.depth-deepestMatchedLevelinTree))
    }   
    
  }else{
    res.loc.tree <- rep(NA,tree.depth)
  }
  return(res.loc.tree)
}

cleanStringVec <- function(dict.title)
{
  #remove all strings with a *
  dict.title <- dict.title[!grepl("[*]",dict.title)]
  #convert everything to lowercase
  dict.title <- tolower(dict.title)
  #remove all non alphabetic characters from strings
  dict.title <- gsub("[^a-z]","",dict.title)
  #remove common words
  #TODO: get proper dict for common
  common.words <- c("and","not","which","to","at","the","in","is","for","pa")
  dict.title <- dict.title[!dict.title %in% common.words]
  #strings which are not alphabetic characters only
  dict.title <- dict.title[grepl("[[:alpha:]+]",dict.title)]
  dict.title <- unique(dict.title)
  return(dict.title)
}


getSalaryforDictionary <- function(x,clean.title.list,train.salary)
{
  relevant.titles.binind <- unlist(lapply(clean.title.list,function(title) {x %in% title}))
  word.avg.salary <- mean(train.salary[relevant.titles.binind],na.rm=T)
  word.sd.salary <- sd(train.salary[relevant.titles.binind],na.rm=T)
  return(c(word.avg.salary,word.sd.salary))
}
getSalaryFromTitle <-function(title,word.title.mod)
{
    #calculate avg. salary
    res <- mean(as.numeric(word.title.mod[!is.na(match(word.title.mod[,1],title)),2]),na.rm=T)
    #calculate sd salary
    res <- c(res,mean(as.numeric(word.title.mod[!is.na(match(word.title.mod[,1],title)),3]),na.rm=T))
    return(res)
}


################################################
# Model salary ~ words in title
################################################
#get dictionary for title texts
title.list <- sapply(train_titleText[,2],strsplit," |[/]|[,]")
clean.title.list1 <- lapply(title.list,cleanStringVec)
clean.dict1 <- unique(unlist(clean.title.list1))
# clean.title.list2 <- lapply(clean.title.list,createTupplesfromCharVec,d=2)
# clean.dict2 <- unique(unlist(clean.title.list2))
# clean.title.list3 <- lapply(clean.title.list,createTupplesfromCharVec,d=3)
# clean.dict3 <- unique(unlist(clean.title.list3))


#simply model for the average salary corresponding to a word or a word tupple
#get avg. salary corresponding to each word from the dict.
word.avg.salary <- t(sapply(clean.dict1,getSalaryforDictionary,clean.title.list1,train_data$SalaryNormalized))
word.title.mod1 <- data.frame("term"=clean.dict1,"salary"=word.avg.salary[,1],"sd"=word.avg.salary[,2])

# word.avg.salary2 <- sapply(clean.dict2,getSalaryforDictionary,clean.title.list2,train_data$SalaryNormalized)
# word.title.mod2 <- data.frame("term"=clean.dict2,"salary"=word.avg.salary2[,1],"sd"=word.avg.salary2[,2])
# 
# word.avg.salary3 <- sapply(clean.dict3,getSalaryforDictionary,clean.title.list3,train_data$SalaryNormalized)
# word.title.mod3 <- data.frame("term"=clean.dict3,"salary"=word.avg.salary3[,1],"sd"=word.avg.salary3[,2])

# save(word.title.mod1,word.title.mod2,word.title.mod3,file="../data/wordTitleModel.RData")
save(word.title.mod1,file="../data/wordTitleModel.RData")


salaryFromTitle1 <- matrix(unlist(lapply(clean.title.list1,getSalaryFromTitle,word.title.mod1)),ncol=2,byrow=T)
#salaryFromTitle2 <- matrix(unlist(lapply(clean.title.list2,getSalaryFromTitle,word.title.mod2)),ncol=2,byrow=T)
#salaryFromTitle3 <- matrix(unlist(lapply(clean.title.list3,getSalaryFromTitle,word.title.mod3)),ncol=2,byrow=T)

#train_salaryFromTitle <- cbind(train_data$Id,salaryFromTitle1,salaryFromTitle2,salaryFromTitle3)
train_salaryFromTitle <- cbind(train_data$Id,salaryFromTitle1)

#####################################
# get the salary for the test data using train data vocabulary

#get dictionary for title texts
test_title.list <- sapply(test_titleText[,2],strsplit," |[/]|[,]")
test_clean.title.list1 <- lapply(test_title.list,cleanStringVec)
# clean.title.list2 <- lapply(clean.title.list,createTupplesfromCharVec,d=2)
# clean.dict2 <- unique(unlist(clean.title.list2))
# clean.title.list3 <- lapply(clean.title.list,createTupplesfromCharVec,d=3)
# clean.dict3 <- unique(unlist(clean.title.list3))


salaryFromTitle1 <- matrix(unlist(lapply(test_clean.title.list1,getSalaryFromTitle,word.title.mod1)),ncol=2,byrow=T)
#salaryFromTitle2 <- matrix(unlist(lapply(clean.title.list2,getSalaryFromTitle,word.title.mod2)),ncol=2,byrow=T)
#salaryFromTitle3 <- matrix(unlist(lapply(clean.title.list3,getSalaryFromTitle,word.title.mod3)),ncol=2,byrow=T)

#test_salaryFromTitle <- cbind(test_data$Id,salaryFromTitle1,salaryFromTitle2,salaryFromTitle3)
test_salaryFromTitle <- cbind(test_data$Id,salaryFromTitle1)

save(train_salaryFromTitle,file="../data/train_SalaryFromTitle.RData")
save(test_salaryFromTitle,file="../data/test_SalaryFromTitle.RData")
#TODO:
#create a matrix for each title indicating which terms occured
##incidence.title.mat <- lapply(title.list,function(x) {clean.dict %in% x})
