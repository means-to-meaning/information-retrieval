library(stringr)

#############################################
# Helper Functions
#############################################
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
  dict.title <- gsub("[^[a-z]]","",dict.title)
  #remove common words
  #TODO: get proper dict for common
  common.words <- c("and","not","which","to","at","the","in","is","for","pa")
  dict.title <- dict.title[!dict.title %in% common.words]
  #strings which are not alphabetic characters only
  dict.title <- dict.title[grepl("[[:alpha:]+]",dict.title)]
  dict.title <- unique(dict.title)
  return(dict.title)
}

getSalaryforDictionary <- function(clean.dict,clean.title.list,train.salary)
{
  relevant.titles.binind <- unlist(lapply(clean.title.list,function(title) {clean.dict %in% title}))
  word.avg.salary <- mean(train.salary[relevant.titles.binind])
  return(word.avg.salary)
}

#############################################
# Loading data
#############################################

train <- read.csv("../data/Train_rev1.csv",stringsAsFactors=F)
train_titleText <- train[,1:2]
train_descText <- train[,c(1,3)]
train_data <- train[,-c(2,3)]
save(train_titleText,file="../data/train_titleText.RData")
save(train_descText,file="../data/train_descText.RData")
save(train_data,file="../data/train_data.RData")

test <- read.csv("../data/Valid_rev1.csv",stringsAsFactors=F)
test_titleText <- test[,1:2]
test_descText <- test[,c(1,3)]
test_data <- test[,-c(2,3)]
save(test_titleText,file="../data/test_titleText.RData")
save(test_descText,file="../data/test_descText.RData")
save(test_data,file="../data/test_data.RData")


##############################################
# Location data

loctree <- read.csv("../data/Location_Tree.csv",stringsAsFactors=F,head=F)
#how many levels are in the tree?
tree.depth <- apply(loctree,1,str_count,"~")
table(tree.depth)

loc.list <- apply(loctree,1,strsplit,"~")
loc.list <- lapply(loc.list,unlist)
location.mat <- list.to.charmatrix(loc.list)
colnames(location.mat) <- c("LocationL1","LocationL2","LocationL3","LocationL4","LocationL5","LocationL6","LocationL7")
save(location.mat,file="../data/location.RData")


#get location data from the location tree using normalized location
train_locs <- t(sapply(train_data$LocationNormalized,getTreeFromLoc,location.mat))
test_locs <- t(sapply(test_data$LocationNormalized,getTreeFromLoc,location.mat))

save(train_locs,file="../data/train_locs.RData")
save(test_locs,file="../data/test_locs.RData")





