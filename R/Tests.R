# This section of the tree_analyzer software is dedicated to defining and writing tests.
# Tests should return results

# to install setwd("/Users/samhamilton/Library/Mobile Documents/com~apple~CloudDocs/Thesis_Aim1/libraries/TreeAnalyzer")
# devtools::document() ; devtools::build() ; devtools::install()
#' @import randomForest
#' @import dplyr
#' @import rpart
#' @import dplyr
#' @import stats
#' @import rpart.plot
#' @import pheatmap
#' @import RColorBrewer
#'RunTests
#'
#'@param tests a list of functions that run tests
#'@export
RunTests = function(tests, testdir = "./tests/",seed = "1234"){
  set.seed(seed)
  dir.create(testdir)
  
  test_results = lapply(tests, function(test_info) {
      test_result = do.call(test_info[[1]], c(list(testdir), test_info[-1]))
      return(test_result)
  })

  return(test_results)
}

#'iris_test
#'
#'@param testdir where to save the plots 
#'@param test_type which iris test to run, options "empty", "filled"
#'@export
iris_test <- function(testdir,test_type = "empty"){
  
  #Builds the iris object
  get_iris_tree <- function(){
  data(iris)
    set.seed(1234)
  iris_tree = randomForest(Species ~ .,data = iris,maxnodes = 6) %>% 
    getTree(k = 5,labelVar=T)  %>% 
    as.data.frame()
  
  return(iris_tree) }
  
  # plots the empty adapted rpart object and saves
  make_empty_adapted <- function(iris_tree){
    
    iris_adapted = iris_tree %>% 
      adapt_rf2rpart(add_data = F)
  }
  
  make_filled_adapted <- function(iris_tree){
    
    iris_adapted = iris_tree %>% 
      adapt_rf2rpart(add_data = T,X = iris[,-5],y = iris[,5])
  }
  
  make_filled_adapted_remove_setosa <- function(iris_tree){
    iris_wrong = iris 
    iris_wrong$Species[iris_wrong$Species == "setosa"] = "virginica"
    iris_adapted = iris_tree %>% 
      adapt_rf2rpart(add_data = T,X = iris_wrong[,-5],y = iris_wrong[,5])
  }
  
  save_and_plot <- function(adapted_tree, saveloc){
    pdf(saveloc)
    rpart.plot(adapted_tree,extra = 1,type = 2)
    dev.off()
  }

### main function executed here
iris_tree <- get_iris_tree()

if(test_type == "empty"){
  iris_adapted = make_empty_adapted(iris_tree)
}
if(test_type == "filled"){
  iris_adapted =  make_filled_adapted(iris_tree)
}

if(test_type == "filled_remove_setosa"){
  iris_adapted =  make_filled_adapted_remove_setosa(iris_tree)
}
save_and_plot(iris_adapted,saveloc = paste0(testdir,"iris_",test_type,".pdf"))

return(paste0("iris_test",test_type,"complete"))
  
}
  

test_list = list(
  list(iris_test, test_type = "empty"),
  list(iris_test, test_type = "filled"),
  list(iris_test, test_type = "filled_remove_setosa")
)
set.seed(100)
RunTests(test_list, testdir = "~/tests/")
iris_test(testdir = "~/tests/",test_type = "filled_remove_setosa")

iris_wrong = iris
iris_wrong$Species = "setosa"

iris_adapted = iris_tree %>% 
  adapt_rf2rpart(add_data = T,X = iris_wrong[,-5],y = iris_wrong[,5])

rf_tree = iris_tree

