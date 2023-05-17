
#' adapt_rf2rpart
#' 
#' adapter function designed to take information from the rf_tree object to the 
#' rpart style object so that the plotting capabilities of the rpart object can be 
#' leveraged.
#'  
#'@param rf_tree  the random forest tree object
#'@param add_data should information from a compatible df
#'                 be used to populate tree info? T/F
#'@param X        if add_data = T, what is the predictor matrix
#'@param y        if add_data = T  what are the true values?

adapt_rf2rpart <- function(rf_tree,add_data,X,y){
  
  # Init the rpart object
  rpart_obj = init_rpart()    %>%   # Initialize the object
    mod_rpart_frame(rf_tree)   %>%    # Modify the frame aspect of the rpart object
    mod_rpart_ylevels(rf_tree) %>%    # Change the ylevels
    mod_rpart_splits(rf_tree)         # modify the splits part of the object
  
  if(add_data == T){
    rpart_obj = rpart_obj %>% fill_tree_data(rf_tree,X,y)}
  
  return(rpart_obj)
  
}

#### rf_object to rpart object adapter work begins here
#'init_rpart
#'
#'This function fit a simple lm to the iris dataset so as to generate an rpart objec
#'which can then be used as a shell for the rest of the code
#'@return an rpart object fit on the iris dataset
#'@export
init_rpart <- function(){
  
  data(iris)
  model <- rpart(Species ~ ., data=iris)
  return(model)
}

#' mod_rpart_frame
#' 
#' this function transfers information from the rf_tree to the frame aspect of the
#' rpart object
#' @param rpart_obj the rpart shell to modify
#' @param rf_tree the random forest tree
#' @return modified rpart object
#' @export
mod_rpart_frame <- function(rpart_obj,rf_tree){
  
  #generate frame shell
  column_names <- c("var", "n", "wt","dev","yval","complexity","ncompete","nsurrogate")
  
  # Create an empty dataframe
  rpart_frame <- data.frame(matrix(0,ncol = length(column_names), 
                                   nrow = nrow(rf_tree)))
  
  # Set the column names
  colnames(rpart_frame) <- column_names
  
  # set names of the var equal to the names from rf_tree
  rpart_frame[,"var"] = rf_tree[,"split var"]
  # set leafs to expected leaf name
  rpart_frame[,"var"] = rpart_frame[,"var"] %>% 
    samthesis::replaceNAw0(replwith = "<leaf>")
  
  
  first_non_na <- rf_tree[,"prediction"]  %>% purrr::detect(~ !is.na(.))
  rf_tree[,"prediction"][is.na(rf_tree[,"prediction"])] <- first_non_na
  # assign the yval labels to associated dataframe levels 
  
  rpart_frame[,"yval"] = get_num_from_factor_levels(rf_tree[["prediction"]])
  
  # initalize the attached matrix
  rpart_frame_mat = init_rpart_frame_matrix(rf_tree)
  
  
  #attach matrix to frame
  
  rpart_frame[["yval2"]] = rpart_frame_mat
  
  # modify the rownames to match the binary tree row names expected by rpart
  rownames(rpart_frame) = make_rownames_binary_tree(rf_tree)
  
  rpart_obj$frame = rpart_frame
  return(rpart_obj)
}

#'init_rpart_frame_matrix
#'
#'initializes the matrix portion of the rpart$frame 
#'@param the rftree object to get the information from
#'@export
init_rpart_frame_matrix = function(rf_tree){
  
  predict_levels = get_num_from_factor_levels(rf_tree[["prediction"]])
  nlevels = length(unique(predict_levels))
  needed_cols = nlevels*2 + 2
  # initalize the attached matrix
  
  rpart_frame_mat = matrix(0,ncol = needed_cols,
                           byrow=F,
                           nrow = nrow(rf_tree))
  colnames(rpart_frame_mat)[needed_cols] = "nodeprob"
  rpart_frame_mat[,1] = predict_levels
  rpart_frame_mat[,2:(needed_cols/2)] = 0
  rpart_frame_mat[,(needed_cols/2 + 1):(needed_cols - 1)] = (1 / nlevels)
  
  return(rpart_frame_mat)
}



#'get_num_from_factor_levels
#'
#'Transforms preidctions in whatever form into the numeric version factor levels 
#'within the rpart_frame object
#'@param rf_tree the rf_tree object
#'@return a numeric vector corresponding to the prediction column from rf_tree
#'@export
get_num_from_factor_levels <- function(predicts){
  num_vec = predicts %>% 
    as.factor()      %>%            
    as.integer()   
  num_vec[is.na(num_vec)] <-  head(na.omit(num_vec), 1)
  return(num_vec)
}

#'make_rownames_binary_tree
#'
#'the rownames of rpart$frame need to be in the binary tree format. THis function
#'calculates those rownames based on the relationships list in the rf_tree object
#'@param rf_tree the rf_tree object
#'@return a vector of numbers of the new rownames
#'@export
make_rownames_binary_tree = function(rf_tree){
  
  #function for traversing the tree
  traverse_tree <- function(rf_tree,tree_node,new_id = 1){
    
    # get the old id
    old_id = rownames(tree_node) %>% 
      as.numeric()
    
    updated_df = data.frame(old = old_id,new = new_id)
    if(tree_node[["left daughter"]] != 0){
      # call traverse_tree on child nodes
      left_daught = traverse_tree(rf_tree,
                                  rf_tree[tree_node[["left daughter"]],],
                                  new_id = new_id*2)
      right_daught =traverse_tree(rf_tree,
                                  rf_tree[tree_node[["right daughter"]],],
                                  new_id = ((new_id*2) + 1)) 
      updated_df = updated_df %>% 
        rbind(left_daught) %>% 
        rbind(right_daught)
      
    }
    return(updated_df)
  }
  
  #add new id column
  rf_tree[["new_id"]] = 0
  
  id_df = traverse_tree(rf_tree,rf_tree[1,])
  id_df = id_df[order(id_df[["old"]]),]
  new_ids = id_df$new
  
  return(new_ids)}

#'mod_rpart_ylevels
#'
#'Supplies new levels to the rpart object based on the predictions from the rf_tree
#'notably adds a blank string "" to the list to supply to nonterminal nodes as a 
#'placeholder
#'@param rpart_obj the rpart object to be modified
#'@param rf_tree the rf_tree object to get the names from
#'@return modified rpart_object
#'@export
mod_rpart_ylevels = function(rpart_obj,rf_tree){
  
  # get unique levels
  outcome_lvls = rf_tree[["prediction"]] %>% 
    unique() %>% 
    as.character() %>%  
    na.omit() %>% 
    as.factor()
  
  attr(rpart_obj,"ylevels") = outcome_lvls
  
  return(rpart_obj)}


#'mod_rpart_splits
#'
#'modifies the split aspect of the rpart_object dataframe to contain information
#'from the rf_tree object 
#'@param rpart_obj the rpart object to be modified
#'@param rf_tree the rf_tree object to get the split information from
#'@return modified rpart_object
#'@export
mod_rpart_splits = function(rpart_obj,rf_tree){
  
  # get split information
  rf_tree_splits = rf_tree %>% 
    filter(!is.na(`split var`)) %>% 
    dplyr::select(all_of(c("split var","split point")))
  
  # initialize split information
  splits = matrix(c(0,-1,40,0,0),
                  nrow = nrow(rf_tree_splits),
                  ncol = 5,
                  byrow = T,
                  dimnames= list(rf_tree_splits[["split var"]],
                                 c("count","ncat","improve","index","adj")))
  splits[,"index"] = rf_tree_splits[["split point"]]
  
  rpart_obj[["splits"]] = splits
  
  return(rpart_obj)}

#' test_mod_rpart
#' 
#' Take a list of models run them through the main function, and store the results
#' 
#' @param model_list named list of models
#' @param save_folder where to save the plotted trees
#' @return plots matching one tree from each model
#' @export
test_mod_rpart <- function(model_list,save_folder){
  dir.create(save_folder)
  
  plts = lapply(names(model_list), function(model_name){
    print(model_name)
    
    model = model_list[[model_name]]
    
    rf_tree = getTree(model, k = 2, labelVar = T)
    
    rpart_obj = adapt_rf2rpart(rf_tree)
    
    jpeg(filename = paste0(save_folder,"/",model_name,".jpg"),
         width = 12, height = 12, units = "in",res = 720)
    
    rpart.plot(rpart_obj,type = 4,extra = 2)
    
    dev.off()
    
    return(rpart_obj)})
  
  names(plts) = names(model_list)
  
  return(plts)}

#' make_rpart_tests
#'
#' Generates models to test using test_rpart, save the results in a named list
#' 
#' @return named list of tests
#' @export
make_rpart_tests <- function(){
  
  # iris test
  
  data(iris)
  iris_model = randomForest(Species ~ ., data = iris)
  
  #mtcars test
  data(mtcars)
  mtcars_model = randomForest(mpg ~.,data = mtcars,maxnodes = 3)
  
  #pima indian test
  library("MASS")
  data(pima)
  pima$test[pima$test ==1] <- "diabetes"
  pima$test[pima$test ==0] <- "no diabetes"
  pima$test = as.factor(pima$test)
  
  pima_model <- randomForest(test ~ ., data = pima,maxnodes = 4)
  #make model list
  models = list(irs = iris_model,mtcrs = mtcars_model,pma_ind = pima_model)
  
  
  return(models)
  
}

#' fill_tree_data
#' 
#' fills the tree with information from which it was trained
#' @param rpart_obj the rpart object to modify
#' @param rf_tree   the rf_tree object representing the tree to fill
#' @param X the data used to train the model or data of an equivilant format
#' no true labels
#' @param y the true labels for the training data
#' @return an rpart object with the split information correctly filled out based on
#' the supplied information
#' @export
fill_tree_data <- function(rpart_obj,rf_tree, X, y){
  
  # create a matrix for rf_tree that correspond to what needs to be filled in for the 
  #matrix part of the rp
  # rpart matrix
  rpart_mat = init_rpart_frame_matrix(rf_tree)
  
  # transform the predicted vector into numbers correspond to their factor levels
  y = get_num_from_factor_levels(y)
  
  for (i in 1:150){
    
    iframe = 1
    
    # get the test row
    irow = X[i,]
    
    # see the class
    iclass = y[i]
    tst = 0
    
    
    
    while(iframe != 0){
      
      # identify splitting variable and splitting value, and next nodes
      split_var      = rf_tree[iframe,"split var"] %>% as.character()
      split_point    = rf_tree[iframe,"split point"]
      right_daughter = rf_tree[iframe,"right daughter"]
      left_daughter  = rf_tree[iframe,"left daughter"]
      
      # add to n
      rpart_mat[iframe,iclass + 1] = rpart_mat[iframe,iclass + 1] + 1
      
      if(!is.na(rf_tree[iframe,"split var"])){
        
        # identify next node
        iframe = if_else(irow[[split_var]] >= split_point,
                         right_daughter,
                         left_daughter)
      }
      else{print("ending")
        iframe = 0}
    }
    next
  }
  
  # modify the remaining columns of the matrix
  nlevels = length(unique(y))
  needed_cols = nlevels*2 + 2
  
  rpart_mat_nums = rpart_mat[,2:(needed_cols/2)]
  rpart_mat_nums_sums = rowSums(rpart_mat_nums)
  
  rpart_mat_nums_perc = rpart_mat_nums / rpart_mat_nums_sums
  rpart_mat[,(needed_cols/2 + 1):(needed_cols - 1)] = rpart_mat_nums_perc
  
  node_max = max(rpart_mat_nums_sums)
  rpart_mat[,needed_cols] = rpart_mat_nums_sums / node_max
  
  # modify rpartobject
  rpart_obj$frame$yval2 = rpart_mat
  return(rpart_mat) }


#'test_tree
#'
#'This function takes a decision tree which is described in a dataframe as
#'randomForest package outputs, takes a dataframe of a new test set and
#'returns predictions for each of those test sets and what node they land at
#'
#'@param tree A data frame describing the decision tree. This data frame should be in the same format as the output from the rpart() function.
#'@param test_df A data frame containing the test set.
#'@param test_col The name of the column in the test set to be used as the dependent variable.
#'@export
#'
#'@return A list with three elements:
#'  \describe{
#'    \item{tree_info}{A data frame describing the decision tree, including information on how the test data is classified.}
#'    \item{conf_mat}{A confusion matrix for the test data.}
#'    \item{accuracy}{The accuracy of the decision tree for the test data.}
#'  }
#'
#'
test_tree = function(tree,test_df,test_col){
  
  # Replace logical values with their level strings in the test data
  test_df[,test_col] = as.character(test_df[,test_col])
  tree$`split var` = as.character(tree$`split var`)
  
  # Add columns to the tree for the number of observations of each class for each node
  tree = tree %>% add_zero_columns(column_name= "prediction")
  tree$bin = 0
  # loop for each obs in the dataset
  for(i in 1:nrow(test_df)){
    obs = test_df[i,]
    node_ind = 1
    
    while(node_ind != "STOP"){
      
      # get the node info
      curr_node = tree[node_ind,]
      
      # get the variable at the node
      curr_var = curr_node[["split var"]]
      # test if this is a terminal node
      if(is.na(curr_var)){
        #get the true value
        true_val = obs[[test_col]]
        # update the num bin variable
        tree[node_ind,"bin"]    = tree[node_ind,"bin"]    + 1
        tree[node_ind,true_val] = tree[node_ind,true_val] + 1
        node_ind = "STOP"
      } else {
        # find if the test_obs is > this value
        test_res = obs[[curr_var]] >= curr_node[["split point"]]
        
        # Find the next node
        if(test_res){
          node_ind = curr_node[["right daughter"]]
        } else {
          node_ind = curr_node[["left daughter"]]}
      }
    }
  }
  
  # All observations have now been processed, now we are interested in making tree statistics
  # Caclualte the purity of each terminal node in the decision tree
  tree$purity = apply(tree,MARGIN = 1,FUN = function(x){
    node_class = x[["prediction"]]
    
    if(!is.na(node_class)){purity = as.numeric(x[[node_class]]) / as.numeric(x[["bin"]])
    } else { purity = 0}
  })
  
  # Make a confusion matrix
  unique_values <- unique(tree[["prediction"]]) %>%
    .[!is.na(.)]
  conf_mat_stuff = apply(tree,MARGIN = 1,FUN = function(x){
    print(x)
    node_class = x[["prediction"]]
    node_size  = x[["bin"]]
    predictions = rep(node_class,node_size)
    true_values = lapply(unique_values,FUN = function(unique_value){
      trues = rep(unique_value,x[[unique_value]])
      return(trues)})
    true_values = unlist(true_values)
    
    # amke a dataframe
    conf_mat = data.frame(predicts = predictions, true_val = true_values)
    
    return(conf_mat)})
  
  conf_mat_stuff = do.call("rbind",conf_mat_stuff)
  acc =sum(conf_mat_stuff$predicts == conf_mat_stuff$true_val) / nrow(conf_mat_stuff)
  sens = sum(conf_mat_stuff$predicts == conf_mat_stuff$true_val) / sum
  return(list(tree_info = tree,conf_mat = conf_mat_stuff, accuracy = acc))
}
