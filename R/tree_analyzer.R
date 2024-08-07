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

#' @export
dplyr::`%>%`

#' adapt_manual_tree
#' 
#' Adapt trees defined from the user interface to the format interfaced with
#' the rest of the package
#' 
#' @param InputTreeDF The input tree from the user interface, the interface is defined
#' as such. columns should be c('left daughter','right daughter','split var','split point','prediction'.
#' For non terminal nodes, the left and right daughter columns should give the numbers of each node
#' for terminal nodes, both left and right daughter values should be 0. For nonterminal nods
#' Split var should include the name of the variable used for splitting at that node. Leave blank for terminal nodes.
#' the split point should give the point at which to split the node. If it is less than the value
#'it will flow to left daughtr, if it is greater it will go to right daughter. prediction needs only
#'  to be filled out for terminal nodes and should indicate the desired prediction.
#' @return The input tree df compatible with the rest of the package
#' @export
adapt_manual_tree <- function(InputTreeDF){
  
  InputTreeDF['split point'][is.na(InputTreeDF['split point'])] = 0
  
  #I don't think this package uses status for anything so 1 is just a placeholder val
  InputTreeDF['status'] = 1 
  
  return(InputTreeDF)}

#' adapt_input_tree2rpart
#'
#' adapter function designed to take information from the input_tree object to the
#' rpart style object so that the plotting capabilities of the rpart object can be
#' leveraged.
#'
#'@param input_tree_df  the random forest tree object
#'@param X        if add_data = T, what is the predictor matrix
#'@param y        if add_data = T  what are the true values?
#'@param model_type what kind of prediction you are making 'categorical' or 'regression'
#'@export
adapt_input_tree2rpart <- function(input_tree_df,X,y,model_type){

  # Init the rpart object
  rpart_obj = init_rpart(model_type) %>%    # Initialize the objects
    mod_rpart_frame(input_tree_df)   %>%    # Modify the frame aspect of the rpart object
    mod_rpart_ylevels(input_tree_df) %>%     # Change the ylevels
    mod_rpart_splits(input_tree_df,X) %>%   # modify the splits part of the object
    fill_tree_data(input_tree_df,
                   X,
                   y,
                   model_type = model_type)

  # Modify the model to the regression outcome part
  if(model_type == 'regression'){
    rpart_obj$frame$yval2 = NULL}
  
  # Add information on the factor levels for categorical predictors to the object attributes
  
  # Initialize an empty list to store levels of factor variables
  xlevels <- list()
  
  # Loop through the variables used in the model
  model_vars <- colnames(X)
  for (var in model_vars) {
    print(var)
    # Check if the variable is a factor
    if (is.factor(X[[var]])) {
      # Store the levels of the factor variable
      xlevels[[var]] <- levels(X[[var]])
    }
  }
  
  # Set the xlevels attribute to the model
  attr(rpart_obj, "xlevels") <- xlevels
  

  return(rpart_obj)
}

#### rf_object to rpart object adapter work begins here
#'init_rpart
#'
#'This function fit a simple rpart object to the iris dataset to generate an rpart object
#'which can be modified
#'@param model_type Whether to return a model for 'regression' or a 'categorical' prediction
#'@return an rpart object fit on the iris dataset
#'@export
init_rpart <- function(model_type){
  
  data(iris)
  
  if(model_type == 'categorical'){
  model <- rpart(Species ~ ., 
                 data=iris)}
  
  if(model_type == 'regression'){
    iris$Species = NULL
    
    model <- rpart(Sepal.Length ~ ., 
                   data=iris)}
    
  return(model)
}

#' mod_rpart_frame
#'
#' Transfer information from the input format to the frame aspect of the rpart object
#' @param rpart_obj the rpart shell to modify
#' @param input_tree_df the random forest tree
#' @return modified rpart object
#' @export
mod_rpart_frame <- function(rpart_obj,input_tree_df){

  #generate frame shell
  column_names <- c("var", "n", "wt","dev","yval","complexity","ncompete","nsurrogate")

  # Create an empty dataframe
  rpart_frame <- data.frame(matrix(0,
                                   ncol = length(column_names),
                                   nrow = nrow(input_tree_df)))

  # Set the column names
  colnames(rpart_frame) = column_names

  # set names of the var equal to the names from input_tree_df
  rpart_frame[,"var"] = input_tree_df[,"split var"]
  rpart_frame[,"var"] = as.character(rpart_frame[,"var"])

  # set leaves to expected leaf name
  rpart_frame[is.na(input_tree_df[,"split var"]),
              "var"] = "<leaf>"


  first_non_na = input_tree_df[,"prediction"]  %>% 
    purrr::detect(~ !is.na(.))
  
  input_tree_df[,"prediction"][is.na(input_tree_df[,"prediction"])] <- first_non_na
  
  # assign the yval labels to associated dataframe levels
  rpart_frame[,"yval"] = get_num_from_factor_levels(input_tree_df[["prediction"]])

  # initalize the attached matrix, yval2 that contains inforamtion for categorical models
  rpart_frame_mat = init_rpart_frame_matrix(input_tree_df)

  #attach matrix to frame
  rpart_frame[["yval2"]] = rpart_frame_mat

  # modify the rownames to match the binary tree row names expected by rpart
  rownames(rpart_frame) = make_rownames_binary_tree(input_tree_df)

  # add in the new object
  rpart_obj$frame = rpart_frame
  
  return(rpart_obj)
}

#'init_rpart_frame_matrix
#'
#'initializes the matrix portion of the rpart$frame with the format for the
#' correct number of classes
#'
#'@param input_tree_df the input df defining the decision tree to get the information from
#'@export
init_rpart_frame_matrix = function(input_tree_df){

  # calc how many classes and how many columns are needed to store the information
  predict_levels = get_num_from_factor_levels(input_tree_df[["prediction"]])
  nlevels = length(unique(predict_levels))
  needed_cols = nlevels*2 + 2
  
  # initalize the attached matrix
  rpart_frame_mat = matrix(0,
                           ncol  = needed_cols,
                           byrow = F,
                           nrow  = nrow(input_tree_df))
  
  colnames(rpart_frame_mat)[needed_cols] = "nodeprob"
  
  # Initialize the labels for each node
  rpart_frame_mat[,1] = predict_levels
  
  rpart_frame_mat[,2:(needed_cols/2)] = 0
  rpart_frame_mat[,(needed_cols/2 + 1):(needed_cols - 1)] = (1 / nlevels)

  return(rpart_frame_mat)
}

#'get_num_from_factor_levels
#'
#'Transforms predictions in whatever form into the numeric version factor levels
#'within the rpart_frame object
#'@param predictions vector of predictions
#'@return a numeric vector corresponding to the prediction column from input_tree_df
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
#'The rownames of rpart$frame need to be in the binary tree format. This function
#'calculates those rownames based on the relationships defined in the input_df object
#'@param input_tree_df the input_tree_df object
#'@return a vector of numbers of the new rownames
#'@export
make_rownames_binary_tree = function(input_tree_df){

  # Recursive function for traversing the tree
  traverse_tree <- function(input_tree_df,tree_node,new_id = 1){

    # get the old id
    old_id = rownames(tree_node) %>%
      as.numeric()

    updated_df = data.frame(old = old_id,
                            new = new_id)
    
    if(tree_node[["left daughter"]] != 0){
      # call traverse_tree on child nodes
      
      left_daught = traverse_tree(input_tree_df,
                                  input_tree_df[tree_node[["left daughter"]],],
                                  new_id = new_id*2)
      
      right_daught = traverse_tree(input_tree_df,
                                   input_tree_df[tree_node[["right daughter"]],],
                                        new_id = ((new_id*2) + 1))
      updated_df = updated_df %>%
        rbind(left_daught) %>%
        rbind(right_daught)

    }
    return(updated_df)
  }

  #add new id column
  input_tree_df[["new_id"]] = 0

  id_df = traverse_tree(input_tree_df,
                        input_tree_df[1,])
  id_df = id_df[order(id_df[["old"]]),]
  new_ids = id_df$new

  return(new_ids)}

#'mod_rpart_ylevels
#'
#'Supplies new levels to the rpart object based on the predictions from the input_tree_df
#'notably adds a blank string "" to the list to supply to nonterminal nodes as a
#'placeholder
#'@param rpart_obj the rpart object to be modified
#'@param input_tree_df the input tree df to get the names from
#'@return modified rpart_object
#'@export
mod_rpart_ylevels = function(rpart_obj,input_tree_df){

  # get unique levels
  outcome_lvls = input_tree_df[["prediction"]] %>%
    unique() %>%
    as.character() %>%
    na.omit() %>%
    as.factor()

  attr(rpart_obj,"ylevels") = outcome_lvls

  return(rpart_obj)}

#'mod_rpart_splits
#'
#'modifies the split aspect of the rpart_object dataframe to contain information
#'from the input_tree_df object
#'
#'@param input_tree_df the input tree df to get the split information from
#'@param X the dataframe of predictors, needed to determine which variables are factors and numerics 
#'@return modified rpart_object
#'@export
mod_rpart_splits = function(rpart_obj,input_tree_df,X){
  
  # Used to create the csplit object that contains information on splitting categorical variables
  make_csplit <- function(rpart_obj,input_tree_df){
    
    # ID splits from the input_tree_df 
    cat_splits = input_tree_df %>% 
      filter(status == -3)
    
    # fetch split object to get into, subset only to cat variables
    splits = rpart_obj$splits 
    splits = splits[splits[,2] > 1,]
    
    
    # ID dimensions
    nrow = sum(splits[,2] > 1) # how many cat splits are there
    ncol = max(splits[,2]) # what is the maximum number of factors
    
    # initialize matrix
    csplit = matrix(nrow = nrow,
                    ncol = ncol)
    
    for(isplit in 1:nrow(cat_splits)){
      split_var = cat_splits[isplit,'split var'][[1]] %>% as.character()
      split_point = cat_splits[isplit,'split point'][[1]]
      ncat = splits[match(split_var,rownames(splits)),
                    'ncat']
      
      csplit[isplit,] = 2 # 2s are unused or irrelevant factor levels
      csplit[isplit,1:ncat] = 3 # for factor levels that fail the test
      csplit[isplit,split_point] = 1 # the correct factor level
    }
    
    print(csplit)
  
    return(csplit) }

    
  
  # categorical predictors require the # of factor levels in this field while the
  # continuous values don't
  Make_ncat_info <- function(input_df) {
    # Initialize vectors to store the column names and their corresponding values
    col_names <- colnames(input_df)
    col_values <- sapply(input_df, function(col) {
      if (is.numeric(col)) {
        return(-1)
      } else if (is.factor(col)) {
        return(length(levels(col)))
      } else {
        return(NA)
      }
    })
    
    # Create a new dataframe with the results
    summary_df <- data.frame(
      ColumnName = col_names,
      Value = col_values
    )
    
    return(summary_df)
  }
  ncat_info = Make_ncat_info(X)
  # get split information
  input_tree_df_splits = input_tree_df %>%
    filter(!is.na(`split var`)) %>%
    dplyr::select(all_of(c("split var",
                           "split point")))

  # initialize split information
  splits = matrix(c(0,-1,40,0,0),
                  nrow     = nrow(input_tree_df_splits),
                  ncol     = 5,
                  byrow    = T,
                  dimnames = list(input_tree_df_splits[["split var"]],
                                  c("count",
                                    "ncat",
                                    "improve",
                                    "index",
                                    "adj")))
  
  # Add the split points
  splits[,"index"] = input_tree_df_splits[["split point"]]
  
  splits[,'ncat'] = ncat_info[['Value']][match(rownames(splits), 
                                               ncat_info[['ColumnName']])]
  
  # For categorical variables replace the index with the index that will refer to the
  # row in the csplit matrix which will contain the splitting information
  splits[(splits[,'ncat'] > 1),
         'index'] = seq(1,sum(splits[,'ncat'] > 1))
  
  rpart_obj[["splits"]] = splits
  
  rpart_obj[['csplit']] = make_csplit(rpart_obj,input_tree_df)



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

    model = model_list[[model_name]]

    input_tree_df = getTree(model, 
                      k        = 2, 
                      labelVar = T)

    rpart_obj = adapt_input_tree2rpart(input_tree_df)

    jpeg(filename = paste0(save_folder,
                           "/",
                           model_name,
                           ".jpg"),
         width    = 12, 
         height   = 12,
         units    = "in",
         res      = 720)

    rpart.plot(rpart_obj,
               type  = 4,
               extra = 2)

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
  iris_model = randomForest(Species ~ ., 
                            data = iris)

  #mtcars test
  data(mtcars)
  mtcars_model = randomForest(mpg ~ .,
                              data     = mtcars,
                              maxnodes = 3)

  #pima indian test
  library("MASS")
  data(pima)
  pima$test[pima$test ==1] <- "diabetes"
  pima$test[pima$test ==0] <- "no diabetes"
  pima$test = as.factor(pima$test)

  pima_model <- randomForest(test ~ .,
                             data     = pima,
                             maxnodes = 4)
  #make model list
  models = list(irs     = iris_model,
                mtcrs   = mtcars_model,
                pma_ind = pima_model)

  return(models)
}

#' fill_tree_data
#'
#' fills the tree with information from which it was trained
#' @param rpart_obj the rpart object to modify
#' @param input_tree_df   the input tree df representing the tree to fill
#' @param X the data used to train the model or data of an equivilant format
#' no true labels
#' @param y the true labels for the training data
#' @param model_type what kind of model 'regression' or 'categorical'
#' @return an rpart object with the split information correctly filled out based on
#' the supplied information
#' @export
fill_tree_data <- function(rpart_obj,input_tree_df, X, y, model_type){
  
  # create a matrix for input tree df that correspond to what needs to be filled in for the
  # matrix part of the rp
  # rpart matrix
  rpart_mat = init_rpart_frame_matrix(input_tree_df)

  # transform the predicted vector into numbers correspond to their factor levels
  response = get_num_from_factor_levels(y)
  
  if(model_type == 'regression'){
    
    response = rep(1,
                   length(response))}
  
  # initialize the list of lists that will hold the values of all the samples that
  # pass through each node
  value_list = replicate(nrow(rpart_mat),
                         c(),
                         simplify = F)
  
  for (i in 1:nrow(X)){

    iframe = 1

    # get the test row
    irow = X[i,]

    # see the class
    iclass = response[i]
    
    # grab the value of y (only used for regression models)
    value = y[i]

    while(iframe != 0){

      # identify splitting variable and splitting value, and next nodes
      split_var      = input_tree_df[iframe,"split var"] %>% as.character()
      var_is_numeric = is.numeric(X[[split_var]])
      split_point    = input_tree_df[iframe,"split point"]
      right_daughter = input_tree_df[iframe,"right daughter"]
      left_daughter  = input_tree_df[iframe,"left daughter"]

      # add to n
      rpart_mat[iframe,iclass + 1] = rpart_mat[iframe,iclass + 1] + 1
      
      # add to the recorded value list
      value_list[[iframe]] <- append(value_list[[iframe]] ,value)

      if(!is.na(input_tree_df[iframe,"split var"])){

        # identify next node
        if(var_is_numeric){
          pass = irow[[split_var]] >= split_point}
        else{
          pass =  as.numeric(irow[[split_var]]) == split_point}
        
        iframe = if_else(pass,
                         right_daughter,
                         left_daughter)}

      else{iframe = 0}}
    next}
  
  # modify the remaining columns of the matrix
  nlevels = length(unique(response))
  needed_cols = nlevels*2 + 2

  rpart_mat_nums = rpart_mat[,2:(needed_cols/2)] %>% 
    as.matrix()
  
  rpart_mat_nums_sums = rowSums(rpart_mat_nums)
    

  rpart_mat_nums_perc = rpart_mat_nums / rpart_mat_nums_sums
  rpart_mat[,(needed_cols/2 + 1):(needed_cols - 1)] = rpart_mat_nums_perc

  rpart_mat[,needed_cols] = rpart_mat_nums_sums / max(rpart_mat_nums_sums)

  # modify rpartobject
  rpart_obj$frame$yval2 = rpart_mat
  
  # change intermediate node display values to whatever is the dominant class
  int_nodes =  which(rpart_obj$frame$var != "<leaf>")
  
  if(model_type == 'categorical'){
    rpart_obj$frame$yval2[int_nodes,1] = apply(rpart_mat_nums[int_nodes,],
                                               MARGIN = 1,
                                               FUN    = which.max)}
    
  # modify n and wt which are used to calculate the node probabilities in teh tree
  # both of these just match the total number of observations in the tree
  print(rpart_mat)
  rpart_obj$frame$n = rowSums(as.matrix(rpart_mat[,2:(2+(nlevels-1))]))
  rpart_obj$frame$wt = rowSums(as.matrix(rpart_mat[,2:(2+(nlevels-1))]))
  
  # Modify the value part of the frame so it matches the average value at each node
  # which is what is displayed in regression models
  if(model_type == 'regression'){
    
    rpart_obj$frame$yval = sapply(value_list,
                                  mean)}
  

  return(rpart_obj) }

#'tree_performance
#'
#'This function takes a decision tree which is described in a dataframe as
#'randomForest package outputs, takes a dataframe of a new test set and
#'returns predictions for each of those test sets and what node they land at
#'
#'@param tree A data frame describing the decision tree.
#' This data frame should be in the same format as the trees used within the RF pacakge
#'@param test_df A data frame containing the test set.
#'@param test_col The name of the column in the test set to be used as the dependent variable.
#'@export
#'
#'@return A list with three elements:
#'  \describe{
#'    \item{tree_info}{A data frame describing the decision tree, including information on how the test data is classified.
#'    In the format of trees used in teh random forest}
#'    \item{conf_mat}{A confusion matrix for the test data.}
#'    \item{accuracy}{The accuracy of the decision tree for the test data.}
#'  }
#'
#'
tree_performance = function(tree,test_df,test_col){

  # Replace logical values with their level strings in the test data
  test_df[,test_col] = as.character(test_df[,test_col])
  tree$`split var` = as.character(tree$`split var`)

  # Add columns to the tree for the number of observations of each class for each node
  tree = tree %>% add_zero_columns(column_name= "prediction")
  tree$bin = 0
  tree$binmembers = ""
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
        tree[node_ind,"binmembers"] = paste(tree[node_ind,"binmembers"],rownames(obs))
        tree[node_ind,true_val] = tree[node_ind,true_val] + 1
        node_ind = "STOP"
      } else {
        # find if the test_obs is > this value
        test_res = obs[[curr_var]] >= curr_node[["split point"]]
        tree[node_ind,"binmembers"] = paste(tree[node_ind,"binmembers"],rownames(obs))

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
  conf_mat_stuff = apply(conf_mat_stuff,MARGIN=2,FUN = function(x){
    as.numeric(as.factor(x)) -1 })

  perf_metrics = eval_metrics(conf_mat_stuff[,1],conf_mat_stuff[,2])

  return(c(list(tree_info = tree,conf_mat = conf_mat_stuff),perf_metrics))
}


#' Evaluate performance metrics for classification
#'
#' This function calculates various performance metrics for binary classification, including
#' sensitivity, specificity, precision, negative predictive value (npv), false positive rate (fpr),
#' false discovery rate (fdr), false negative rate (fnr), F1 score, and accuracy.
#'
#' @param predictions A numeric or logical vector of predicted classes.
#' @param actuals A numeric or logical vector of actual classes.
#'
#' @return A list containing the following elements:
#'   * sensitivity: The proportion of actual positive cases that were correctly identified.
#'   * specificity: The proportion of actual negative cases that were correctly identified.
#'   * precision: The proportion of predicted positive cases that were correctly identified.
#'   * npv: The proportion of predicted negative cases that were correctly identified.
#'   * fpr: The proportion of actual negative cases that were incorrectly identified as positive.
#'   * fdr: The proportion of predicted positive cases that were incorrect.
#'   * fnr: The proportion of actual positive cases that were incorrectly identified as negative.
#'   * f1: The harmonic mean of precision and sensitivity.
#'   * accuracy: The proportion of total cases that were correctly identified.
#'
#' @examples
#' \dontrun{
#' results = eval_metrics(predicted_classes, actual_classes)
#' print(results)
#' }
#' @export
eval_metrics <- function(predictions, actuals) {

  # Assuming '1' is the positive class
  actual_positives = actuals == 1
  predicted_positives = predictions == 1

  actual_negatives = actuals == 0
  predicted_negatives = predictions == 0

  true_positives = sum(actual_positives & predicted_positives)
  true_negatives = sum(actual_negatives & predicted_negatives)

  false_positives = sum(actual_negatives & predicted_positives)
  false_negatives = sum(actual_positives & predicted_negatives)

  all_actual_positives = sum(actual_positives)
  all_actual_negatives = sum(actual_negatives)

  # Sensitivity, also known as True Positive Rate or Recall
  sens = true_positives / all_actual_positives

  # Specificity, also known as True Negative Rate
  spec = true_negatives / all_actual_negatives

  # Precision, also known as Positive Predictive Value
  prec = true_positives / (true_positives + false_positives)

  # Negative Predictive Value
  npv = true_negatives / (true_negatives + false_negatives)

  # Fall out or False Positive Rate
  fpr = false_positives / all_actual_negatives

  # False Discovery Rate
  fdr = false_positives / (true_positives + false_positives)

  # False Negative Rate
  fnr = false_negatives / all_actual_positives

  # F1 Score
  f1 = 2 * ((prec * sens) / (prec + sens))

  # Accuracy
  acc = (true_positives + true_negatives) / length(actuals)

  # Return all metrics in a list
  return(list(sensitivity = sens, specificity = spec, precision = prec,
              npv = npv, fpr = fpr, fdr = fdr, fnr = fnr, f1 = f1, accuracy = acc))
}

#' add_zero_columns
#' Add Columns with 0s for Each Unique Value in a Specified Column
#'
#' This function adds new columns to a dataframe for each unique value in a specified column, initializing these new columns with 0s.
#'
#' @param df A data.frame to which the new columns will be added.
#' @param column_name A character string representing the name of the column in 'df' containing the unique values for creating new columns.
#'
#' @return A data.frame with new columns initialized with 0s for each unique value in the specified column.
#' @export
#'
#' @examples
#' # Create an example dataframe
#' data <- data.frame(
#'   A = c("apple", "banana", "apple", "banana", "orange"),
#'   B = c(1, 2, 3, 4, 5)
#' )
#'
#' # Use the add_zero_columns function with dplyr
#' library(dplyr)
#' result <- data %>%
#'   mutate(data, add_zero_columns(., "A"))
#' print(result)
add_zero_columns <- function(df, column_name) {
  unique_values <- unique(df[[column_name]]) %>%
    .[!is.na(.)]

  for (value in unique_values) {
    df[[value]] <- 0
  }

  return(df)
}
#'mk_confusion_matrix
#'
#'Produces a heatmap that functions as a confusion matrix using output of
#'tree_performance.
#'
#'@param confusion_data a dataframe with two columns, the first being predictions and
#'the second being the true values. a compatible object is output by tree_performance in
#'the $conf_mat attribute
#'@param saveloc where should the confusion matrix be stored
#'@export
mk_confusion_mat <- function(tree_pref_obj, save_loc = "~/confmatsz.jpeg") {

  confusion_data <- table(tree_pref_obj$conf_mat[,1],tree_pref_obj$conf_mat[,2])
  classnames <- unique(tree_pref_obj$tree_info$prediction)[-1]

  rownames(confusion_data) <-  classnames
  colnames(confusion_data) <-  classnames

  pheatmap::pheatmap(confusion_data,
                     color = QckRBrwrPllt("OrRd",100)[1:50],
                     cluster_rows = F,
                     cluster_cols = F,
                     legend       = F,
                     display_numbers=T,
                     number_format="%.0f",
                     width  = 4,
                     height = 4,
                     filename= save_loc,
                     fontsize_number=30,
                     fontsize = 15,
              margin = 40)

}
#'QckRBrwrPllt
#'
#'This is a quick function that combines two RColorBrewer functions into one that
#'makes it more intuitive to use and setup
#'@param name the Rbrewer pallette name
#'@param n how many colors you want
#'@return a color vector
#'@export
QckRBrwrPllt <- function(name,n) {
  plt <- colorRampPalette(RColorBrewer::brewer.pal(8,name))(n)
  return(plt)
}


