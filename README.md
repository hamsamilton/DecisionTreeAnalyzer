# Tree Analyzer

Decision Trees are a powerful machine learning method owing to their simplicity and interpretability. However, tools to analyze them in R are limited. This package addresses this by defining a decision tree interface that can be built in R or imported in a .csv, as well as tools to evaluate those decision trees. In addition, it provides tools to adapt those decision trees to be used with the decision tree plotting software in the RPart package. While the decision tree interface the RPart package defines is powerful, it is only compatible with the RPart objects generated by fitting the RPart model, these objects are very complex and cannot be easily modified by hand, nor interface with trees created by other software.

The main function is `adapt_input_tree_df2rpart()`. Call this function on the dataframe defined in the correct format and it will transform it into an rpart object compatible with `rpart.plot`.

By default, the tree is populated with fake sample data. However, by setting `add_data = T` and supplying an X and y input, you can provide training data to observe how your data traverses the tree.

