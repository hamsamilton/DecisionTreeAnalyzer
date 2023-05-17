Tree Analyzer

RPart provides powerful software for visualizing decision trees. Unfortunately, it is only compatible with the very complex and difficult to understand and create RPart object. This is a problem for those of us who would like to visualize decision trees created either by hand or with other software. 

This package is functionally an adapter for RPart which allows users to supply a decision tree in the user friendly format used by the randomForest package. Thus far, it is unable to produce some of the "extra" info that can be produced by rpart.plot, but it generally works.

This software is currently in beta and needs additional bug testing.

In the future, there may be additional functions added for decision tree analysis. There are some additional functions provided to further analyze decision trees, although these are currently a work in progress.

The main function is adapt_rf2rpart. Call this on your randomForest style tree object and it will transform it into an rpart object compatible with rpart.plot.

By default fake information on samples populates the tree, by setting add_data = T and an X and y input you can supply training data to see howyour data falls down the tree.

