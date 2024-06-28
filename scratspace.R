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
  adapt_input_tree2rpart(add_data = T,X = iris_wrong[,-5],y = iris_wrong[,5])
