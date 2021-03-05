install.packages("tensorflow")
library(tensorflow)
install_tensorflow()

# You can confirm that the installation succeeded with:
library(tensorflow)
tf$constant("Hellow Tensorflow")
## tf.Tensor(b'Hellow Tensorflow', shape=(), dtype=string)

reticulate::conda_version()
sessionInfo()
