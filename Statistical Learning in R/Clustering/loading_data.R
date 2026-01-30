# The following code was copied and pasted from the below link :
# https://gist.github.com/daviddalpiaz/ae62ae5ccd0bada4b9acd6dbc9008706
# It is designed to help read the mnist files

# helper function for visualization
show_digit = function(arr784, col = gray(12:1 / 12), ...) {
  image(matrix(as.matrix(arr784[-785]), nrow = 28)[, 28:1], col = col, ...)
}

# load image files
load_image_file = function(filename) {
  ret = list()
  f = file(filename, 'rb')
  readBin(f, 'integer', n = 1, size = 4, endian = 'big')
  n    = readBin(f, 'integer', n = 1, size = 4, endian = 'big')
  nrow = readBin(f, 'integer', n = 1, size = 4, endian = 'big')
  ncol = readBin(f, 'integer', n = 1, size = 4, endian = 'big')
  x = readBin(f, 'integer', n = n * nrow * ncol, size = 1, signed = FALSE)
  close(f)
  data.frame(matrix(x, ncol = nrow * ncol, byrow = TRUE))
}

# load label files
load_label_file = function(filename) {
  f = file(filename, 'rb')
  readBin(f, 'integer', n = 1, size = 4, endian = 'big')
  n = readBin(f, 'integer', n = 1, size = 4, endian = 'big')
  y = readBin(f, 'integer', n = n, size = 1, signed = FALSE)
  close(f)
  y
}

# load images
train = load_image_file("..\\Task 3\\train-images.idx3-ubyte")
test  = load_image_file("..\\Task 3\\t10k-images.idx3-ubyte")

# load labels
train$y = as.factor(load_label_file("..\\Task 3\\train-labels.idx1-ubyte"))
test$y  = as.factor(load_label_file("..\\Task 3\\t10k-labels.idx1-ubyte"))
