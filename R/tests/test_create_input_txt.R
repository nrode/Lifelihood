source(file.path('R', 'create_input_txt.R'))

# test data
test_data <- data.frame(
  sex = c(13, 13, 15, 14, 19, 12),
  mat = c(12, 12, 14, 13, 18, 11),
  pon_1 = c(20, 19, 18, 20, 21, 18),
  pon_2 = c(23, 23, 22, 24, 22, 23),
  pon_3 = c(26, 27, 25, 27, 22, 26),
  mor = c(102, 95, 78, 66, 135, 74)
)

# write test data to a text file
create_txt_file(test_data, "test_input.txt")