set.seed(12345)

test_data <-
  data.frame(
    .id = 1:10,
    enrollment = ceiling((1:10)/2),
    x_1 = rep(0:1, length.out = 10),
    tx = sample(x = 0:1, size = 10, replace = TRUE),
    tte_1 = ceiling((1:10)/2) + 1,
    tte_2 = 2*(1:10),
    event_1 = 1:10 %% 2,
    event_2 = abs((1:10 %% 2) - 1),
    y_1 = 1:10,
    y_2 = 1:10 + 5,
    .t_1 = 1:10,
    .t_2 = 1:10 + 5
  )

usethis::use_data(test_data, overwrite = TRUE)
