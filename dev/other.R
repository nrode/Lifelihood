dataLifelihood <- data.frame(geno_l = toto - 1, t1 = t1 - 0.5, t2 = t1 + 0.5) |>
  as_tibble() |>
  dplyr::mutate(
    sex_start = dplyr::if_else(t1 <= 1, 0.0001, t1 - 1),
    sex_end = dplyr::if_else(t2 <= 1, 0.0001, t2 - 1),
    sex = 0
  ) |>
  dplyr::mutate(
    mat_start = dplyr::if_else(t1 <= 1, 0.0001, t1 - 1),
    mat_end = dplyr::if_else(t2 <= 1, 0.0001, t2 - 1)
  ) |>
  dplyr::mutate(clutch_start1 = NA, clutch_end1 = NA, clutch_size1 = NA) |>
  dplyr::mutate(
    t1 = dplyr::if_else(t1 <= 0, 0.0001, t1),
    t2 = dplyr::if_else(t2 <= 0, 0.0001, t2)
  ) |>
  dplyr::rename(death_start = t1, death_end = t2) |>
  dplyr::relocate(death_start, .after = last_col()) |>
  dplyr::relocate(death_end, .after = last_col()) |>
  mutate(geno_l = as.factor(geno_l), toto = as.factor(geno_l))

set.seed(123)
n <- 1000
longevity <- 10
slope <- 1
toto <- rep(1:2, each = n / 2)
t1 <- rexp(n = n, rate = 1 / (longevity * toto))


df <- datapierrick |>
  as_tibble() |>
  mutate(
    geno = as.factor(geno),
    par = as.factor(par)
  )
