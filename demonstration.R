##################################
# Tokyo.R#66
# そろそろ手を出すpurrr
# Twitter: @u_ribo
# GitHub: @uribo
##################################
# 文字数を数える処理 ---------------------------------------------------------------
x <- c("kazutan", "hoxom", "uribo")
nchar(x[1])
nchar(x[2])
nchar(x[3])

# 関数を引数にとる高階関数
sapply(x, nchar)

library(purrr)
map(x, nchar)
Map(nchar, x) # 第一引数に対象のオブジェクトを取らない
# パイプ演算子との親和性が低い
x %>% map(nchar)

# map_* -------------------------------------------------------------------
# map色々(1) 返り値を任意の型のベクトルに
x %>% map_int(nchar)
x %>% map_int(nchar) %>% sum()
# 任意の文字列を論理型に変換することはできない
x %>% map_chr(nchar)
# x %>% map_lgl(nchar)
# Error: Can't coerce element 1 from a integer to a logical

# map色々(2) 位置や条件による適用
x %>% map_at(.at = 2, nchar)
x %>% map_if(.p = . == "kazutan", nchar)
x %>% map_if(x == "kazutan", nchar)

iris %>% 
  split(.$Species) %>% 
  map(function(df) {
    lm(Petal.Width ~ Sepal.Length, data = df)
  })
iris %>% 
  split(.$Species) %>% 
  map(~lm(Petal.Width ~ Sepal.Length, data = .))

res <- x %>% map(~nchar)
res[[1]] %>% class()
res <- x %>% map(~nchar(.))
res[[1]] %>% class()

iris %>% 
  split(.$Species) %>% 
  map_dfc(~ mean(.$Sepal.Width))

# map色々(3) 引数ごとに異なる対象を指定
map2_int(
  .x = 1:3,
  .y = 4:6,
  .f = `+`
)

rnorm(n = 3, mean = 0, sd = 1)
# meanとsdの値を変更し、
# nは固定した正規分布に従う乱数を生成
map2(
  .x = 0, # mean
  .y = c(1, 1.5, 2), # sd
  .f = rnorm,
  n = 3
)

map2(.x = c(0, -1, 1), # mean
     .y = c(1, 1.5, 2), # sd
     .f = ~ rnorm(mean = .x, sd = .y, n = 3))
map2(.x = c(0, -1, 1), # mean
     .y = c(1, 1.5, 2), # sd
     .f = ~ rnorm(n = 3, .x, .y))

library(jpmesh)
# 緯度経度からメッシュコードを返却
# メッシュコードはメッシュサイズに応じて桁数が異なる
coords_to_mesh(longitude = 141.3468, latitude = 43.06462, mesh_size = "80km")
coords_to_mesh(141.3468, 43.06462, "1km")
coords_to_mesh(141.3468, 43.06462, "500m")

d <- tibble::data_frame(
  longitude = c(141.3468, 139.6917, 139.7147),
  latitude  = c(43.06462, 35.68949, 35.70078),
  mesh_size = c("80km", "1km", "500m")
)
d %>% 
  pmap_chr(jpmesh::coords_to_mesh)

tibble::data_frame(
  latitude  = c(43.06462, 35.68949, 35.70078),
  mesh_size = c("80km", "1km", "500m"),
  longitude = c(141.3468, 139.6917, 139.7147)
) %>% pmap_chr(jpmesh::coords_to_mesh)

tibble::data_frame(
  x = c(141.3468, 139.6917, 139.7147),
  y  = c(43.06462, 35.68949, 35.70078),
  z = c("80km", "1km", "500m")) %>%
  pmap_chr(., function(x, y, z) {
    jpmesh::coords_to_mesh(x, y, z)
    })

# purrr + tidyverse -------------------------------------------------------
library(tidyverse)
# 冒頭の問題
# irisデータセットをSpeciesごとに保存したcsv
target_files <- list.files("data/",
                           pattern = ".csv$",
                           full.names = TRUE)
target_files %>% map_df(readr::read_csv)

# 全変数が実数型であれば
# df %>% map_df(mean)
# 実数列だけを対象に
# df %>% map_if(is.double, mean)

walk2(paste0("img_", unique(iris$Species), ".png"),
      iris %>% 
        split(.$Species) %>% 
        map(~ggplot(., aes(Sepal.Length, Petal.Width)) + geom_point()), 
      ggsave, 
      # ggsave()に渡す引数（固定）
      width = 4, height = 3)
map2_int(1:3, 4:6, `+`)
walk2(1:3, 4:6, `+`)

# dplyrの関数内でmap()

df_mesh <- read_csv("data/mesh_1km.csv", col_types = "c")
df_mesh %>% sample_n(3L)
jpmesh::mesh_to_coords(meshcode = df_mesh$mesh_1km[1])

df_mesh_map <- df_mesh %>% 
  mutate(out = pmap(., ~ jpmesh::mesh_to_coords(meshcode = .x)))
df_mesh_map %>% unnest()

# グループに対してnest, unnest
iris_nest <- iris %>% 
  group_by(Species) %>% 
  nest()

all_equal(iris_nest %>% unnest(),
          iris)

iris_model <- iris_nest %>% 
  transmute(out = map(data, function(df) {
    broom::tidy(lm(Sepal.Length ~ Petal.Width, data = df))
  }))

iris_model %>% unnest()


# Appendix ----------------------------------------------------------------
# keep / discard
res <- x %>% map_at(1, nchar)
res %>% 
  keep(~ is.integer(.) == TRUE)
res %>% 
  discard(~ is.integer(.) == TRUE)

# invoke
# library(stringr)
c("str_to_upper", "str_to_title", "str_to_lower") %>% 
  invoke_map_chr(x)

# reduce
1:3 %>% reduce(`+`)
x %>% map(nchar) %>% 
  reduce(c)

# partial
set.seed(71)
f <- partial(runif, n = rpois(1, 5), .lazy = FALSE)
f
f()
f(min = 0.2)

# flatten
# 2階層のリスト
x <- list(
  list(hijiyama = c("kazutan")),
  list(tokyo = c("hoxom", "uribo"))
)
x %>% flatten()
x %>% unlist()
x %>% flatten() %>% flatten_chr()
