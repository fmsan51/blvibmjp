Sys.setlocale("LC_CTYPE", "Japanese")

Japanese_plot_prev <- list(
  title = "陽性率の推移",
  xlab = "シミュレーション開始後月数",
  ylab = "陽性率")

Japanese_plot_route <- list(
  title = "感染原因別頭数",
  xlab = Japanese_plot_prev$xlab,
  ylab = "頭数",
  legend_title = "感染原因")

Japanese_redefine_route_levels <- list(
  route_labels = c(
    uninfected = "非感染",
    initial = "開始時点での感染牛",
    insects = "吸血昆虫",
    contact = "感染牛との接触",
    rp = "直腸検査",
    vertical = "垂直感染",
    colostrum = "初乳",
    introduced = "感染牛の導入",
    pasture = "公共牧場",
    other = "その他")
)

Japanese_plot_prev <- lapply(Japanese_plot_prev, iconv, to = "UTF-8")
Japanese_plot_route <- lapply(Japanese_plot_route, iconv, to = "UTF-8")
Japanese_redefine_route_levels <- lapply(Japanese_redefine_route_levels,
                                         iconv, to = "UTF-8")

usethis::use_data(Japanese_plot_prev, Japanese_plot_route,
                  Japanese_redefine_route_levels,
                  internal = T, overwrite = T)
