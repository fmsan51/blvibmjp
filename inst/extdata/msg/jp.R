Sys.setlocale("LC_CTYPE", "Japanese")

double_sep <- "、"

msg <- list()

msg$English$plot_prev <- list(
  title = "Change of prevalence",
  xlab = "Months in simulation",
  ylab = "Prevalence"
  )
msg$Japanese$plot_prev <- list(
  title = "陽性率の推移",
  xlab = "シミュレーション開始後月数",
  ylab = "陽性率"
  )

msg$English$plot_route <- list(
  title = "Change of prevalence",
  legend_title = "Infection route",
  xlab = "Months in simulation",
  ylab = "Number of cattle"
  )
msg$Japanese$plot_route <- list(
  title = "感染原因別頭数",
  xlab = msg$Japanese$plot_prev$xlab,
  ylab = "頭数",
  legend_title = "感染原因"
  )

msg$English$redefine_route_levels <- list(
  uninfected = "Uninfected",
  initial = "Infected at month 0",
  tie_exposed_baseline = "Tie-stall, with an infected neighbor (baseline)",
  tie_exposed_risk = "Tie-stall, with an infected neighbor (due to the neighbor)",
  tie_non_exposed = "Tie-stall, without an infected neighbor",
  free = "Free-stall or outside",
  insects = "Insects",
  contact = "Direct contact",
  rp = "Rectal palpation",
  vertical = "Vertical transmission",
  colostrum = "Colostrum",
  introduced = "Introduction from another farm",
  pasture = "Infected at communal pasture",
  other = "Other"
)
msg$Japanese$redefine_route_levels <- list(
  uninfected = "非感染",
  initial = "開始時点での感染牛",
  tie_exposed_baseline = "つなぎ・隣に感染牛（ベースライン）",
  tie_exposed_risk = "つなぎ・隣に感染牛（隣接牛によるリスク）",
  tie_non_exposed = "つなぎ・隣は非感染牛",
  free = "フリーストール・外",
  insects = "吸血昆虫",
  contact = "感染牛との接触",
  rp = "直腸検査",
  vertical = "垂直感染",
  colostrum = "初乳",
  introduced = "感染牛の導入",
  pasture = "公共牧場",
  other = "その他"
)

msg <- purrr::map_depth(msg, 3, iconv, to = "UTF-8")

usethis::use_data(msg, double_sep, internal = T, overwrite = T)

