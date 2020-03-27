
<!-- README.md is generated from README.Rmd. Please edit that file -->

# blvibmjp

<!-- badges: start -->

<!-- badges: end -->

<!--
## Installation

You can install the released version of blvibmjp from [CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("blvibmjp")
```
-->

牛白血病ウイルス（BLV）の農場内での感染の広がり方をシミュレーションするパッケージです。

## Installation

R、RStudioおよびRtoolsをインストールしていない場合、まずそちらをインストールする。 インストール方法参考：

  - [RおよびRStudioのインストール方法(Mac/Windows) -
    Qiita](https://qiita.com/daifuku_mochi2/items/ad0b398e6affd0688c97)
  - [Rtoolsのインストール](https://k-metrics.github.io/cabinet/env_install_tls.html)

次に、RStudioで次のコードを実行することで、このパッケージをインストールする。

``` r
# 初回のみ
install.packages("remotes")
remotes::install_github("fmsan51/blvibmjp")
```

## Example

1.  以下のコードを実行すると、フォルダ名が表示される。

<!-- end list -->

``` r
file.path(.libPaths()[1], "blvibmjp")
```

2.  フォルダ内の input.xlsx を適当な場所にコピーして、牛・牛舎・移動などのデータを入力する。

3.  以下のようなコードを実行する。

<!-- end list -->

``` r
# パッケージの読み込み
library(blvibmjp)

# データの入力
data <- process_raw_data(
  "C:\\Users\\xxx\\Desktop\\input.xlsx"  # データを入力したファイルの場所
  )
# Note: method with signature ... や Following item(s) in the `infection_status` ... というメッセージは無視してOK

# シミュレーションの設定
param_simulation$simulation_length <- 60  # シミュレーション期間の長さ（月）
param_simulation$n_simulation <- 3  # シミュレーション回数

# 対策の設定
param_farm$control_insects <- 0.5  # 吸血昆虫対策を行っている場合、対策により吸血昆虫がどれだけ減少するか（0.5＝50%減）
param_farm$change_gloves <- TRUE  # 直検手袋を毎回交換するか（TRUE/FALSE）
param_farm$feed_raw_colostrum <- FALSE  # 凍結・加温処理していない初乳を子牛に与えているか
# 他、詳細は help("param_farm") 参照

# シミュレーションの実行（1回ごとに十数秒～数分）
simulate_blv_spread(param_simulation, param_farm, param_area,
                    processed_data = data)
```

4.  以下のコードで表示されるフォルダ内に、シミュレーション結果（simulationXX.csv）が保存される。

<!-- end list -->

``` r
output <- file.path(getwd(), "data", "output")
output
```

5.  シミュレーション結果をグラフにする。

<!-- end list -->

``` r
plot_infection_route(file.path(output, "simulation01.csv"),
                     language = "Japanese")
```
