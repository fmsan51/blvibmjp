
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

R、RStudioおよびRtoolsをインストールしていない場合、まずそちらをインストールする。  
インストール方法参考：

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
    （とりあえず適当なデータでシミュレーションを試したい、という場合は同フォルダ内のテスト用データ（input\_test.xlsx）を使ってみてください。）

3.  以下のようなコードを実行する。

<!-- end list -->

``` r
# パッケージの読み込み
library(blvibmjp)

# シミュレーションの設定
param$simulation_length <- 60  # シミュレーション期間の長さ（月）
param$n_simulation <- 3  # シミュレーション回数
param$output_dir <- "result"  # シミュレーション結果の保存フォルダ

# 対策の設定
param$control_insects <- 0.3  # 吸血昆虫対策を行っている場合、対策により吸血昆虫がどれだけ減少するか（0.3＝30%に減少（7割減））
param$change_gloves <- TRUE  # 直検手袋を毎回交換するか（TRUE/FALSE）
param$feed_raw_colostrum <- FALSE  # 凍結・加温処理していない初乳を子牛に与えているか
# 他、詳細は help("param") 参照
```

``` r
# データの入力
data <- prepare_data(
  "C:\\Users\\xxx\\Desktop\\input.xlsx",  # データを入力したファイルの場所
  param)
```

``` r
# シミュレーションの実行（1回ごとに十数秒～数分）
simulate_blv_spread(data, param)
```

4.  以下のコードで表示されるフォルダ内に、シミュレーション結果（simulationXX.csv）が保存される。

<!-- end list -->

``` r
output <- file.path(getwd(), param$output_dir)
output
```

5.  シミュレーション結果をグラフ・表にする。

<!-- end list -->

``` r
plot_prev(param, language = "Japanese")  # 陽性率の推移
```

<img src="man/figures/README-plot-1.png" width="100%" />

``` r
plot_route(param, language = "Japanese")  # 感染原因別頭数
```

<img src="man/figures/README-plot-2.png" width="100%" />

``` r
calc_prev(param, type = "prop")  # 月ごとの感染率
#>     i_month prevalence
#>  1:       0  0.2000000
#>  2:       1  0.2500000
#>  3:       2  0.2500000
#>  4:       3  0.2500000
#>  5:       4  0.2500000
#>  6:       5  0.2500000
#>  7:       6  0.2857143
#>  8:       7  0.2500000
#>  9:       8  0.2500000
#> 10:       9  0.2500000
#> 11:      10  0.3750000
#> 12:      11  0.3000000
#> 13:      12  0.3333333
#> 14:      13  0.3333333
#> 15:      14  0.3333333
#> 16:      15  0.3750000
#> 17:      16  0.3750000
#> 18:      17  0.2857143
#> 19:      18  0.2857143
#> 20:      19  0.2857143
#> 21:      20  0.2857143
#> 22:      21  0.2857143
#> 23:      22  0.2857143
#> 24:      23  0.2857143
#> 25:      24  0.2857143
#> 26:      25  0.2857143
#> 27:      26  0.1666667
#> 28:      27  0.1666667
#> 29:      28  0.1666667
#> 30:      29  0.1666667
#> 31:      30  0.1666667
#> 32:      31  0.1666667
#> 33:      32  0.2000000
#> 34:      33  0.2000000
#> 35:      34  0.2000000
#> 36:      35  0.2000000
#> 37:      36  0.2000000
#> 38:      37  0.4000000
#> 39:      38  0.3333333
#> 40:      39  0.3333333
#> 41:      40  0.3333333
#> 42:      41  0.3333333
#> 43:      42  0.3333333
#> 44:      43  0.2000000
#> 45:      44  0.2000000
#> 46:      45  0.2000000
#> 47:      46  0.2000000
#> 48:      47  0.2000000
#> 49:      48  0.2000000
#> 50:      49  0.3333333
#> 51:      50  0.3333333
#> 52:      51  0.4000000
#> 53:      52  0.4000000
#> 54:      53  0.4000000
#> 55:      54  0.4000000
#> 56:      55  0.4000000
#> 57:      56  0.4000000
#> 58:      57  0.4000000
#> 59:      58  0.6666667
#> 60:      59  0.6666667
#> 61:      60  0.6666667
#>     i_month prevalence
calc_prev(param, type = "count")  # 月ごとの感染牛頭数
#>     i_month inf noinf
#>  1:       0   2     8
#>  2:       1   2     6
#>  3:       2   2     6
#>  4:       3   2     6
#>  5:       4   2     6
#>  6:       5   2     6
#>  7:       6   2     5
#>  8:       7   2     6
#>  9:       8   2     6
#> 10:       9   2     6
#> 11:      10   3     5
#> 12:      11   3     7
#> 13:      12   3     6
#> 14:      13   3     6
#> 15:      14   3     6
#> 16:      15   3     5
#> 17:      16   3     5
#> 18:      17   2     5
#> 19:      18   2     5
#> 20:      19   2     5
#> 21:      20   2     5
#> 22:      21   2     5
#> 23:      22   2     5
#> 24:      23   2     5
#> 25:      24   2     5
#> 26:      25   2     5
#> 27:      26   1     5
#> 28:      27   1     5
#> 29:      28   1     5
#> 30:      29   1     5
#> 31:      30   1     5
#> 32:      31   1     5
#> 33:      32   1     4
#> 34:      33   1     4
#> 35:      34   1     4
#> 36:      35   1     4
#> 37:      36   1     4
#> 38:      37   2     4
#> 39:      38   2     4
#> 40:      39   2     4
#> 41:      40   2     4
#> 42:      41   2     4
#> 43:      42   2     4
#> 44:      43   1     4
#> 45:      44   1     4
#> 46:      45   1     4
#> 47:      46   1     4
#> 48:      47   1     4
#> 49:      48   1     4
#> 50:      49   2     4
#> 51:      50   2     3
#> 52:      51   2     2
#> 53:      52   2     2
#> 54:      53   2     2
#> 55:      54   2     2
#> 56:      55   2     2
#> 57:      56   2     2
#> 58:      57   2     2
#> 59:      58   2     1
#> 60:      59   2     1
#> 61:      60   2     1
#>     i_month inf noinf
calc_prev(param, type = "status")  # 月ごとの感染ステージ別頭数（s=非感染、ial=感染・無症状、ipl=持続性リンパ球増多症、ebl=地方病性牛白血病）
#>     i_month s ial ipl ebl
#>  1:       0 8   2   0   0
#>  2:       1 6   2   0   0
#>  3:       2 6   2   0   0
#>  4:       3 6   2   0   0
#>  5:       4 6   2   0   0
#>  6:       5 6   2   0   0
#>  7:       6 5   2   0   0
#>  8:       7 6   2   0   0
#>  9:       8 6   2   0   0
#> 10:       9 6   2   0   0
#> 11:      10 5   3   0   0
#> 12:      11 7   3   0   0
#> 13:      12 6   3   0   0
#> 14:      13 6   3   0   0
#> 15:      14 6   3   0   0
#> 16:      15 5   3   0   0
#> 17:      16 5   3   0   0
#> 18:      17 5   2   0   0
#> 19:      18 5   2   0   0
#> 20:      19 5   2   0   0
#> 21:      20 5   2   0   0
#> 22:      21 5   2   0   0
#> 23:      22 5   2   0   0
#> 24:      23 5   2   0   0
#> 25:      24 5   2   0   0
#> 26:      25 5   2   0   0
#> 27:      26 5   1   0   0
#> 28:      27 5   1   0   0
#> 29:      28 5   1   0   0
#> 30:      29 5   1   0   0
#> 31:      30 5   1   0   0
#> 32:      31 5   1   0   0
#> 33:      32 4   1   0   0
#> 34:      33 4   1   0   0
#> 35:      34 4   1   0   0
#> 36:      35 4   1   0   0
#> 37:      36 4   1   0   0
#> 38:      37 4   2   0   0
#> 39:      38 4   2   0   0
#> 40:      39 4   2   0   0
#> 41:      40 4   2   0   0
#> 42:      41 4   2   0   0
#> 43:      42 4   2   0   0
#> 44:      43 4   1   0   0
#> 45:      44 4   1   0   0
#> 46:      45 4   1   0   0
#> 47:      46 4   1   0   0
#> 48:      47 4   1   0   0
#> 49:      48 4   1   0   0
#> 50:      49 4   2   0   0
#> 51:      50 3   2   0   0
#> 52:      51 2   2   0   0
#> 53:      52 2   2   0   0
#> 54:      53 2   2   0   0
#> 55:      54 2   2   0   0
#> 56:      55 2   2   0   0
#> 57:      56 2   2   0   0
#> 58:      57 2   2   0   0
#> 59:      58 1   2   0   0
#> 60:      59 1   2   0   0
#> 61:      60 1   2   0   0
#>     i_month s ial ipl ebl
```

``` r
write.csv(calc_prev(param, type = "prop"), file.path(output, "prev.csv"))  # 表をcsvに保存（保存場所は 4. で表示されたフォルダ）
```
