# research_from_2018

#各国各年の代表値を算出する方法

０．必要なパッケージをインストールします．

１．入力ファイルがそろっているかを確認します．

２．crop_calendar_converter.R のsettingsをいじります．
 /* 年数が多いと，万が一の時に悲しくなるので，最初は短い年数で回してみるのがいいとおもいます． */

３．作業ディレクトリを "./script/" にします．

４．実行コードを入力します（ファイルを実行する）．
source("crop_calendar_converter.R")
source("climate_representer.R")

５．環境にもよるが，数時間で出来上がるでしょう．

#memo
180612
要修正箇所　年ラベル貼り付けの問題、年末の収穫は何年に反映されるかの問題

気になったこと　summary関数のmaxとmax関数のna.rm=Tは一致しない？
零がたくさんあるとsummary$maxは零になる？
