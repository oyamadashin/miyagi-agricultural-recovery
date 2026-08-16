# パッケージ読み込み----
library(tidyverse)
library(readxl) # Excelデータを読み込む
library(car) # VIFを求めるのに使う

# インポート----

## いったんすべて文字列として読み込む----
# （数値データはあとで数値に変えた方が安全）

by_farm_size_2010 <- read_excel(
  "02_processed_data/entities_by_farm_size_2010_miyagi_processed.xlsx",
  col_types = "text"
)

by_farm_size_2015 <- read_excel(
  "02_processed_data/entities_by_farm_size_2015_miyagi_processed.xlsx",
  col_types = "text"
)

by_farm_size_2020 <- read_excel(
  "02_processed_data/entities_by_farm_size_2020_miyagi_processed.xlsx",
  col_types = "text"
)

by_organization_type_2010 <- read_excel(
  "02_processed_data/entities_by_organization_type_2010_miyagi_processed.xlsx",
  col_types = "text"
)

by_organization_type_2015 <- read_excel(
  "02_processed_data/entities_by_organization_type_2015_miyagi_processed.xlsx",
  col_types = "text"
)

by_organization_type_2020 <- read_excel(
  "02_processed_data/entities_by_organization_type_2020_miyagi_processed.xlsx",
  col_types = "text"
)

# 前処理の続き----

## # 2020年データのarea_codeを作成する----

# 2020年データはarea_codeの構成要素がばらばらの列に入っているので、結合する。

by_farm_size_2020 <- by_farm_size_2020 |> 
  mutate(
    municipality_code = str_pad(
      municipality_code,
      width = 3,
      side = "left",
      pad = "0"
    ),
    former_municipality_code = str_pad(
      former_municipality_code,
      width = 2,
      side = "left",
      pad = "0"
    ),
    area_code = str_c(
      municipality_code,
      former_municipality_code,
      sep = "-"
    )
  )

by_organization_type_2020 <- by_organization_type_2020 |> 
  mutate(
    municipality_code = str_pad(
      municipality_code,
      width = 3,
      side = "left",
      pad = "0"
    ),
    former_municipality_code = str_pad(
      former_municipality_code,
      width = 2,
      side = "left",
      pad = "0"
    ),
    area_code = str_c(
      municipality_code,
      former_municipality_code,
      sep = "-"
    )
  )

# 不要になった municipality_code,former_municipality_codeを削除
by_farm_size_2020 <- by_farm_size_2020 |> 
  select(
    -municipality_code,
    -former_municipality_code
  )

by_organization_type_2020 <- by_organization_type_2020 |> 
  select(
    -municipality_code,
    -former_municipality_code
  )

## 事実不詳・未調査・秘匿・空欄の処理----
# まずは変換用の関数をつくる

convert_entity_columns <- function(df) {
  df |>
    mutate(
      across(
        -any_of(c(
          "area_name",
          "area_code",
          "municipality_code",
          "former_municipality_code",
          "census_year"
        )),
        ~ {
          x <- str_trim(as.character(.x))
          
          # 調査したが事実なし
          x[x %in% c("-", "－")] <- "0"
          
          # 事実不詳・未調査・秘匿・空欄
          x[x %in% c("", "...", "…", "X", "x")] <- NA_character_
          
          parse_number(
            x,
            locale = locale(grouping_mark = ",")
          )
        }
      )
    )
}


# 次に、この関数をそれぞれのデータセットに適用する

by_farm_size_2010 <- convert_entity_columns(by_farm_size_2010)
by_farm_size_2015 <- convert_entity_columns(by_farm_size_2015)
by_farm_size_2020 <- convert_entity_columns(by_farm_size_2020)

by_organization_type_2010 <- convert_entity_columns(by_organization_type_2010)
by_organization_type_2015 <- convert_entity_columns(by_organization_type_2015)
by_organization_type_2020 <- convert_entity_columns(by_organization_type_2020)



# データ結合----

# まず、「刈田郡」のように、見出しとしてのみ使われていて具体的なデータもarea_codeも持たない行を削除する。

by_farm_size_2010 <- by_farm_size_2010 |>
  filter(!is.na(area_code))

by_farm_size_2015 <- by_farm_size_2015 |>
  filter(!is.na(area_code))

by_farm_size_2020 <- by_farm_size_2020 |>
  filter(!is.na(area_code))

by_organization_type_2010 <- by_organization_type_2010 |>
  filter(!is.na(area_code))

by_organization_type_2015 <- by_organization_type_2015 |>
  filter(!is.na(area_code))

by_organization_type_2020 <- by_organization_type_2020 |>
  filter(!is.na(area_code))

# area_codeに重複がないことをチェック
by_farm_size_2010 |> count(area_code) |> filter(n > 1)
by_farm_size_2015 |> count(area_code) |> filter(n > 1)
by_farm_size_2020 |> count(area_code) |> filter(n > 1)

by_organization_type_2010 |> count(area_code) |> filter(n > 1)
by_organization_type_2015 |> count(area_code) |> filter(n > 1)
by_organization_type_2020 |> count(area_code) |> filter(n > 1)

# 同一年のデータを横結合する関数

join_census_tables <- function(farm_size_df, organization_df){
  farm_size_df |> 
    left_join(
      organization_df |> 
        select(
          -any_of(c("area_name", "census_year"))
        ),
      by = "area_code",
      relationship = "one-to-one"
    )
}

# 各年ごとにセンサスデータを横結合
census_2010 <- join_census_tables(
  by_farm_size_2010,
  by_organization_type_2010
)

census_2015 <- join_census_tables(
  by_farm_size_2015,
  by_organization_type_2015
)

census_2020 <- join_census_tables(
  by_farm_size_2020,
  by_organization_type_2020
)

df <- bind_rows(
  census_2010,
  census_2015,
  census_2020
) |>
  arrange(area_code, census_year)


# 追加の前処理----

## old_municipalityをつくる----
# 旧市区町村単位のデータかどうかを示すold_municipalityをつくる。

upper_level_codes <- c(
  "000-00",  # 宮城県計
  "100-00"   # 仙台市計
)

df <- df |>
  mutate(
    area_prefix = str_sub(area_code, 1, 3),
    area_suffix = str_sub(area_code, -2)
  ) |>
  group_by(area_prefix) |>
  mutate(
    # 3時点のいずれかに01以降があれば、下位地域ありと判定
    has_subarea = any(area_suffix != "00", na.rm = TRUE)
  ) |>
  ungroup() |>
  mutate(
    old_municipality = case_when(
      is.na(area_code) ~ NA_integer_,
      area_code %in% upper_level_codes ~ 0L,
      area_suffix != "00" ~ 1L,
      area_suffix == "00" & has_subarea ~ 0L,
      area_suffix == "00" & !has_subarea ~ 1L,
      TRUE ~ NA_integer_
    )
  )

## 2020年センサスの大規模層区分を2010, 2015年センサスに合わせる----

df <- df |> 
  mutate(
    entities_over_100_0ha = if_else(
      census_year == "2020",
      entities_100_0_150_0ha + entities_over_150_0ha,
      entities_over_100_0ha
    )
  )



## 大規模層経営体数（大規模の基準は複数設定）----
df <- df |>
  mutate(
    entities_over_5_0ha =
      entities_5_0_10_0ha +
      entities_10_0_20_0ha +
      entities_20_0_30_0ha +
      entities_30_0_50_0ha +
      entities_50_0_100_0ha +
      entities_over_100_0ha,
    
    entities_over_10_0ha =
      entities_10_0_20_0ha +
      entities_20_0_30_0ha +
      entities_30_0_50_0ha +
      entities_50_0_100_0ha +
      entities_over_100_0ha,
    
    entities_over_20_0ha =
      entities_20_0_30_0ha +
      entities_30_0_50_0ha +
      entities_50_0_100_0ha +
      entities_over_100_0ha,
    
    entities_over_30_0ha =
      entities_30_0_50_0ha +
      entities_50_0_100_0ha +
      entities_over_100_0ha,
    
    entities_over_50_0ha =
      entities_50_0_100_0ha +
      entities_over_100_0ha
  )


# 津波被災農地面積データの結合----

# 旧市区町村ごとの農地面積
farmland_total_by_kcity <- read_csv("02_processed_data/farmland_total_by_kcity.csv")

# 旧市区町村ごとの津波被災農地面積
tunami_farmland_total_by_kcity <- read_csv("02_processed_data/tsunami_farmland_total_by_kcity.csv")

# area_codeに重複がないことをチェック
farmland_total_by_kcity |> count(area_code) |> filter(n > 1)
tunami_farmland_total_by_kcity |> count(area_code) |> filter(n > 1)


# 両データフレームを結合し、津波被災農地割合を求める
tsunami_farmland_by_kcity <-
  farmland_total_by_kcity |>
  left_join(
    tunami_farmland_total_by_kcity,
    by = "area_code"
  ) |>
  mutate(
    tsunami_farmland_share =
      round(tunami_farmland_total_by_kcity / farmland_total_by_kcity, 2),
    farmland_total_by_kcity = round(farmland_total_by_kcity, 3), 
    tunami_farmland_total_by_kcity =  round(tunami_farmland_total_by_kcity, 3)
  )

# 割合の値がおかしくなっていないことをチェック
tsunami_farmland_by_kcity |>
  summarise(
    min_share = min(tsunami_farmland_share, na.rm = TRUE),
    max_share = max(tsunami_farmland_share, na.rm = TRUE)
  )

# dfに結合
df <- df |>
  left_join(
    tsunami_farmland_by_kcity |>
      select(area_code, 
             farmland_total_by_kcity, 
             tunami_farmland_total_by_kcity,
             tsunami_farmland_share),
    by = "area_code"
  )


# 旧市区町村コードが00で下位区分にも旧町村がある、という地域を取り除く
# たとえば気仙沼市（205-00）には下位区分の鹿折町（205-02）などがある。津波被災を表すGISデータでは、津波被災状況のデータが下位区分の町村の方には含まれるが、上位（たとえば気仙沼市（205-00））には含まれないので、そのままだと気仙沼市の津波被災農地面積がNAと表示されてしまう。
df <- df |>
  mutate(
    city_code = substr(area_code, 1, 3),
    kcity_code = substr(area_code, 5, 6)
  )

df <- df |>
  group_by(city_code) |>
  mutate(
    has_subarea = any(kcity_code != "00")
  ) |>
  ungroup()

df <- df |>
  filter(!(kcity_code == "00" & has_subarea))


# c-1、c-4事業実施地域データの結合----

c_one_four <- read_csv("02_processed_data/c1_c4_check.csv")

df <- df |>
  left_join(
    c_one_four |>
      select(area_code, 
             c_one,
             c_four),
    by = "area_code"
  )


# 機械所有データの結合----

entities_by_agricultural_machinery_2010 <- read_excel(
  "02_processed_data/entities_by_agricultural_machinery_2010_miyagi_processed.xlsx",
  col_types = "text"
)

entities_by_agricultural_machinery_2015 <- read_excel(
  "02_processed_data/entities_by_agricultural_machinery_2015_miyagi_processed.xlsx",
  col_types = "text"
)

# 2010年と2015年を縦に結合
entities_by_agricultural_machinery <- bind_rows(
  entities_by_agricultural_machinery_2010,
  entities_by_agricultural_machinery_2015
) |> 
  select(
    census_year,
    area_code,
    rice_transplanter_entities,
    rice_transplanter_units,
    tractor_entities,
    tractor_units,
    combine_entities,
    combine_units
  )

# dfに結合
df <- df |> 
  left_join(
    entities_by_agricultural_machinery,
    by = c("census_year", "area_code")
  )

# 秘匿データなどの処理をしておく
df <- convert_entity_columns(df)