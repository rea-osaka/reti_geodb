############################################################################
# retiの所在に対応する緯度経度情報をもつデータテーブルを作成
# 情報元
#  位置参照情報
#   「大字・町丁目位置参照情報　国土交通省」
#    http://nlftp.mlit.go.jp/isj/index.html
#  
#  不動産取引価格情報
#   「不動産取引価格情報　国土交通省」
#    http://www.land.mlit.go.jp/webland/download.html
############################################################################

library(tidyverse)
library(reti)

############################################################################
# 一時配布元からダウンロードしたcsvファイルを保存してあるディレクトリのパス
############################################################################
geopath <- "./geocsv"
pricepath <- "./reticsv"


############################################################################
# オリジナルの位置参照情報CSVデータをＲに取り込む
############################################################################
retisub_read_original_geotable_csv <- function(path){
    
    df_list <- lapply(path, utils::read.csv, fileEncoding = "cp932",
                      stringsAsFactors = F, na.strings = c("","NULL"))
    
    ans <- do.call(rbind, df_list)
#    ans <- select(ans, 市区町村コード, 大字町丁目名, 緯度, 経度)
    ans <- select(ans, 市区町村コード, 大字町丁目名, 緯度, 経度)
    
    names(ans) <- c("code", "地区名", "緯度", "経度")
    
    return(ans)
}

############################################################################
# retiデータ上で、code,地区名の組み合わせがユニークになるものを取り出す
############################################################################
retisub_get_uniqe_addr <- function(path){
    df <- read_csvfile(path)
    ans  <- df  %>% select(code,地区名) %>% unique()  %>% droplevels()
    ans <- subset(ans, !is.na(ans$code) & !is.na(ans$地区名))
    return(ans)
}

############################################################################
# 市町村コードと所在文字列から位置情報を計算するサブルーチン
############################################################################
retisubu_calc_mean_geocode <- function(city_code, addr_str, original_db){
    
    # 与えられた市町村コードに含まれる所在文字列のベクトルを返す
    # original_dbが基本となるデータベース
    same_city_code <- subset(original_db, original_db$code == city_code)
    
    # 検索パターンを決定する
    # 今のところ、先頭から一致する文字列
    my_pattern <- paste0("^", addr_str)
    
    # 候補データを決定
    candidate_table <- subset(same_city_code, str_detect(same_city_code$地区名, my_pattern))
    
    # 経度、緯度の平均を返す
    lon_lat <- c(lon = mean(candidate_table$経度), lat = mean(candidate_table$緯度))
    
    return(lon_lat)
    
}

############################################################################
# テーブルデータを作る
############################################################################
retisub_make_geotable <- function(unique_addr_df, original_db){
    
    len <- nrow(unique_addr_df)
    ans_table <- data.frame(code = numeric(len),
                            地区名 = numeric(len),
                            経度 = numeric(len), 
                            緯度 = numeric(len) )
    
    for(i in 1:len){
        
        # 位置情報の作り方を決定
        pos <- retisubu_calc_mean_geocode(unique_addr_df[i,]$code,
                                 unique_addr_df[i,]$地区名,
                                 original_db)
        
        ans_table[i,]$code <- unique_addr_df[i,]$code
        ans_table[i,]$地区名 <- unique_addr_df[i,]$地区名  %>% as.character()
        ans_table[i,]$経度 <- pos[1]
        ans_table[i,]$緯度 <- pos[2]
        
    }
    
    return(ans_table)
} 


############################################################################
# メインの呼び出しルーチン
############################################################################

reti_make_geodb <- function(geocsvs_path, reticsv_path){
    # geo参照情報
    original_db <- retisub_read_original_geotable_csv(geocsvs_path)

    # retiのユニークアドレスデータ
    unique_addr_df <- retisub_get_uniqe_addr(reticsv_path)

    return(retisub_make_geotable(unique_addr_df, original_db))
}

