library(dplyr)
library(tibble)
library(data.table)
library(tidyr)
library(readxl)
library(openxlsx)
library(stringr)

#' -----------------------------------------------------------------------------
#' @title: admin_label
#' -----------------------------------------------------------------------------
#' @description:
#' This function adds region, province and district and even mixed labels to 
#' the given data set.
#' This function needs regular 420 district codes, not UNOCHA codes.
#' @param: df, data frame
#' @param: region code, string
#' @param: province code, string
#' @param: district code, string
#' @return: df, data frame
#' -----------------------------------------------------------------------------

region_codes = c(
  "CH",
  "CR",
  "ER",
  "NE",
  "NR",
  "SE",
  "SR",
  "WR"
)

region_lables = c(
  "Central Highland",
  "Capital",
  "Eastern",
  "North Eastern",
  "Northern",
  "South Eastern",
  "Southern",
  "Western"
)

province_codes = c(
  "AF01","AF02","AF03","AF04","AF05",
  "AF06","AF07","AF08","AF09","AF10",
  "AF11","AF12","AF13","AF14","AF15",
  "AF16","AF17","AF18","AF19","AF20",
  "AF21","AF22","AF23","AF24","AF25",
  "AF26","AF27","AF28","AF29","AF30",
  "AF31","AF32","AF33","AF34"
)


province_lables = c(
  "Kabul","Kapisa","Parwan","Maidan Wardak","Logar",
  "Nangarhar","Laghman","Panjsher","Baghlan","Bamyan",
  "Ghazni","Paktika","Paktya","Khost","Kunar",
  "Nuristan","Badakhshan","Takhar","Kunduz","Samangan",
  "Balkh","Sar-e-Pul","Ghor","Daykundi","Uruzgan",
  "Zabul","Kandahar","Jawzjan","Faryab","Hilmand",
  "Badghis","Hirat","Farah","Nimroz"
)

district_codes = c(
  "AF0101","AF0102","AF0103","AF0104","AF0105",
  "AF0106","AF0107","AF0108","AF0109","AF0110",
  "AF0111","AF0112","AF0113","AF0114","AF0115",
  "AF0201","AF0202","AF0203","AF0204","AF0205",
  "AF0206","AF0207","AF0301","AF0302","AF0303",
  "AF0304","AF0305","AF0306","AF0307","AF0308",
  "AF0309","AF0310","AF0401","AF0402","AF0403",
  "AF0404","AF0405","AF0406","AF0407","AF0408",
  "AF0409","AF0501","AF0502","AF0503","AF0504",
  "AF0505","AF0506","AF0507","AF0601","AF0602",
  "AF0603","AF0604","AF0605","AF0606","AF0607",
  "AF0608","AF0609","AF0610","AF0611","AF0612",
  "AF0613","AF0614","AF0615","AF0616","AF0617",
  "AF0618","AF0619","AF0620","AF0621","AF0622",
  "AF0701","AF0702","AF0703","AF0704","AF0705",
  "AF0801","AF0802","AF0803","AF0804","AF0805",
  "AF0806","AF0807","AF0901","AF0902","AF0903",
  "AF0904","AF0905","AF0906","AF0907","AF0908",
  "AF0909","AF0910","AF0911","AF0912","AF0913",
  "AF0914","AF0915","AF1001","AF1002","AF1003",
  "AF1004","AF1005","AF1006","AF1007","AF1101",
  "AF1102","AF1103","AF1104","AF1105","AF1106",
  "AF1107","AF1108","AF1109","AF1110","AF1111",
  "AF1112","AF1113","AF1114","AF1115","AF1116",
  "AF1117","AF1118","AF1119","AF1201","AF1202",
  "AF1203","AF1204","AF1205","AF1206","AF1207",
  "AF1208","AF1209","AF1210","AF1211","AF1212",
  "AF1213","AF1214","AF1215","AF1216","AF1217",
  "AF1218","AF1219","AF1301","AF1302","AF1303",
  "AF1304","AF1305","AF1306","AF1307","AF1308",
  "AF1309","AF1310","AF1311","AF1401","AF1402",
  "AF1403","AF1404","AF1405","AF1406","AF1407",
  "AF1408","AF1409","AF1410","AF1411","AF1412",
  "AF1413","AF1501","AF1502","AF1503","AF1504",
  "AF1505","AF1506","AF1507","AF1508","AF1509",
  "AF1510","AF1511","AF1512","AF1513","AF1514",
  "AF1515","AF1601","AF1602","AF1603","AF1604",
  "AF1605","AF1606","AF1607","AF1608","AF1701",
  "AF1702","AF1703","AF1704","AF1705","AF1706",
  "AF1707","AF1708","AF1709","AF1710","AF1711",
  "AF1712","AF1713","AF1714","AF1715","AF1716",
  "AF1717","AF1718","AF1719","AF1720","AF1721",
  "AF1722","AF1723","AF1724","AF1725","AF1726",
  "AF1727","AF1728","AF1801","AF1802","AF1803",
  "AF1804","AF1805","AF1806","AF1807","AF1808",
  "AF1809","AF1810","AF1811","AF1812","AF1813",
  "AF1814","AF1815","AF1816","AF1817","AF1901",
  "AF1902","AF1903","AF1904","AF1905","AF1906",
  "AF1907","AF2001","AF2002","AF2003","AF2004",
  "AF2005","AF2006","AF2007","AF2101","AF2102",
  "AF2103","AF2104","AF2105","AF2106","AF2107",
  "AF2108","AF2109","AF2110","AF2111","AF2112",
  "AF2113","AF2114","AF2115","AF2116","AF2201",
  "AF2202","AF2203","AF2204","AF2205","AF2206",
  "AF2207","AF2301","AF2302","AF2303","AF2304",
  "AF2305","AF2306","AF2307","AF2308","AF2309",
  "AF2310","AF2401","AF2402","AF2403","AF2404",
  "AF2405","AF2406","AF2407","AF2408","AF2409",
  "AF2501","AF2502","AF2503","AF2504","AF2505",
  "AF2506","AF2507","AF2601","AF2602","AF2603",
  "AF2604","AF2605","AF2606","AF2607","AF2608",
  "AF2609","AF2610","AF2611","AF2701","AF2702",
  "AF2703","AF2704","AF2705","AF2706","AF2707",
  "AF2708","AF2709","AF2710","AF2711","AF2712",
  "AF2713","AF2714","AF2715","AF2716","AF2801",
  "AF2802","AF2803","AF2804","AF2805","AF2806",
  "AF2807","AF2808","AF2809","AF2810","AF2811",
  "AF2901","AF2902","AF2903","AF2904","AF2905",
  "AF2906","AF2907","AF2908","AF2909","AF2910",
  "AF2911","AF2912","AF2913","AF2914","AF3001",
  "AF3002","AF3003","AF3004","AF3005","AF3006",
  "AF3007","AF3008","AF3009","AF3010","AF3011",
  "AF3012","AF3013","AF3101","AF3102","AF3103",
  "AF3104","AF3105","AF3106","AF3107","AF3201",
  "AF3202","AF3203","AF3204","AF3205","AF3206",
  "AF3207","AF3208","AF3209","AF3210","AF3211",
  "AF3212","AF3213","AF3214","AF3215","AF3216",
  "AF3301","AF3302","AF3303","AF3304","AF3305",
  "AF3306","AF3307","AF3308","AF3309","AF3310",
  "AF3311","AF3401","AF3402","AF3403","AF3404",
  "AF3405"
  
)

district_lables = c(
  "Kabul","Paghman","Chahar Asyab","Bagrami","Deh Sabz",
  "Shakar Dara","Musahi","Mir Bacha Kot","Khak-e-Jabbar","Kalakan",
  "Guldara","Farza","Estalef","Qara Bagh","Surobi",
  "Mahmood-e-Raqi","Hisa-e-Duwum-e-Kohistan","Koh Band","Hisa-e-Awal-e-Kohistan","Nijrab",
  "Tagab","Alasay","Charikar","Bagram","Shinwari",
  "Sayed Khel","Jabal Saraj","Salang","Ghorband","Koh-e-Safi",
  "Surkh-e-Parsa","Shekh Ali","Maydan Shahr","Nerkh","Jalrez",
  "Chak-e-Wardak","Saydabad","Daymirdad","Hesa-e-Awal-e-Behsud","Jaghatu",
  "Markaz-e-Behsud","Pul-e-Alam","Baraki Barak","Charkh","Khoshi",
  "Mohammad Agha","Kharwar","Azra","Jalalabad","Behsud",
  "Surkh Rod","Chaparhar","Kama","Kuz Kunar","Rodat",
  "Khogyani","Bati Kot","Deh Bala","Pachir Wa Agam","Dara-e-Nur",
  "Kot","Goshta","Achin","Shinwar","Muhmand Dara",
  "Lalpur","Sherzad","Nazyan","Hesarak","Dur Baba",
  "Mehtarlam","Qarghayi","Alishang","Alingar","Dawlatshah",
  "Bazarak","Rukha","Dara","Khenj","Anawa",
  "Shutul","Paryan","Pul-e-Khumri","Dahana-e-Ghori","Doshi",
  "Nahrin","Baghlan-e-Jadid","Khinjan","Andarab","Deh Salah",
  "Khwaja Hejran","Burka","Tala Wa Barfak","Pul-e-Hisar","Khost Wa Fereng",
  "Guzargah-e-Nur","Fereng Wa Gharu","Bamyan","Shibar","Sayghan",
  "Kahmard","Yakawlang","Panjab","Waras","Ghazni",
  "Wal-e-Muhammad-e-Shahid","Khwaja Umari","Waghaz","Deh Yak","Jaghatu",
  "Andar","Zanakhan","Rashidan","Nawur","Qara Bagh",
  "Giro","Ab Band","Jaghuri","Muqur","Malistan",
  "Gelan","Ajristan","Nawa","Sharan","Mata Khan",
  "Yosuf Khel","Yahya Khel","Sar Rawzah","Omna","Zarghun Shahr",
  "Gomal","Jani Khel","Surobi","Urgun","Ziruk",
  "Nika","Barmal","Giyan","Dila","Wazakhah",
  "Wormamay","Turwo","Gardez","Ahmadaba","Zurmat",
  "Shawak","Zadran","Sayed Karam","Jaji","Lija Ahmad Khel",
  "Jani Khel","Chamkani","Dand Wa Patan","Matun","Mandozayi",
  "Gurbuz","Tani","Musa Khel","Nadir Shah Kot","Sabari",
  "Terezayi","Bak","Qalandar","Spera","Shamal",
  "Jaji Maydan","Asad Abad","Marawara","Watapur","Narang",
  "Sar Kani","Shigal","Dara-e-Pech","Bar Kunar","Chawkay",
  "Khas Kunar","Ghazi Abad","Dangam","Chapa Dara","Nurgal",
  "Nari","Parun","Waygal","Wama","Nurgaram",
  "Duab","Kamdesh","Mandol","Barg-e-Matal","Fayzabad",
  "Argo","Arghanj Khwah","Yaftal-e-Sufla","Khash","Baharak",
  "Darayem","Kohistan","Yawan","Jorm","Teshkan",
  "Shuhada","Shahr-e-Buzorg","Raghestan","Keshem","Warduj",
  "Tagab","Yamgan","Shighnan","Khwahan","Kofab",
  "Darwaz-e-Payin","Eshkashem","Shaki","Zebak","Koran Wa Monjan",
  "Darwaz-e-Balla","Wakhan","Taloqan","Hazar Sumuch","Baharak",
  "Bangi","Chal","Namak Ab","Kalafgan","Farkhar",
  "Khwaja Ghar","Rostaq","Eshkmesh","Dasht-e-Qala","Warsaj",
  "Khwaja Bahawuddin","Darqad","Chahab","Yangi Qala","Kunduz",
  "Chahar Darah","Ali Abad","Khan Abad","Imam Sahib","Dasht-e-Archi",
  "Qala-e-Zal","Aybak","Hazrat-e-Sultan","Khuram Wa Sarbagh","Feroz Nakhchir",
  "Ruy-e-Duab","Dara-e-Suf-e-Payin","Dara-e-Suf-e-Bala","Mazar-e-Sharif","Nahr-e-Shahi",
  "Dehdadi","Charkent","Marmul","Balkh","Sholgareh",
  "Chemtal","Dawlat Abad","Khulm","Char Bolak","Shortepa",
  "Kaldar","Keshendeh","Zari","Sharak-e-Hayratan","Sar-e-Pul",
  "Sayad","Kohestanat","Sozmaqala","Sancharak","Gosfandi",
  "Balkhab","Feroz Koh","DoLayna","Dawlatyar","Charsadra",
  "Pasaband","Shahrak","Lal Wa Sarjangal","Taywarah","Tolak",
  "Saghar","Nili","Shahrestan","Ashtarlay","Khadir",
  "Kiti","Miramor","Sang-e-Takht","Kajran","Patoo",
  "Tirinkot","Dehrawud","Chora","Shahid-e-Hassas","Khas Uruzgan",
  "Chinarto","Gizab","Qalat","Tarnak Wa Jaldak","Shinkay",
  "Mizan","Arghandab","Shah Joi","Daychopan","Atghar",
  "Nawbahar","Shamul Zayi","Kakar","Kandahar","Arghandab",
  "Daman","Panjwayi","Zheray","Shah Wali Kot","Khakrez",
  "Arghestan","Ghorak","Maywand","Spin Boldak","Nesh",
  "Miyanshin","Shorabak","Maruf","Reg","Shiberghan",
  "Khwaja Dukoh","Khanaqa","Mingajik","Qush Tepa","Khamyab",
  "Aqcha","Fayzabad","Mardyan","Qarqin","Darzab",
  "Maymana","Pashtun Kot","Khwaja Sabz Posh","Almar","Bilcheragh",
  "Shirin Tagab","Qaysar","Garzewan","Dawlat Abad","Kohistan",
  "Qaram Qul","Qurghan","Andkhoy","Khan-e-Char Bagh","Lashkargah",
  "Nad-e-Ali","Nawa-e-Barakzaiy","Nahr-e-Saraj","Washer","Garmser",
  "Nawzad","Sangin","Musa Qala","Kajaki","Reg-i-Khan Nishin",
  "Baghran","Deh-e-Shu","Qala-e-Naw","Ab Kamari","Muqur",
  "Qadis","Bala Murghab","Jawand","Ghormach","Hirat",
  "Injil","Guzara","Karukh","Zindajan","Pashtun Zarghun",
  "Kushk","Gulran","Adraskan","Kushk-e-Kuhna","Ghoryan",
  "Obe","Kohsan","Shindand","Farsi","Chisht-e-Sharif",
  "Farah","Pushtrod","Khak-e-Safed","Qala-e-Kah","Shibkoh",
  "Bala Buluk","Anar Dara","Bakwa","Lash-e-Juwayn","Gulistan",
  "Pur Chaman","Zaranj","Kang","Chakhansur","Char Burjak",
  "Khashrod"
)

region_ref <- data.frame(region_codes, region_lables)
province_ref <- data.frame(province_codes, province_lables)
district_ref <- data.frame(district_codes, district_lables)


# combined all codes and labels into one single data frame
all_ref <- rbindlist(list(region_ref, province_ref, district_ref), use.names = FALSE)
colnames(all_ref)[colnames(all_ref) == "region_codes"] = "all_codes"
colnames(all_ref)[colnames(all_ref) == "region_lables"] = "all_lables"

all_ref <- as.data.frame(all_ref)
all_ref <- rbind(c("all","All"), all_ref)

admin_label <- function(data, afg, region, province, district, remove_codes = FALSE){

  if(missing(data)){
    message('Please set a data sets.')
    opt <- options(show.error.messages=FALSE)
    on.exit(options(opt))
    stop()
  }else{
    df_name <- deparse(substitute(data))
    if (!exists(df_name)){
      message('The provicded data set dose not exist.')
      opt <- options(show.error.messages=FALSE)
      on.exit(options(opt))
      stop()
    }
  }

  df <- data
  
  if(!missing(afg)){
    df <- extract_lable(df, afg, remove_codes)
  }
  
  
  if(!missing(region)){
    df <- extract_lable(df, region, remove_codes)
  }

  if(!missing(province)){
    df <- extract_lable(df, province, remove_codes)
  }

  if(!missing(district)){
    df <- extract_lable(df, district, remove_codes)
  }
  
  return(df)

}

extract_lable <- function(target_df, param, param_code){

  tryCatch(
    {
      if(!is.character(param)){
        message('The parameter is not string.')
        opt <- options(show.error.messages=FALSE)
        on.exit(options(opt))
        stop()
      }
      
      param_index <- grep(param, colnames(target_df))
      target_df1 <- target_df
      colnames(target_df1)[colnames(target_df1) == colnames(target_df1[param])] = "param_column"
      target_df1$param_column <- as.character(target_df1$param_column)
    },
    error=function(e) {
      message("Please check the parameters. The parameters should be a valid string name of contaning area codes.")
      opt <- options(show.error.messages=FALSE)
      on.exit(options(opt))
      stop()
    }
  )
  
  # left joining target data with reference data based on the type of parameter
  code_type <- deparse(substitute(param))
  result = switch(  
    code_type,  
    "afg" = {
      
      target_df2 <- target_df1 %>% left_join(all_ref, by= c("param_column"="all_codes"))
      
      if(sum(!is.na(target_df2$all_lables)) > 0){
          target_df2 <- target_df2 %>% mutate(
            param_name = all_lables
          )
          target_df2 <- target_df2 %>% relocate(param_name, .after=param_column)
          colnames(target_df2)[colnames(target_df2) == "param_name"] = "area_name"
          target_df2$all_lables <- NULL
          
        }else{
          warning("The afg parameter has been ignored. No lables found.")
          return(target_df)
          
        }
      },
    "region" = {
      target_df2 <- target_df1 %>% left_join(region_ref, by= c("param_column"="region_codes"))
      
      if(sum(!is.na(target_df2$region_lables)) > 0){
      target_df2 <- target_df2 %>% mutate(
        param_name = region_lables
      )
      target_df2 <- target_df2 %>% relocate(param_name, .after=param_column)
      colnames(target_df2)[colnames(target_df2) == "param_name"] = "region_name"
      target_df2$region_lables <- NULL
      
      }else{
        warning("No lables found in the region.")
        return(target_df)
      }
    },
    "province" = {
      target_df2 <- target_df1 %>% left_join(province_ref, by= c("param_column"="province_codes"))
      
      if(sum(!is.na(target_df2$province_lables)) > 0){      
      target_df2 <- target_df2 %>% mutate(
        param_name = province_lables
      )
      target_df2 <- target_df2 %>% relocate(param_name, .after=param_column)
      colnames(target_df2)[colnames(target_df2) == "param_name"] = "province_name"
      target_df2$province_lables <- NULL
      
      }else{
        warning("No lables found in the province.")
        return(target_df)
      }
    },
    "district" = {
      target_df2 <- target_df1 %>% left_join(district_ref, by= c("param_column"="district_codes"))
      
      if(sum(!is.na(target_df2$district_lables)) > 0){ 
      target_df2 <- target_df2 %>% mutate(
        param_name = district_lables
      )
      target_df2 <- target_df2 %>% relocate(param_name, .after=param_column)
      colnames(target_df2)[colnames(target_df2) == "param_name"] = "district_name"
      target_df2$district_lables <- NULL
      
      }else{
        warning("No lables found in the district.")
        return(target_df)
      }
    }
  )
  
  if (param_code){
    target_df2 <- target_df2[,-param_index]
  }else{
    colnames(target_df2)[param_index]= param
  }
  
  cat(paste0(code_type, "_name added."), sep = "\n")
  return(target_df2)
}


