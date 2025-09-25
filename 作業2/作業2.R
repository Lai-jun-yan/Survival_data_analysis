#資料讀取
data = read.csv("melanoma.csv",header = TRUE)

###資料前處理
#設定censoring indicator
for(i in 1:nrow(data)){
  if(data[i,"Indicator"]==1){
    data[i,"delta"] = 1
  }else {
    data[i,"delta"] = 0
  }
}

#設定兩個dummy variables表示腫瘤厚度範圍
data$skin.1 = 0
data$skin.2 = 0

for(i in 1:nrow(data)){
  if(data[i,"Tumor.Thickness"]<=5 & data[i,"Tumor.Thickness"]>2){
    data[i,"skin.1"] = 1
  }else if(data[i,"Tumor.Thickness"]>5){
    data[i,"skin.1"] = 1
    data[i,"skin.2"] = 1
  }
}

#根據性別將資料分開
male = data[data$Sex==1,]#男性
female = data[data$Sex==0,]#女性

###敘述統計
#樣本數
table(data$Sex)

#定義比較函數(使用t檢定)
compare_by_sex_with_t_test = function(data,name){
  
  male = data[data$Sex==1,]
  female = data[data$Sex==0,]
  
  male_mean = mean(male[,name])
  male_sd = sd(male[,name])
  female_mean = mean(female[,name])
  female_sd = sd(female[,name])
  t_result = t.test(male[,name],female[,name],equal = FALSE)
  
  result = list(
    varible = name,
    male_mean = male_mean,
    male_sd = male_sd,
    female_mean = female_mean,
    female_sd = female_sd,
    t_result = t_result
    
  )
  
  return(result)
  
}

#追蹤時間分布與檢定
compare_by_sex_with_t_test(data,"tilde.T")

#年齡分布與檢定
compare_by_sex_with_t_test(data,"Age")

#腫瘤厚度比率





