getwd()
library(readxl)
library(ggplot2)

# 从文件夹data中读取所有xlsx文件,参数recursive参数为TRUE表示递归读取子文件夹
file_list <- list.files(path = "data/001965", pattern = "*.xlsx", 
                        recursive = TRUE, full.names = TRUE)
# 载入这个文件夹中所有xlsx文件
df <- lapply(file_list, read_excel)
# 按照行合并所有数据
cb_df <-  bind_rows(df)
#输出cb_df的列名
cb_df |> colnames()
# 按照交易日期从近到远排列
cb_df <- 
  cb_df |> arrange(desc(交易日期)) |> 
  rename(成交量 = '成交量(万股)',
         涨跌幅 = '涨跌幅（%）',
         成交金额= '成交金额(万元)')

ggplot(data = cb_df,
       aes(x = 交易日期, y = 成交量)) +
  geom_bar(stat = "identity") 


file.name <- list.files(path = "D:\\100study\\110_UM\\115_实验室财务\\报销项目",
                          pattern = "*.xlsx", recursive = TRUE, full.names = TRUE)
df_list = lapply(file.name, read_excel)
