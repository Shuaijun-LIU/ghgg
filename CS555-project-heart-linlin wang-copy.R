# 该项目代码提供了一个全面的数据分析流程，从数据加载、清理，到探索性数据分析、
# 执行多种统计测试（如相关性测试、T检验、方差分析、协方差分析、非参数检验和分位数回归）
# 及结果可视化。通过这些步骤，我有效地分析了心脏病数据集中的各种变量如年龄、性别、胸痛类型
# 和最大心率等因素如何影响心脏病的发生，同时可视化结果帮助更直观地理解数据特征和分析结果，
# 解决了如何从生物医学数据中提取有用信息以支持临床决策的问题。

# The project code provides a comprehensive data analysis process from data loading and cleaning, 
# to exploratory data analysis, performing multiple statistical tests 
# (e.g., correlation tests, t-tests, ANOVA, ANCOVA, non-parametric tests, 
# and quantile regression), and visualization of results. Through these steps, 
# I effectively analyzed how various variables such as age, gender, 
# type of chest pain, and maximum heart rate affect heart disease in a cardiology dataset, 
# while visualizing the results helped to more intuitively understand the data characteristics 
# and analytical results, and solved the problem of how to extract useful information 
# from biomedical data in order to support clinical decision-making.

# Install and load the necessary libraries
# 检查并安装需要的R包，确保所有功能都能正常使用
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("readr")) install.packages("readr")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("car")) install.packages("car")
if (!require("quantreg")) install.packages("quantreg")

library(tidyverse)
library(readr)
library(ggplot2)
library(car)
library(quantreg)

# Data loading with specific types for better control
# 加载数据并指定列的数据类型，以防自动类型推测出错
data <- read_csv("~/Downloads/heart.csv", col_types = list(
  Age = col_double(),
  Sex = col_factor(levels = c("M", "F")),
  ChestPainType = col_factor(),
  RestingBP = col_double(),
  Cholesterol = col_double(),
  FastingBS = col_double(),
  RestingECG = col_factor(),
  MaxHR = col_double(),
  ExerciseAngina = col_factor(),
  Oldpeak = col_double(),
  ST_Slope = col_factor(),
  HeartDisease = col_factor(levels = c("0", "1"))
))

# Data cleaning
# 数据清理：去重、去除缺失值和生物学上不可能的值
data_clean <- data %>%
  distinct() %>%  # 去除重复数据
  drop_na() %>%   # 删除含有NA值的行
  filter(Cholesterol > 0, RestingBP > 0) %>%  # 过滤掉胆固醇和静息血压为0的行
  mutate(
    Sex = factor(Sex),
    ChestPainType = factor(ChestPainType),
    RestingECG = factor(RestingECG),
    ExerciseAngina = factor(ExerciseAngina),
    ST_Slope = factor(ST_Slope),
    HeartDisease = factor(HeartDisease)
  )

# Exploratory data analysis
# 探索性数据分析，提供数据的概要统计和结构信息
summary(data_clean)
glimpse(data_clean)

# Correlation test - Age and MaxHR
# 计算年龄和最大心率之间的相关性，了解两者是否存在线性关系
cor_test <- cor.test(data_clean$Age, data_clean$MaxHR)
print(cor_test)  # 打印相关性测试结果

# Visualization - Relationship between age and maximum heart rate
# 可视化年龄与最大心率的关系，并按性别和胸痛类型分类
ggplot(data_clean, aes(x = Age, y = MaxHR, color = Sex)) +
  geom_point() +
  facet_wrap(~ChestPainType) +
  ggtitle("Age vs. MaxHR by Chest Pain Type and Sex")

# Visualization - Age distribution
# 可视化不同性别的年龄分布
ggplot(data_clean, aes(x = Age, fill = Sex)) +
  geom_histogram(bins = 20, alpha = 0.6) +
  facet_grid(Sex ~ .) +
  ggtitle("Distribution of Age by Sex")

# Visualization - Heart Rate Distribution by Heart Disease status
# 根据心脏病状态可视化心率分布
ggplot(data_clean, aes(x = MaxHR, fill = HeartDisease)) +
  geom_density(alpha = 0.5) +
  ggtitle("Heart Rate Distribution by Heart Disease Status")

# Visualization - Boxplot of MaxHR by ChestPainType
# 使用箱线图可视化不同胸病类型下的最大心率分布
ggplot(data_clean, aes(x = ChestPainType, y = MaxHR, fill = ChestPainType)) +
  geom_boxplot() +
  ggtitle("MaxHR by Chest Pain Type")


# Visualization - Gender and cholesterol levels
# 使用盒形图可视化不同性别的胆固醇水平分布
ggplot(data_clean, aes(x = Sex, y = Cholesterol, fill = Sex)) +
  geom_boxplot() +
  ggtitle("Cholesterol Levels by Sex")


# T test - sex and cholesterol level
# T检验，比较不同性别的胆固醇水平是否存在显著差异
t_test_cholesterol <- t.test(Cholesterol ~ Sex, data = data_clean)
print(t_test_cholesterol)  # 打印T检验结果

# Two-sample tests for proportions - HeartDisease by Sex
# 使用比例检验分析不同性别之间心脏病患病率的差异
prop_test <- prop.test(table(data_clean$Sex, data_clean$HeartDisease))
print(prop_test)  # 打印比例检验结果

# Logistic regression analysis
# 逻辑回归分析，评估年龄、性别、胸痛类型和最大心率对心脏病风险的影响
logistic_model <- glm(HeartDisease ~ Age + Sex + ChestPainType + MaxHR, data = data_clean, family = "binomial")
summary(logistic_model)  # 输出逻辑回归模型的统计摘要

# Multiple linear regression
# 多重线性回归分析，探索年龄、性别和胸痛类型对最大心率的影响
lm_model <- lm(MaxHR ~ Age * Sex * ChestPainType, data = data_clean)
summary(lm_model)  # 输出模型的详细统计摘要

# ANOVA test - Compare MaxHR across different ChestPainTypes
# 使用方差分析比较不同胸痛类型的最大心率差异
anova_model <- aov(MaxHR ~ ChestPainType, data = data_clean)
summary(anova_model)  # 输出方差分析结果

# ANCOVA test - Adjust for Age and Sex in the analysis of ChestPainType on MaxHR
# 协方差分析，控制年龄和性别变量，比较不同胸痛类型对最大心率的影响
ancova_model <- aov(MaxHR ~ ChestPainType + Age + Sex, data = data_clean)
summary(ancova_model)  # 输出协方差分析结果

# Visualization for ANCOVA results - MaxHR by ChestPainType adjusted for Age and Sex
# 可视化协方差分析结果：考虑年龄和性别后，不同胸痛类型对最大心率的影响
ggplot(data_clean, aes(x = ChestPainType, y = MaxHR, color = Age)) +
  geom_point(aes(shape = Sex)) +
  geom_smooth(method = "lm", formula = y ~ x + Age + Sex) +
  ggtitle("Adjusted MaxHR by Chest Pain Type, Age, and Sex") +
  labs(color = "Age", shape = "Sex")


# Kruskal-Wallis test - Non-parametric test for comparing MaxHR across different ChestPainTypes
# Kruskal-Wallis非参数检验，比较不同胸痛类型下最大心率的分布是否有显著差异
kruskal_test <- kruskal.test(MaxHR ~ ChestPainType, data = data_clean)
print(kruskal_test)  # 打印非参数检验结果

# Visualization for Kruskal-Wallis Test results - MaxHR distributions across ChestPainTypes
# 可视化Kruskal-Wallis非参数检验结果：不同胸痛类型下最大心率的分布
ggplot(data_clean, aes(x = ChestPainType, y = MaxHR, fill = ChestPainType)) +
  geom_violin() +
  stat_summary(fun = "median", geom = "point", color = "black", size = 3) +
  ggtitle("MaxHR Distributions by Chest Pain Type (Kruskal-Wallis Test)")


# Quantile regression - Analyze the relationship between HeartDisease and other factors at different quantiles
# 分位数回归，分析心脏病与其他变量在不同分位数下的关系
quantile_model <- rq(HeartDisease ~ Age + Sex + ChestPainType + MaxHR, tau = c(0.25, 0.5, 0.75), data = data_clean)
summary(quantile_model)  # 输出分位数回归模型结果

# 获取分位数回归的摘要并计算标准误差
quantile_summary <- summary(quantile_model, se = "boot")

# 创建一个适用于绘图的数据帧
coefficients_data <- do.call(rbind, lapply(quantile_summary, function(x) {
  data.frame(
    tau = x$tau,
    variable = rownames(x$coefficients),
    coefficient = x$coefficients[,1],
    lowerCI = x$coefficients[,2],
    upperCI = x$coefficients[,3]
  )
}))

# 可视化分位数回归系数结果
ggplot(coefficients_data, aes(x = factor(tau), y = coefficient, color = variable)) +
  geom_line(aes(group = variable), size = 1.2) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = lowerCI, ymax = upperCI), width = 0.02, size = 0.5) +
  facet_wrap(~ variable, scales = "free_y") +
  labs(x = "Quantile", y = "Coefficient", color = "Predictor") +
  ggtitle("Quantile Regression Coefficients for Predicting Heart Disease")


# Save the cleaned data
# 保存清洗后的数据为CSV文件
write_csv(data_clean, "cleaned_heart_data.csv")
