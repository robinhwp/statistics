

import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
# 데이터 읽기
survey = pd.read_csv("./data/survey.csv")
# 빈도수 구하기
edu_freq = pd.crosstab(index=survey.edu, columns='count')
edu_freq
# 케이스 라벨 지정하기
edu_freq.index = ["none", "elementary", "middle", "high", "college"]

# 막대그림 그리기
plt.bar(edu_freq.index, edu_freq["count"])
# 원그림 그리기
plt.pie(edu_freq["count"], labels=edu_freq.index) 

# (edu, sex) 분할표 구하기
edu_sex_tb = pd.crosstab(index=survey.edu, columns=survey.sex)
# 케이스 및 변수이름 지정하기
edu_sex_tb.index = ["none", "elementary", "middle", "high", "college"]
edu_sex_tb.columns = ["Male", "Female"]

# 겹친 막대그림 그리기
edu_sex_tb.plot.bar(stacked=True)

# 한화면에 여러개의 그래프 그리기
plt.figure()
plt.subplot(121) # 121 1행 2열 1번째
plt.bar(edu_freq.index, edu_freq["count"])
plt.subplot(122) # 121 1행 2열 2번째
plt.pie(edu_freq["count"], labels=edu_freq.index) 

import matplotlib.pyplot as plt
# 히스토그램 그리기
plt.hist(survey["salary"])

# 줄기-잎 그림 그리기
# pip3 install stemgraphic (in DOS prompt)
import stemgraphic
stemgraphic.stem_graphic(survey.salary, scale=50)

import seaborn as sns
sns.boxplot(x="sex", y="salary", data=survey)
