


import numpy as np
import pandas as pd
import matplotlib.pyplot as plt

# 자료읽기
ex2_4 = pd.read_csv("./exdata/ex2-4.csv", header = 0, index_col = 0)
ex2_4.head()

# 변수이름 확인하기
ex2_4.columns

# 기술통계량 구하기 - 소수점 이하 2자리 반올림 표시
round(ex2_4.describe(), 2)

##### ② 상관계수행렬 및 산점도행렬 보기
# 상관계수 행렬
ex2_4.corr()

# 산점도 행렬(seaborn 사용)
import seaborn as sns
sns.pairplot(ex2_4)

##### ③ 주성분분석 실행하기
from sklearn.decomposition import PCA
# 주성분분석 - 주성분 수를 3으로 함.
pca = PCA(n_components=4)
pca_ex2_4 = pca.fit_transform(ex2_4)

# 주성분 분산
pca.explained_variance_
# 주성분 표준편차
np.sqrt(pca.explained_variance_)

# 주성분분산비율
pca.explained_variance_ratio_

# 주성분계수 - 소숫점 이하 3자리 표시
np.round(pca.components_, 3)

##### ④ 스크리 그림 및 주성분 계수
# 스크리 그림 그리기
import matplotlib.pyplot as plt
plt.clf()
plt.scatter(range(1,pca.n_components_+1), pca.explained_variance_)
plt.plot(range(1,pca.n_components_+1), pca.explained_variance_)
plt.title('Scree plot')
plt.xlabel('Number of components')
plt.ylabel('Explained variance ratio')
plt.grid()
plt.show()
