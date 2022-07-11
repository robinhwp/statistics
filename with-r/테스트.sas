


DATA data5_2;
	INPUT 국가분류 $ 주가 @@;
DATALINES;
F 120 K 135 K 170 F 139 K 114 F 163
K 165 K 161 F 147 F 150 F 175 K 145
K 147 K 102 F 235 F 157 K 129 K 129
F 144 K 165 F 161 K 173 F 111 K 145
RUN;

/* 줄기-잎 그림을 그리기위해 그래픽 옵션을 끈다. 예전 방식으로 출력하기위해 */
ODS LISTING;
ODS GRAPHICS OFF;

PROC UNIVARIATE DATA=data5_2 PLOT;
	VAR 주가;
	CLASS 국가분류;
RUN;	





> library(vcd)
> # 기술통계 확인
> summary(Arthritis)
ID          Treatment      Sex          Age          Improved 
Min.   : 1.00   Placebo:43   Female:59   Min.   :23.00   None  :42  
1st Qu.:21.75   Treated:41   Male  :25   1st Qu.:46.00   Some  :14  
Median :42.50                            Median :57.00   Marked:28  
Mean   :42.50                            Mean   :53.36              
3rd Qu.:63.25                            3rd Qu.:63.00              
Max.   :84.00                            Max.   :74.00      

임상시험 참여자 정보는 위약을 치료 받은 그룹은 43명이고 새로운 치료제를 투약한 그룹은 41명이다. 
성별로 보면 여자는 59명이고 남자는 25명이다.
치료후 차도가 거의 없었던 참여자수는 42명이고 약간 좋아진 참여자수는 14명 매우 좋아진 참여자는 28명이다.


> # 분할표 확인 
> (xtabs(~ Treatment + Improved, data = Arthritis))
Improved
Treatment None Some Marked
Placebo   29    7      7
Treated   13    7     21

위약을 투약한 그룹에서는 차도가 거의 없었던 참여자는 29명이고 약간 좋아졌거나 매우 좋아진 참여자는 14명인 반면에 
새로운 치료제를 투약한 그룹에서는 차도가 거의 없었던 참여자는 13명 약간 좋아졌거나 매우 좋아진 참여자는 36명이나 됐다.

mosaicplot(~Treatment+Improved+Sex, data = Arthritis, color=3:5)



library(ggplot2)
# 치료에 따른 향상을 성별로 분할하여 막대그래프로 그린다.
ggplot(data=Arthritis) +
  geom_bar(mapping=aes(x = Treatment, fill = Improved))+
  facet_wrap(~ Sex) 
# 남성과 여성의 참여자 수가 다르기 때문에 그래프를 화면에 채워서서 표시하면
ggplot(data=Arthritis) +
  geom_bar(mapping=aes(x = Treatment, fill = Improved), position="fill")+
  facet_wrap(~ Sex) 