# 라이브러리
library(readr)
library(stringr) # 문자열 처리 패키지
library(doBy)
library(ggplot2)
library(scales)
library(RColorBrewer)
library(corrplot)
library(doBy)
library(dplyr) # 전처리
library(randomForest)
library(gridExtra)


# 데이터 가져오기
train <- read_csv('C:/R/my-first-github/teamproject/data/train.csv')
test <- read_csv('C:/R/my-first-github/teamproject/data/test.csv')
full <- bind_rows(train, test)

# 데이터 유형 변경
full <- full %>%
  mutate(Survived = factor(Survived),
         Pclass   = factor(Pclass, ordered = T),
         Name     = factor(Name),
         Sex      = factor(Sex),
         Embarked = factor(Embarked))

str(full)

# 데이터 확인
head(full)
str(full)
summary(full)
sapply(train, function(x) length(unique(x)))


# 결측치 확인
colSums(is.na(full))


# 성별, 나이, 티켓클래스, 생존유무에 따른 EDA
## 성별
table(full$Sex)
full %>% group_by(Survived, Sex) %>% summarise(freq = n())
prop.table(table(full$Sex,full$Survived),1)
# => 여자의 생존확률이 높다.

# 성별 막대그래프
sex.p1 <- full %>% 
  dplyr::group_by(Sex) %>% 
  summarize(N = n()) %>% 
  ggplot(aes(Sex, N)) +
  geom_col() +
  geom_text(aes(label = N), size = 5, vjust = 1.2, color = "#FFFFFF") + 
  ggtitle("탑승객 성별") +
  labs(x = "성별", y = "인원수")

# 성별에 따른 생존률 막대그래프
sex.p2 <- full%>%
  filter(!is.na(Survived)) %>%
  ggplot(aes(factor(Sex), fill = factor(Survived))) +
  scale_color_discrete(limits = c("사망","생존")) +
  geom_bar(position = "fill") + 
  scale_y_continuous(labels = percent) +
  scale_fill_brewer(palette = "Set3") +
  ggtitle("성별별 생존율") + 
  labs(x = "성별", y = "생존율")

grid.arrange(sex.p1,sex.p2,ncol=2)


## 티켓 클래스
table(full$Pclass)
prop.table(table(full$Pclass,full$Survived),1)

# Pclass 막대그래프
pclass.p1 <- full %>% 
  dplyr::group_by(Pclass) %>% 
  summarize(N = n()) %>% 
  ggplot(aes(Pclass, N)) +
  geom_col() +
  geom_text(aes(label = N), size = 5, vjust = 1.2, color = "#FFFFFF") + 
  ggtitle("티켓클래스") +
  labs(x = "클래스", y = "티켓수")

# Pclass에 따른 생존률 막대그래프
pclass.p2 <- full%>%
  filter(!is.na(Survived)) %>%
  ggplot(aes(factor(Pclass), fill = factor(Survived))) +
  geom_bar(position = "fill") + 
  scale_fill_brewer(palette = "Set3") +  
  ggtitle("클래스별 생존율") + 
  labs(x = "클래스", y = "생존율")

grid.arrange(pclass.p1,pclass.p2,ncol=2)  

## 나이
hist(full$Age)

# 나이 분포 히스토그램
age.p1 <- full %>% 
  ggplot(aes(Age)) +
  geom_histogram(col = "white",
                 fill   = "tomato") +
  ggtitle("탑승객의 성별 히스토그램") +
  theme(plot.title = element_text(face = "bold", 
                                  hjust = 0.5,
                                  size = 15, color = "tomato"))

# 나이에 따른 생존율 밀도그래프
age.p2 <- full %>% 
  filter(!is.na(Survived)) %>%
  ggplot(aes(Age, fill = Survived)) + 
  geom_density(alpha = .5) +
  ggtitle("탑승객의 나이 분포") + 
  theme(plot.title = element_text(face = "bold", hjust = 0.5,
                                  size = 15, color = "tomato"))

grid.arrange(age.p1,age.p2,ncol=2)


# 나이 결측치 전처리를 위해 파생변수 title 생성

Title <- full$Name
Title <- gsub("^.*, (.*?)\\..*$", "\\1", Title) # 정규표현식
full$Title <- Title
unique(full$Title)
descr::CrossTable(full$Title)

# 범주 단순화 
full <- full %>%
  
  mutate(Title = ifelse(Title %in% c("Mlle", "Ms", "Lady", "Dona"), "Miss", Title),
         Title = ifelse(Title == "Mme", "Mrs", Title),
         Title = ifelse(Title %in% c("Capt", "Col", "Major", "Dr", "Rev", "Don",
                                     "Sir", "the Countess", "Jonkheer"), "Officer", Title),
         Title = factor(Title))

# 각 범주별 빈도수, 비율 확인 
descr::CrossTable(full$Title)

## 성별과 나이의 상관성 파악
age.sex <- full %>% 
  ggplot(aes(Age, fill = Sex)) + 
  geom_density(alpha = .5) +  
  ggtitle("Titanic passengers Age density plot") + 
  theme(plot.title = element_text(face = "bold", hjust = 0.5,
                                  size = 15, color = "darkblue"))
age.sex


# Logistic Regression


titan_glm <- glm(Survived ~ Sex+Age+Pclass, data = train, family = 'binomial')
summary(titan_glm)
