setwd("C:/Users/vera/PycharmProjects/kaggle-titanic")
source(paste0(getwd(),'/Mode.r'))

#________________________SAMPLE____________________________

test <- read.csv("test.csv", header = T);
train <- read.csv("train.csv", header = T);

test_mod <- test;
train_mod <- train;

test_mod$Survived <- NA;

train_mod$smp <- "train";
test_mod$smp <- "test";

all_data <- rbind(train_mod, test_mod);
all_data_backup <- rbind(train_mod, test_mod);



#________________________MISSING DATA 1____________________________

nrow(all_data[!is.na(all_data$Fare) == F,])
all_data$Fare[1044] <- median(all_data$Fare, na.rm = T)

all_data[c(62,830),]$Embarked <- "S" # Mode for 1 Pclass

all_data$Embarked <- as.character(all_data$Embarked)
all_data$Embarked <- as.factor(all_data$Embarked)

#________________________Corrections____________________________


# I assume mistake in data
all_data[c(69, 1106),]$SibSp <- 0
all_data[c(69, 1106),]$Parch <- 0

all_data[all_data$Name == "Abbott, Mrs. Stanton (Rosa Hunt)", ]$SibSp <- 0
all_data[all_data$Name == "Abbott, Mrs. Stanton (Rosa Hunt)", ]$Parch <- 2
all_data[all_data$Name == "Abbott, Master. Eugene Joseph", ]$SibSp <- 1
all_data[all_data$Name == "Abbott, Master. Eugene Joseph", ]$Parch <- 1

#Hansen family, suppose they ignored id 624 and others replationship, maybe he is nephew 
all_data[all_data$Name == "Hansen, Mr. Henry Damsgaard", ]$SibSp <- 2
all_data[all_data$Name == "Hansen, Mr. Henrik Juul", ]$SibSp <- 2
all_data[all_data$Name == "Hansen, Mr. Claus Peter", ]$SibSp <- 3

all_data$Ticket <- as.character(all_data$Ticket);

#Elias family
all_data[all_data$Ticket == "2660" | 
           all_data$Ticket == "2674" |
           all_data$Ticket == "2675" |
           all_data$Ticket == "2689" |
           all_data$Ticket == "2690" | 
           all_data$Ticket == "2695",]$Ticket <- "2660"
#Persson family
all_data[all_data$Ticket == "347083",]$Ticket <- "347054"

all_data$Ticket <- as.factor(all_data$Ticket);


#________________________FEATURES ENGENEERING 1____________________________

#______________________________ROOM SECTOR____________________________
all_data$RoomSector <-
  substr(gsub(" ","", gsub("[-^0-9]", "", all_data$Cabin)), 
         1, 1) # переписать аналогично выше без substr

mode_1 <-
  Mode(all_data[all_data$RoomSector != "" &
                  all_data$Pclass == 1,]$RoomSector)
mode_2 <-
  Mode(all_data[all_data$RoomSector != "" &
                  all_data$Pclass == 2,]$RoomSector)
mode_3 <-
  Mode(all_data[all_data$RoomSector != "" &
                  all_data$Pclass == 3,]$RoomSector)

all_data[all_data$RoomSector == "" &
           all_data$Pclass == 1,]$RoomSector <- mode_1
all_data[all_data$RoomSector == "" &
           all_data$Pclass == 2,]$RoomSector <- mode_2
all_data[all_data$RoomSector == "" &
           all_data$Pclass == 3,]$RoomSector <- mode_3

all_data[all_data$RoomSector == "T",]$RoomSector <- "G"

#______________________________ROOM GROUP____________________________

all_data$RoomNumberRaw <-
  as.numeric(gsub("[^0-9]", "", gsub(" .*$", "", all_data$Cabin)))

#all_data[is.na(all_data$RoomNumberRaw) == T,]$RoomNumberRaw <- median(all_data[is.na(all_data$RoomNumberRaw) == F,]$RoomNumberRaw);

all_data$RoomBin <- "bnull"

all_data[all_data$RoomNumberRaw < 25 & is.na(all_data$RoomNumberRaw) == F ,]$RoomBin <- "b25"

all_data[all_data$RoomNumberRaw >= 25 & all_data$RoomNumberRaw < 50 &  is.na(all_data$RoomNumberRaw) == F ,]$RoomBin <- "b50"
all_data[all_data$RoomNumberRaw >= 50 & all_data$RoomNumberRaw < 75 &  is.na(all_data$RoomNumberRaw) == F ,]$RoomBin <- "b75"
all_data[all_data$RoomNumberRaw >= 75 & all_data$RoomNumberRaw < 100 &  is.na(all_data$RoomNumberRaw) == F ,]$RoomBin <- "b100"
all_data[all_data$RoomNumberRaw >= 100 &  is.na(all_data$RoomNumberRaw) == F ,]$RoomBin <- "b150"

#______________________________TITUL____________________________

#all_data$Titul <- as.character(all_data$Titul);

all_data$Titul <- "Other";

all_data$Titul[grep("Mrs\\.|Dona\\.", all_data$Name, value = F)] <- "Mrs."; #Dona. 
all_data$Titul[grep("Mr\\.|Don\\.|Sir\\.|Jonkheer|Capt\\.|Major\\.|Col\\.", all_data$Name  , value = F)] <- "Mr.";
all_data$Titul[grep("Miss\\.|Mlle\\.|Mme\\.|Countess|Ms\\.|Lady\\.", all_data$Name, value = F)] <- "Miss."; 
all_data$Titul[grep("Master", all_data$Name, value = F)] <-
  "Master.";
all_data$Titul[grep("Dr\\.", all_data$Name, value = F)] <- "Dr.";

all_data$Titul[grep("Rev\\.", all_data$Name, value = F)] <- "Rev."; #1041, 1056 священники

all_data[797,]$Titul <- "Mrs.";

all_data$Titul <- as.factor(all_data$Titul);

table(all_data$Titul); # check 6 level

#______________________________FAMILY SIZE____________________________

all_data$FamilySize <- all_data$SibSp + all_data$Parch +1;

#______________________________COMPANY SIZE____________________________

sub <-
  aggregate(
    all_data$PassengerId / all_data$PassengerId, by = list(Ticket = all_data$Ticket), FUN =
      sum
  );

mergedf <- merge(all_data, sub, by.x = "Ticket", by.y = "Ticket", sort = F);

all_data$CompanySize <-
  mergedf[order(mergedf$PassengerId), ]$x;


#______________________________FARE PER PERSON____________________________

all_data$FarePerPerson <- all_data$Fare / all_data$CompanySize;

#______________________________SURNAME____________________________

all_data$Surname <-
  gsub(", .*$", "", all_data$Name)


#______________________________FAMILY SURVIVAL RATE____________________________
all_data$FamiliSurvRate <-NA

subFam <-
  aggregate(all_data[all_data$CompanySize >= 3 &
                       all_data$smp == "train",]$Survived, by = list(Ticket = all_data[all_data$CompanySize >=
                                                                                         3 & all_data$smp == "train",]$Ticket), FUN =
              mean)

mergedf <- merge(all_data[all_data$CompanySize >= 3,], subFam, by.x = "Ticket", by.y = "Ticket", sort = F)

mergedf_all <- merge(all_data, mergedf, by.x = "PassengerId", by.y = "PassengerId", sort = F, all.x= T)

all_data$FamiliSurvRate <-
  mergedf_all[order(mergedf_all$PassengerId), ]$x


#______________________________TOWN____________________________

all_data$Town <- "Other";
all_data[grep("^SOTON", toupper(all_data$Ticket)),]$Town <- "SOTON";  
all_data[grep("^STON", toupper(all_data$Ticket)),]$Town <- "STON"; 
all_data[grep("^PC", toupper(all_data$Ticket)),]$Town <- "PC"; 
all_data[grep("PARIS", toupper(all_data$Ticket)),]$Town <- "PARIS";

all_data$Town <- as.factor(all_data$Town);

#______________________________ IS MOTHER prev___________________________

all_data$IsMotherPrev <- "no";
all_data[all_data$Sex == "female" & all_data$Parch != 0 & all_data$Titul == "Mrs.", ]$IsMotherPrev <- "yes";
all_data$IsMotherPrev <- as.factor(all_data$IsMotherPrev);


#______________________________ IS FATHER prev___________________________

all_data$IsFatherPrev <- "no";
all_data[all_data$Sex == "male" &
           all_data$Parch != 0 &
           (all_data$Titul == "Mr." |
              all_data$Titul == "Dr." |
              all_data$Titul == "Rev."),]$IsFatherPrev <- "yes";
all_data$IsFatherPrev <- as.factor(all_data$IsFatherPrev);

#______________________________AGE____________________________

all_data$Age <- all_data_backup$Age;
nrow(all_data[!is.na(all_data$Age) == F,]);
all_data$AgeInit <- all_data$Age;

library(rpart);

agerpart <- rpart(
  Age ~ Pclass + Sex + SibSp + Parch +
    Embarked + RoomSector +
    Titul + FamilySize + CompanySize + FarePerPerson + Town + IsFatherPrev + IsMotherPrev,
  data = all_data, method = "anova", control = rpart.control(minsplit = 2, cp = 0.0074
  ))

plotcp(agerpart)

# install.packages("rattle");
# install.packages("rpart.plot");
# install.packages("RColorBrewer");
# 
library(rattle)
library(rpart.plot)
library(RColorBrewer)

print(agerpart)

fancyRpartPlot(agerpart)

all_data[!is.na(all_data$Age) == F,]$Age <-
  predict(agerpart, all_data[!is.na(all_data$Age) == F,], type = "vector")

# мб добавить отдельно моделирование возраста с использованием survived для тренировочной выборки...
# попробовало, дерево получилось немного более другим, но глубина похожая и признак survived не подхватитлся

#______________________________ IS MOTHER___________________________

subMot <-
  all_data[ all_data$Parch != 0 &
              #all_data$SibSp != 0 &
              all_data$Sex == "female" &
              (
                all_data$Titul == "Mrs." 
              ) & all_data$Age >= 20,]

subMott <- subMot[order(subMot$Ticket,-subMot$Age),]

subMottt <- aggregate(subMott, list(subMott$Ticket), FUN = head, 1)

mergeMot <- merge(all_data, subMottt, by.x = "Ticket", by.y = "Ticket", sort = F, all.x= T);

mergeMot[mergeMot$PassengerId.y != mergeMot$PassengerId.x & is.na(mergeMot$PassengerId.y) ==F ,]$PassengerId.y <- 0

all_data$IsMother1 <-
  mergeMot[order(mergeMot$PassengerId.x), ]$PassengerId.y

all_data[is.na(all_data$IsMother1) == F & all_data$IsMother1 != 0, ]$IsMother1 <- "yes"
all_data[is.na(all_data$IsMother1) != F | all_data$IsMother1 == 0, ]$IsMother1 <- "no"

#______________________________IS FATHER____________________________

subMan <-
  all_data[ all_data$Parch != 0 &
              #all_data$SibSp != 0 &
              all_data$Sex == "male" &
              (
                all_data$Titul == "Dr." |
                  all_data$Titul == "Mr." | 
                  all_data$Titul == "Rev."
              ) & all_data$Age >= 20,]

subMan <- subMan[order(subMan$Surname,-subMan$Age),]

subMan1 <- aggregate(subMan, list(subMan$Surname), FUN = head, 1)

mergeMan <- merge(all_data, subMan1, by.x = "Surname", by.y = "Surname", sort = F, all.x= T)

mergeMan[mergeMan$PassengerId.y != mergeMan$PassengerId.x & is.na(mergeMan$PassengerId.y) ==F ,]$PassengerId.y <- 0

all_data$IsFather <-
  mergeMan[order(mergeMan$PassengerId.x), ]$PassengerId.y

all_data[is.na(all_data$IsFather) == F & all_data$IsFather != 0, ]$IsFather <- "yes"
all_data[is.na(all_data$IsFather) != F | all_data$IsFather == 0, ]$IsFather <- "no"

#Williams


#______________________________ Survival rate for singles______________________________ 

survrpart <- rpart(
  FamiliSurvRate ~ Pclass + Sex + Age + SibSp + Parch +
    Embarked + RoomSector + RoomBin +
    Titul + FamilySize + CompanySize + FarePerPerson + IsMother1 + IsFather + Town + IsFather,
  data = all_data, method = "anova", control = rpart.control(minsplit = 2, cp = 0.012)
)

plotcp(survrpart)

fancyRpartPlot(survrpart)

all_data[!is.na(all_data$FamiliSurvRate) == F,]$FamiliSurvRate <-
  predict(survrpart, all_data[!is.na(all_data$FamiliSurvRate) == F,], type = "vector")


#______________________________ MAIDEN NAME______________________________   

all_data$MaidenName <- NA

all_data[grep("^([^(]*\\(([^\"][^)]* )?([^\") ]+)\\).*)$", all_data$Name),]$MaidenName <-
  gsub("^([^(]*\\(([^\"][^)]* )?([^\") ]+)\\).*)$", "\\3", all_data[grep("^([^(]*\\(([^\"][^)]* )?([^\") ]+)\\).*)$", all_data$Name),]$Name)


subMaiden <- all_data[is.na(all_data$MaidenName)==F 
                      & all_data$Sex == "female" 
                      & all_data$MaidenName != "Elias" 
                      & all_data$MaidenName != "Persson"
                      & all_data$MaidenName != "Wilson"
                      & all_data$MaidenName != "Dyker"
                      , ]


#install.packages("sqldf")

library(sqldf)

mergeMaiden <- sqldf(
  "
  SELECT L.Survived, L.PassengerId as PassengerId, L.Name, L.Surname, L.MaidenName, L.SibSp, L.Parch, L.Ticket, L.Age, L.Sex, r.MaidenName MaidenName_mod, r.Name Name_mod, r.Surname Surname_mod,
  r.Survived Survived_mod, r.SibSp SibSp_mod, r.Parch Parch_mod, r.Ticket Ticket_mod, r.Age Age_mod, r.Sex Sex_mod
  FROM 
  (
  select 
  PassengerId   ,
  Survived       ,
  Pclass         ,
  Name           ,
  Sex            ,
  Age            ,
  SibSp          ,
  Parch          ,
  Ticket         ,
  Fare           ,
  Cabin          ,
  Embarked       ,
  smp            ,
  RoomSector     ,
  RoomNumberRaw  ,
  RoomBin        ,
  Titul          ,
  FamilySize     ,
  CompanySize    ,
  FarePerPerson  ,
  case when MaidenName = 'Needs' then 'Hocking' else Surname end as  Surname,
  FamiliSurvRate ,
  Town           ,
  IsMotherPrev   ,
  IsFatherPrev   ,
  case when MaidenName = 'Needs' then null else MaidenName end as MaidenName
  from
  all_data 
  ) as L
  LEFT JOIN ( select m.* from  subMaiden m where  m.MaidenName in (select distinct Surname from all_data)  ) as r
  ON
  L.Surname = r.MaidenName
AND L.Pclass = r.Pclass
  and (L.SibSp + L.Parch >= 1)
  and (r.SibSp + r.Parch >= 1)
  and (L.age < 10 and L.Surname = r.Surname or L.age > 10)
  and (case when L.MaidenName = 'Hocking' then r.Surname <> 'Quick' else  L.Surname = r.MaidenName end)
  --or L.Name = r.Name
  or L.Surname = r.Surname 
and (L.SibSp + L.Parch >=1) 
and (r.SibSp + r.Parch >=1) 
and abs(L.Ticket - r.Ticket) <5
  
  ORDER BY L.PassengerId"
)


sqldf(
  "
  SELECT PassengerId, count(*)
  FROM mergeMaiden as L
  group by PassengerId
  having count(*) > 1
  "
)

#______________________________family code____________________________

all_data$FamilyCode <- mergeMaiden$Name_mod
all_data[is.na(all_data$FamilyCode) == T,]$FamilyCode <- all_data[is.na(all_data$FamilyCode) == T,]$Ticket

#______________________________COMPANY SIZE new____________________________

sub <-
  aggregate(
    all_data$PassengerId / all_data$PassengerId, by = list(FamilyCode = all_data$FamilyCode), FUN =
      sum
  );

mergedf <- merge(all_data, sub, by.x = "FamilyCode", by.y = "FamilyCode", sort = F);

all_data$CompanySizeNew <-
  mergedf[order(mergedf$PassengerId), ]$x;


# #______________________________FAMILY SURVIVAL RATE new____________________________



all_data$FamiliSurvRateNew <-NA
# 
# all_data$SurvivedMod <- NA
# 
# all_data[is.na(all_data$Survived) == F & all_data$Survived == "yes",]$SurvivedMod <- 1
# all_data[is.na(all_data$Survived) == F & all_data$Survived == "no",]$SurvivedMod <- 0
# # 
# all_data[is.na(all_data$Survived) == T,]$SurvivedMod <- 0

subFam <-
  aggregate(all_data[all_data$smp == "train",]$Survived, by = list(FamilyCode = all_data[all_data$smp == "train",]$FamilyCode), FUN =
              mean)

mergedf <- merge(all_data, subFam, by.x = "FamilyCode", by.y = "FamilyCode", sort = F)

mergedf_all <- merge(all_data, mergedf, by.x = "PassengerId", by.y = "PassengerId", sort = F, all.x= T)

all_data$FamiliSurvRateNew <-
  mergedf_all[order(mergedf_all$PassengerId), ]$x


#______________________________ Survival rate for singles new______________________________ 

set.seed(111)
survNFs <-
  randomForest(
    FamiliSurvRateNew ~ Pclass + Sex + Age + SibSp + Parch +
      Embarked + RoomSector + RoomBin +
      Titul + FamilySize + CompanySizeNew + FarePerPerson + IsMother1 + IsFather + Town + IsFather,
    data = all_data[all_data$CompanySizeNew == 1,], importance = T , ntree = 10000
  );


print(survNFs)


varImpPlot(survNFs);

# 
# 
# survrpart <- rpart(
#   FamiliSurvRateNew ~ Pclass + Sex + Age + SibSp + Parch +
#     Embarked + RoomSector + RoomBin +
#     Titul + FamilySize + CompanySizeNew + FarePerPerson + IsMother1 + IsFather + Town + IsFather,
#   data = all_data[all_data$CompanySizeNew == 1,], method = "anova", control = rpart.control(minsplit = 2, cp = 0.023) #0.0061
# )
# 
# plotcp(survrpart)
# 
# fancyRpartPlot(survrpart)

all_data[!is.na(all_data$FamiliSurvRateNew) == F & all_data$CompanySizeNew == 1,]$FamiliSurvRateNew <-
  predict(survNFs, all_data[!is.na(all_data$FamiliSurvRateNew) == F & all_data$CompanySizeNew == 1,], type = "response")

#_____ 
set.seed(111)
survNFs <-
  randomForest(
    FamiliSurvRateNew ~ Pclass + Sex + Age + SibSp + Parch +
      Embarked + RoomSector + RoomBin +
      Titul + FamilySize + CompanySizeNew + FarePerPerson + IsMother1 + IsFather + Town + IsFather,
    data = all_data[all_data$CompanySizeNew != 1,], importance = T , ntree = 10000
  );


print(survNFs)


varImpPlot(survNFs);

all_data[!is.na(all_data$FamiliSurvRateNew) == F & all_data$CompanySizeNew != 1,]$FamiliSurvRateNew <-
  predict(survNFs, all_data[!is.na(all_data$FamiliSurvRateNew) == F & all_data$CompanySizeNew != 1,], type = "response")


#______________________________ Рефактор______________________________

all_data[all_data$Survived == "0" & all_data$smp == "train",]$Survived <- "no";
all_data[all_data$Survived == "1" & all_data$smp == "train",]$Survived <- "yes";

all_data$Survived <- as.character(all_data$Survived);
all_data$Sex <- as.character(all_data$Sex);
all_data$Embarked <- as.character(all_data$Embarked);
all_data$RoomSector <-  as.character(all_data$RoomSector);
all_data$Titul <- as.character(all_data$Titul);
all_data$IsMother <- as.character(all_data$IsMother1);
all_data$IsFather <- as.character(all_data$IsFather);
all_data$Town <- as.character(all_data$Town);
all_data$RoomBin <- as.character(all_data$RoomBin);


all_data$Survived <- as.factor(all_data$Survived);
all_data$Sex <- as.factor(all_data$Sex);
all_data$Embarked <- as.factor(all_data$Embarked);
all_data$RoomSector <-  as.factor(all_data$RoomSector);
all_data$Titul <- as.factor(all_data$Titul);
all_data$IsMother1 <- as.factor(all_data$IsMother1);
all_data$IsFather <- as.factor(all_data$IsFather);
all_data$Town <- as.factor(all_data$Town);
all_data$RoomBin <- as.factor(all_data$RoomBin);

test <- all_data[all_data$smp == "test",];
train <- all_data[all_data$smp == "train",];
# 
# train1 <- train[1:600,]
# train2 <- train[601:891,]

#________________________RANDOM FOREST____________________________

#install.packages("randomForest");
library(randomForest);

set.seed(111);

survNF <-
  randomForest(
    Survived ~ Pclass + Sex + Age + SibSp + Parch +
      Embarked + RoomSector + RoomBin +
      Titul + FamilySize + CompanySizeNew + FarePerPerson + FamiliSurvRateNew + IsMother1 + IsFather + Town , data = train, importance = T , ntree = 10000
  );

survNF;

print(survNF)


varImpPlot(survNF);

res <- predict(survNF, test, type = "class");

result <-
  cbind(test$PassengerId, as.numeric(res) - 1); #переписать тут нормально
result <- as.data.frame(result);
names(result)[1] <- "PassengerId";
names(result)[2] <- "Survived";

write.csv(result, "titanic_rf09232305.csv", row.names = FALSE);

output <- read.csv("titanic_rf09230009.csv");

#________________________DESICION TREE____________________________

survDT <-
  rpart(
    Survived ~ Pclass + Sex + Age + SibSp + Parch +
      Embarked + RoomSector + RoomNumber +
      Titul + FamilySize + CompanySize + FarePerPerson, data = train,
    control = rpart.control(minsplit = 2, cp = 0.005)
  );

resDT <- predict(survDT, test, type = "class");

resultDT <-
  cbind(test$PassengerId, as.numeric(resDT) - 1); #переписать тут нормально
resultDT <- as.data.frame(resultDT);
names(resultDT)[1] <- "PassengerId";
names(resultDT)[2] <- "Survived";

write.csv(resultDT, "titanic_dt.csv", row.names = FALSE);
outputDT <- read.csv("titanic_dt.csv");

#________________________GBM____________________________

train_gbm <- train;


# переписать эту жесть
train_gbm$Survived <- as.numeric(train_gbm$Survived) - 1;

train_gbm[train_gbm$Survived == 0,]$Survived <- "no";
train_gbm[train_gbm$Survived == 1,]$Survived <- "yes";

train_gbm$Survived <- as.factor(train_gbm$Survived);

train_gbm_cut <- train_gbm[,-13][,-4][,-1][,-7][,-8][,-15];

train_gbm_cut$Survived <- as.character(train_gbm_cut$Survived);

levels <- unique(train_gbm_cut$Survived);

train_gbm_cut$Survived <-
  factor(train_gbm_cut$Survived, labels = make.names(levels));


train_gbm$Embarked <- as.character(train_gbm$Embarked);
train_gbm$Embarked <- as.factor(train_gbm$Embarked);


# проверяем, что не нагенерировано левых имен

make.names(levels(train_gbm_cut$Survived));
levels(train_gbm_cut$Survived)
make.names(levels(train_gbm_cut$Sex));
levels(train_gbm_cut$Sex)
make.names(levels(train_gbm_cut$Embarked));
levels(train_gbm_cut$Embarked)
make.names(levels(train_gbm_cut$RoomSector));
levels(train_gbm_cut$RoomSector)
make.names(levels(train_gbm_cut$Titul));
levels(train_gbm_cut$Titul);
make.names(levels(train_gbm_cut$IsMother));
levels(train_gbm_cut$IsMother);

install.packages("caret");
install.packages("e1071");
install.packages("gbm");
library(caret);

set.seed(111);

controll <-
  trainControl(
    method = 'repeatedcv', number = 20, repeats = 20, returnResamp = 'none',  classProbs = TRUE
  )

survGBM <-
  train(
    Survived ~ ., data = train_gbm_cut, method = "gbm", trControl = controll, verbose = TRUE, distribution =
      "bernoulli"
  );

survGBM;

resGBM <- predict(survGBM, test, type = "raw");

resultGBM <-
  cbind(test$PassengerId, as.numeric(resGBM) - 1); #переписать тут нормально
resultGBM <- as.data.frame(resultGBM);
names(resultGBM)[1] <- "PassengerId";
names(resultGBM)[2] <- "Survived";

write.csv(resultGBM, "titanic_gbm.csv", row.names = FALSE);

output1 <- read.csv("titanic_gbm.csv");
