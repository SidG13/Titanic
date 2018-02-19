train <- read.csv("train.csv", header = T)
test <- read.csv("test.csv", header = T)

test$Survived = "None"

data.combined <- rbind(train, test)


data.combined$Survived <- as.factor(data.combined$Survived)
data.combined$Pclass <- as.factor(data.combined$Pclass)

table(data.combined$Survived)

ggplot(train, aes(x = factor(Pclass), fill = factor(Survived))) +
  geom_bar(width = 0.5) +
  xlab("Pclass") +
  ylab("Total Count") +
  labs(fill = "Survived")

as.character(train$Name)

length(unique(as.character(data.combined$Name)))

#extracts duplicate names
data.combined[which(duplicated(data.combined$Name, fromLast = T)),]
data.combined[which(duplicated(data.combined$Name, fromLast = F)),]

#finds all misses and puts into vector
misses <- data.combined[which(str_detect(data.combined$Name, "Miss.")),]
misses[1:5,]

#puts all males into vector
males <- data.combined[which(data.combined$Sex=="male"),]
males[1:5,]

#extract titles from "Names" and add to "data.combined" as another column
data.combined$Titles[str_detect(data.combined$Name,"Miss.")==T]="Miss"
data.combined$Titles[str_detect(data.combined$Name,"Master")==T]="Master"
data.combined$Titles[str_detect(data.combined$Name,"Mr.")==T]="Mr."
data.combined$Titles[str_detect(data.combined$Name,"Mrs.")==T]="Mrs."

ggplot(data.combined[1:891,], aes(x = Age, fill = Survived)) +
  facet_wrap(~Sex + Pclass) +
  geom_histogram(binwidth = 10) +
  xlab("Age") +
  ylab("Total Count")

boys <- data.combined[which(data.combined$Title == "Master"),]
summary(boys$Age)

summary(misses$Age)

misses.alone <- misses[which(misses$SibSp==0 & misses$Parch==0),]
summary(misses.alone$Age)

#change sibsp into a factor
length(unique(data.combined$SibSp))
data.combined$SibSp <- as.factor(data.combined$SibSp)

#make family size variable
temp.sibsp <- c(train$SibSp, test$SibSp)
temp.parch <- c(train$Parch, test$Parch)
data.combined$Family.Size <- as.factor(temp.sibsp + temp.parch + 1)

#ticket stuff
str(data.combined$Ticket)
data.combined <- as.character(data.combined$Ticket)
ticket.first.char <- substr(data.combined$Ticket, 1, 1)
unique(ticket.first.char)

#fares stuff
summary(data.combined$Fare)

#cabin stuff
str(data.combined$Cabin)


# Cabin really isn't a factor, make a string and the display first 100
data.combined$Cabin <- as.character(data.combined$Cabin)
data.combined$Cabin[1:100]


# Replace empty cabins with a "U"
data.combined[which(data.combined$Cabin == ""), "Cabin"] <- "U"
data.combined$Cabin[1:100]
cabin.first.char <- as.factor(substr(data.combined$Cabin, 1, 1))
str(cabin.first.char)
levels(cabin.first.char)
