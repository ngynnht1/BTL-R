#Khai bao thu vien
setwd("D:/CTRR/Data")
library("xlsx")
#Nh???p data
GK = read.xlsx("192_CO1007.xlsx",
               2,
               startRow = 5,
               colIndex = 1:29)
CK = read.xlsx(
  "192_CO1007.xlsx",
  4,
  startRow = 5,
  endRow = 366,
  colIndex = 1:33
)

#i. Xac dinh so luong sinh vien trong tap mau
##GK
svGK = nrow(GK)
names(svGK) <- c("Tong so sinh vien thi giua ki")
svGK
##CK
svCK = nrow(CK)
names(svCK) <- c("Tong so sinh vien thi cuoi ki")
svCK

#ii Nhom cau hoi lien quan den so cau cua sinh vien
#1/ 
##GK
socaudungGK = data.frame()
for (i in 1:nrow(GK)) {
  socaudungGK = rbind(socaudungGK, sum(GK[i, 4:28]))
}
colnames(socaudungGK) = c("so cau dung")
socaudungGK
##CK
socaudungCK = data.frame()
for (i in 1:nrow(CK)) {
  socaudungCK = rbind(socaudungCK, sum(CK[i, 4:32]))
}
colnames(socaudungCK) = c("so cau dung")
socaudungCK

#2/ 
##GK
tongsocauGK = sum(ncol(GK[4:28]))
socausaiGK = tongsocauGK - socaudungGK
colnames(socausaiGK) = c("so cau sai")
socausaiGK
##CK

tongsocauCK = sum(ncol(CK[4:32]))
socausaiCK = tongsocauCK - socaudungCK
colnames(socausaiCK) = c("so cau sai")
socausaiCK

#3/
##GK
#MAX

maxdungGK = subset(socaudungGK, socaudungGK == max(socaudungGK))
names(maxdungGK) = (" So cau dung GK nhieu nhat")
maxdungGK
#MIN
mindungGK = min(socaudungGK)
names(mindungGK) = (" SO cau dung GK it nhat")
mindungGK
##CK
#MAX
maxdungCK = max(socaudungCK)
names(maxdungCK) = (" So cau dung CK nhieu nhat")
maxdungCK
#MIN
mindungCK = min(socaudungCK)
names(mindungCK) = (" SO cau dung CK it nhat")
mindungCK

#4/ 
##GK
#Thu vi???n tham gia
library(dplyr)
library(ggplot2)
thongkeGK <- cbind(socaudungGK, socausaiGK)

colnames(GK)[29] = "MADE"
thongkeGK = cbind.data.frame(thongkeGK, "MADE" = GK$MADE)

de1 = thongkeGK[thongkeGK$MADE == 1921, ]
de2 = thongkeGK[thongkeGK$MADE == 1922, ]
de3 = thongkeGK[thongkeGK$MADE == 1923, ]
de4 = thongkeGK[thongkeGK$MADE == 1924, ]

hist(
  de1$"so cau dung",
  main = "BIEU DO PHO SO CAU DUNG DE 1921",
  xlab = "So cau dung",
  col = "blue",
  breaks = c(0:29),
  ylim = c(0, 30)
)
hist(
  de1$"so cau sai",
  main = "BIEU DO PHO SO CAU SAI DE 1921",
  xlab = "So cau sai",
  col = "red",
  breaks = c(0:29),
  ylim = c(0, 30)
)

hist(
  de2$"so cau dung",
  main = "BIEU DO PHO SO CAU DUNG DE 1922",
  xlab = "So cau dung",
  col = "blue",
  breaks = c(0:29),
  ylim = c(0, 30)
)
hist(
  de2$"so cau sai",
  main = "BIEU DO PHO SO CAU SAI DE 1922",
  xlab = "So cau sai",
  col = "red",
  breaks = c(0:29),
  ylim = c(0, 30)
)

hist(
  de3$"so cau dung",
  main = "BIEU DO PHO SO CAU DUNG DE 1923",
  xlab = "So cau dung",
  col = "blue",
  breaks = c(0:29),
  ylim = c(0, 30)
)
hist(
  de3$"so cau sai",
  main = "BIEU DO PHO SO CAU SAI DE 1923",
  xlab = "So cau sai",
  col = "red",
  breaks = c(0:29),
  ylim = c(0, 30)
)

hist(
  de4$"so cau dung",
  main = "BIEU DO PHO SO CAU DUNG DE 1924",
  xlab = "So cau dung",
  col = "blue",
  breaks = c(0:29),
  ylim = c(0, 30)
)
hist(
  de4$"so cau sai",
  main = "BIEU DO PHO SO CAU SAI DE 1924",
  xlab = "So cau sai",
  col = "red",
  breaks = c(0:29),
  ylim = c(0, 30)
)

##CK
thongkeCK <- cbind(socaudungCK, socausaiCK)
colnames(CK)[33] = "MADE"
thongkeCK = cbind.data.frame(thongkeCK, "MADE" = CK$MADE)

de1 = thongkeCK[thongkeCK$MADE == 1921, ]
de2 = thongkeCK[thongkeCK$MADE == 1922, ]
de3 = thongkeCK[thongkeCK$MADE == 1923, ]
de4 = thongkeCK[thongkeCK$MADE == 1924, ]

hist(
  de1$"so cau dung",
  main = "BIEU DO PHO SO CAU DUNG DE 1921",
  xlab = "So cau dung",
  col = "blue",
  breaks = c(0:29),
  ylim = c(0, 30)
)
hist(
  de1$"so cau sai",
  main = "BIEU DO PHO SO CAU SAI DE 1921",
  xlab = "So cau sai",
  col = "red",
  breaks = c(0:29),
  ylim = c(0, 30)
)

hist(
  de2$"so cau dung",
  main = "BIEU DO PHO SO CAU DUNG DE 1922",
  xlab = "So cau dung",
  col = "blue",
  breaks = c(0:29),
  ylim = c(0, 30)
)
hist(
  de2$"so cau sai",
  main = "BIEU DO PHO SO CAU SAI DE 1922",
  xlab = "So cau sai",
  col = "red",
  breaks = c(0:29),
  ylim = c(0, 30)
)


hist(
  de3$"so cau dung",
  main = "BIEU DO PHO SO CAU DUNG DE 1923",
  xlab = "So cau dung",
  col = "blue",
  breaks = c(0:29),
  ylim = c(0, 30)
)
hist(
  de3$"so cau sai",
  main = "BIEU DO PHO SO CAU SAI DE 1923",
  xlab = "So cau sai",
  col = "red",
  breaks = c(0:29),
  ylim = c(0, 30)
)


hist(
  de4$"so cau dung",
  main = "BIEU DO PHO SO CAU DUNG DE 1924",
  xlab = "So cau dung",
  col = "blue",
  breaks = c(0:29),
  ylim = c(0, 30)
)
hist(
  de4$"so cau sai",
  main = "BIEU DO PHO SO CAU SAI DE 1924",
  xlab = "So cau sai",
  col = "red",
  breaks = c(0:29),
  ylim = c(0, 30)
)

#iii 


library(magrittr)
library(ggplot2)
library(reshape2)
library(moments)
library(ggpubr)
#a/
##GK
diemGK = round(rowMeans(as.vector(socaudungGK)) * 10 / tongsocauGK, 2)
diemGK = data.frame(diemGK)
diemGK
##CK
diemCK = round(rowMeans(as.vector(socaudungCK)) * 10 / tongsocauCK, 2)
diemCK = data.frame(diemCK)
diemCK
#b/
diemTongket = transform(merge(diemGK, diemCK, by = 0, all = TRUE),
                        row.names = Row.names,
                        Row.names = NULL)
diemTongket = diemTongket[order(as.numeric(row.names(diemTongket))), ]
diemTongket[is.na(diemTongket)] = 0
temp = data.frame(diemTongket$diemGK * 0.4 + diemTongket$diemCK * 0.6)
colnames(temp) = "diemTongket"
diemTongket = cbind.data.frame(temp)
diemTongket


#1/
##GK
thongkeGK = as.data.frame(GK[1:366, 2:3])
colnames(thongkeGK) = c("Nhom", "To")
thongkeGK$DiemGK = rowSums(GK[1:366, 4:28]) * (10 / 25)
TongdiemGK = round(summary(thongkeGK$DiemGK), 1)
TongdiemGK

DiemGK1 = cbind(thongkeGK$DiemGK)
DiemGK1
##CK
thongkeCK = as.data.frame(CK[1:361, 2:3])
colnames(thongkeCK) = c("Nhom", "To")
thongkeCK$DiemCK = rowSums(CK[1:361, 4:32]) * (10 / 29)
TongdiemCK = round(summary(thongkeCK$DiemCK), 1)

thongkeCK$DiemCK
DiemCK1 = cbind(thongkeCK$DiemCK)

TongdiemCK

#2/ Dem so sinh vien co diem lon hon hoac bang 9
svGK9 = count(thongkeGK, thongkeGK$DiemGK >= 9)
colnames(svGK9)[1] = c("Gia tri TRUE hoac FALSE")
colnames(svGK9)[2] = c("sinh vien co diem Giua Ky >=9")
svGK9

svCK9 = count(thongkeCK, thongkeCK$DiemCK >= 9)
colnames(svCK9)[1] = c("Gia tri TRUE hoac FALSE")
colnames(svCK9)[2] = c("sinh vien co diem Cuoi Ky >=9")
svCK9

#3
svGK7 = count(thongkeGK, thongkeGK$DiemGK >= 7)
colnames(svGK7)[1] = c("Gia tri TRUE hoac FALSE")
colnames(svGK7)[2] = c("sinh vien co diem Giua Ky >=7")
svGK7

svCK7 = count(thongkeCK, thongkeCK$DiemCK >= 7)
colnames(svCK7)[1] = c("Gia tri TRUE hoac FALSE")
colnames(svCK7)[2] = c("sinh vien co diem Cuoi Ky >=7")
svCK7


#4
svGK5 = count(thongkeGK, thongkeGK$DiemGK >= 5)
colnames(svGK5)[1] = c("Gia tri TRUE hoac FALSE")
colnames(svGK5)[2] = c("sinh vien co diem Giua Ky >= 5")
svGK5

svCK5 = count(thongkeCK, thongkeCK$DiemCK >= 5)
colnames(svCK5)[1] = c("Gia tri TRUE hoac FALSE")
colnames(svCK5)[2] = c("sinh vien co diem Cuoi Ky >= 5")
svCK5

#5
svGKN5 = count(thongkeGK, thongkeGK$DiemGK < 5)
colnames(svGKN5)[1] = c("Gia tri TRUE hoac FALSE")
colnames(svGKN5)[2] = c("sinh vien co diem Giua Ky < 5")
svGKN5

svCKN5 = count(thongkeCK, thongkeCK$DiemCK < 5)
colnames(svCKN5)[1] = c("Gia tri TRUE hoac FALSE")
colnames(svCKN5)[2] = c("sinh vien co diem Cuoi Ky < 5")
svCKN5

count(thongkeCK, thongkeCK$DiemCK < 5)

#6 Bieu do pho
library(ggplot2)
##GK
ggplot(thongkeGK, aes(x = thongkeGK$DiemGK)) + geom_histogram(bins = 10, col = "white", fill = "green") + labs(title = " PHO DIEM CUA SINH VIEN TRONG TAP MAU GIUA KI", x =
                                                                                                                 " DIEM SO", y = "SO SINH VIEN")  + theme_minimal()
##CK
ggplot(thongkeCK, aes(x = thongkeCK$DiemCK)) + geom_histogram(bins = 20, col = "white", fill = "green") + labs(title = " PHO DIEM CUA SINH VIEN TRONG TAP MAU CUOI KI", x =
                                                                                                                 " DIEM SO", y = "SO SINH VIEN")  + theme_minimal()

#7
thongkeGK
library(dplyr)
thongkeGK = cbind.data.frame(
  No = GK$No,
  MANH = GK$MANH,
  TO = GK$TO,
  DiemGK = DiemGK1
)
thongkeCK = cbind.data.frame(
  No = CK$No,
  MANH = CK$MANH,
  TO = CK$TO,
  DiemCK = DiemCK1
)

#GK
MaxGK = max(DiemGK1)
DSSV_DiemCaoGK = subset(thongkeGK, thongkeGK$DiemGK == MaxGK)
#CK
MaxCK = max(DiemCK1)
DSSV_DiemCaoCK = subset(thongkeCK, thongkeCK$DiemCK == MaxCK)

#8
#GK
MinGK = min(DiemGK1)
MinGK
DSSV_DiemthapGK = subset(thongkeGK, thongkeGK$DiemGK == MinGK)
DSSV_DiemthapGK
#CK
MinCK = min(DiemCK1)
DSSV_DiemthapCK = subset(thongkeCK, thongkeCK$DiemCK == MinCK)

#9 -10
AverageGK = mean(thongkeGK$DiemGK)
DSSV_DiemtbGK = nrow(select(thongkeGK[thongkeGK[, "DiemGK"] == AverageGK, ], No, MANH, TO, DiemGK))

AverageCK = mean(thongkeCK$DiemCK)
DSSV_DiemtbCK = nrow(select(thongkeCK[thongkeCK[, "DiemCK"] == AverageCK, ], No, MANH, TO, DiemCK))

DSSV_DiemtbCK

#11
#GK
desc <- function(AverageGK)
{
  av <- mean(AverageGK)
  sd <- sd(AverageGK)
  se <- sd / sqrt(length(AverageGK))
  c(MEAN = av, SD = sd, SE = se)
}

desc(thongkeGK$DiemGK)

#CK
desc <- function(AverageCK)
{
  av <- mean(AverageCK)
  sd <- sd(AverageCK)
  se <- sd / sqrt(length(AverageCK))
  c(MEAN = av, SD = sd, SE = se)
}

desc(thongkeCK$DiemCK)

#Mean: D??? l???ch chu???n. #SD D??? l???ch chu???n. # SE: Sai s??? chu???n

#12
install.packages(e1071)
library(e1071)
skewness(DiemGK1)
kurtosis(DiemGK1)

#13
QuartileGK = summary(DiemGK1)
QuartileCK = summary(DiemCK1)


#14 -New
thongkeGK
GKK <- function(k) {
  temp <- table(DiemGK1)
  options(digits = 2)
  muc <- as.double(names(temp[length(temp) - k + 1]))
  DS <- thongkeGK[thongkeGK[, "DiemGK"] == muc, ]
  return(DS)
}

GKK(1)
GKK(2)

GK2MucCaoNhat = rbind.data.frame(GKK(1), GKK(2))
GK2MucCaoNhat
#15
ggplot(GK2MucCaoNhat, aes(x = DiemGK)) + geom_histogram(bins = 5, col = "white", fill = "green") + labs(title = " PHO DIEM SINH VIEN O 2 MUC DIEM CAO NHAT GIUA KY", x =" DIEM SO", y = "SO SINH VIEN")  + theme_minimal()

#16
#GK
levelGK <- function(k) {
  lv = round(MaxGK, k)
  return(lv)
}

levelGK(10)

#CK tuong tu

#17 Cau nay chua ro nghia cau hoi
