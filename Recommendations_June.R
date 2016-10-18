
##OIT Library Cleaning and Matching File Creation (FIRST REPLACE X WITH 1)
OIT.Library <- read.csv("~/Documents/OIT_June_v2.csv", check.names = FALSE)
offer_lib <- OIT.Library[,c(1,2,8:623)]
colnames(offer_lib) <- offer_lib[1,]
ones <- which(offer_lib==1, arr.ind=T)
offer_lib[ones]<-colnames(offer_lib)[ones[,2]]
cn <- as.numeric(names(offer_lib), na.rm = T)
cn <- cn[!is.na(cn)]
offer_lib$sel_lib <- do.call("paste", c(offer_lib[c(3:618)], sep=","))
offer_lib$sel_lib <- gsub("NA,", "", offer_lib$sel_lib)
offer_lib$sel_lib <- gsub(",NA", "", offer_lib$sel_lib)
write.csv(offer_lib, "offer_lib_2016_June.csv")

OIT.Library <- read.csv("~/Documents/GMA/OIT_Coop_AJC_Mar.csv", header=FALSE, comment.char="#")
offer_look_up <- OIT.Library[1:2,c(8:543)]
offer_look_up <- t(offer_look_up)
offer_look_up <- data.frame(offer_look_up)
colnames(offer_look_up) <- c("offer_name","offer_id")
offer_look_up$offer_name  <- gsub('\x8e','e',offer_look_up$offer_name)

# Fuzzy match offer ID and offer name 
comp_offer_id <-read.csv("~/Documents/GMA/offer rec with id_Apr.csv")

comp_offer_id$offer_name<- gsub('McCaf_','McCafe',comp_offer_id$offer_name)

comp_offer_id$offer_name<- gsub('\xfc\xbe\x8c\x86\x90\xbc ','',comp_offer_id$offer_name)

comp_offer_id$offer_name<-gsub('\x8e','e',comp_offer_id$offer_name)

comp_offer_id$offer_name<-gsub('\x96','',comp_offer_id$offer_name)

comp_offer_id$offer_name<- gsub('w/Shakin\xd5','w/Shakin',comp_offer_id$offer_name)

comp_offer_id$offer_name<- gsub('Frapp_','Frappe',comp_offer_id$offer_name)

comp_offer_id$offer_name<- gsub('Jalape\xd0os','Jalapenos',comp_offer_id$offer_name)

comp_offer_id$offer_name<- gsub('\xe9','e',comp_offer_id$offer_name)

comp_offer_id$offer_name<- gsub('w/Shakin\x92','w/Shakin',comp_offer_id$offer_name)

comp_offer_id$offer_name<- gsub('w/Shakin\xcd','w/Shakin',comp_offer_id$offer_name)

comp_offer_id$offer_name<- gsub('Jalape\xf1os','Jalapenos',comp_offer_id$offer_name)

Apr_Offers <- unique(comp_offer_rating_id$offer_name)

agrepMerge <- function(df1, df2, by, all.x = FALSE, all.y = FALSE, 
                       ignore.case = FALSE, value = FALSE, max.distance = 0.1, useBytes = FALSE) {
  
  df1$index <- apply(df1[,by, drop = FALSE], 1, paste, sep = "", collapse = "")
  df2$index <- apply(df2[,by, drop = FALSE], 1, paste, sep = "", collapse = "")
  
  matches <- lapply(seq_along(df1$index), function(i, ...) {
    agrep(df1$index[i], df2$index, ignore.case = ignore.case, value = value,
          max.distance = max.distance, useBytes = useBytes)
  })
  
  df1_match <- rep(1:nrow(df1), sapply(matches, length))
  df2_match <- unlist(matches)
  
  df1_hits <- df1[df1_match,]
  df2_hits <- df2[df2_match,]
  
  df1_miss <- df1[setdiff(seq_along(df1$index), df1_match),]
  df2_miss <- df2[setdiff(seq_along(df2$index), df2_match),]
  
  remove_cols <- colnames(df2_hits) %in% colnames(df1_hits)
  
  df_out <- cbind(df1_hits, df2_hits[,!remove_cols])
  
  if(all.x) {
    missing_cols <- setdiff(colnames(df_out), colnames(df1_miss))
    df1_miss[missing_cols] <- NA
    df_out <- rbind(df_out, df1_miss)
  }
  if(all.x) {
    missing_cols <- setdiff(colnames(df_out), colnames(df2_miss))
    df2_miss[missing_cols] <- NA
    df_out <- rbind(df_out, df2_miss)
  }
  df_out[,setdiff(colnames(df_out), "index")]
}

offer_matching <- agrepMerge(comp_offer_id,Apr_Offers)

offer_matching_discount  <- filter(offer_matching,grepl("or more",offer_name))

agrepMerge <- function(df1, df2, by, all.x = FALSE, all.y = FALSE, 
                       ignore.case = FALSE, value = FALSE, max.distance = 0.1, useBytes = FALSE) {
  
  df1$index <- apply(df1[,by, drop = FALSE], 1, paste, sep = "", collapse = "")
  df2$index <- apply(df2[,by, drop = FALSE], 1, paste, sep = "", collapse = "")
  
  matches <- lapply(seq_along(df1$index), function(i, ...) {
    agrep(df1$index[i], df2$index, ignore.case = ignore.case, value = value,
          max.distance = max.distance, useBytes = useBytes)
  })
  
  df1_match <- rep(1:nrow(df1), sapply(matches, length))
  df2_match <- unlist(matches)
  
  df1_hits <- df1[df1_match,]
  df2_hits <- df2[df2_match,]
  
  df1_miss <- df1[setdiff(seq_along(df1$index), df1_match),]
  df2_miss <- df2[setdiff(seq_along(df2$index), df2_match),]
  
  remove_cols <- colnames(df2_hits) %in% colnames(df1_hits)
  
  df_out <- cbind(df1_hits, df2_hits[,!remove_cols])
  
  if(all.x) {
    missing_cols <- setdiff(colnames(df_out), colnames(df1_miss))
    df1_miss[missing_cols] <- NA
    df_out <- rbind(df_out, df1_miss)
  }
  if(all.x) {
    missing_cols <- setdiff(colnames(df_out), colnames(df2_miss))
    df2_miss[missing_cols] <- NA
    df_out <- rbind(df_out, df2_miss)
  }
  df_out[,setdiff(colnames(df_out), "index")]
}

offer_matching <- agrepMerge(comp_offer_id,Apr_Offers)
offer_matching  <- filter(offer_matching,!grepl("or more",offer_name))


# Packages
library(dplyr)
library(reshape)
library(recommenderlab)
library(stats)
library(plyr)

# Files
Redeemed_Offers <- read.csv("~/Documents/GMA/recommendations_june.csv")
lift <- read.csv("~/Downloads/lift (1).csv.gz")
Recommendations_Lifts_V6 <- read.csv("~/Desktop/Scripts and Data/Optimize Offer/Offers/Recommendations_Lifts_V6.csv")
comp_offer_id <-read.csv("~/Documents/GMA/offer rec with id_Apr_v3.csv")
Offer_Info <- read.csv("~/Documents/GMA/recommendations_june.csv")
offer_lib <- read.csv("~/Desktop/Scripts and Data/Optimize Offer/Offers/offer_lib_2016_June.csv", row.names=1)
Missing_Redemptions_Functions.for.Types <- read.csv("~/Documents/Missing_Redemptions_Functions for Types_June.csv")

##Data transformation

#Redeemed_Offers2 <- read.csv("~/Documents/GMA/Recommendations_Feb.csv")

Redeemed_Offers<- Redeemed_Offers[,c("offer_name_a","coop_name","RedeemedCount")]

##Remove Welcome Offer, if necessary
Redeemed_Offers<-Redeemed_Offers[!Redeemed_Offers$offer_name_a == "Free Breakfast or Regular Menu Sandwich", ]

##Create IDs for All Offers to Run Program
Rec_ID<-transform(Redeemed_Offers,id=as.numeric(factor(offer_name_a)))

Rec_ID_Coop_Redeemed<-Rec_ID[,c("id","coop_name","RedeemedCount")]

##Transform into Matrix and Normalize for Program
Rec_ID_Matrix<-cast(Rec_ID_Coop_Redeemed,coop_name~id,sum)

Rec_ID_Matrix_Rows<-data.frame(Rec_ID_Matrix[,-1],row.names = Rec_ID_Matrix[,1])

Rec_ID_Matrix_Rows[Rec_ID_Matrix_Rows==0]<-NA

Rec_ID_Matrix_Rows<-t(apply(Rec_ID_Matrix_Rows,1,scale))

#Recommendation Program
Real_Rec<-as(as.matrix(Rec_ID_Matrix_Rows),"realRatingMatrix") #Transformation to realRatingMatrix object

Rec_Model_IBCF<-Recommender(Real_Rec,method="IBCF")  #Cosine similarity transformation

Recommended_Offers<-predict(Rec_Model_IBCF,Real_Rec,type="ratings") #offer rating predictions based on weighted sum

##Evaluation Plots and Error Values (RMSE and MAE)
es <- evaluationScheme(Recommended_Offers, method="split", train=0.9, given=1, goodRating=0)

r1<-Recommender(getData(es,"train"),method="IBCF")
r2<-Recommender(getData(es,"train"),method="UBCF")
r3<-Recommender(getData(es,"train"),method="POPULAR")

p1 <- predict(r1,getData(es,"known"),type="ratings")
p2 <- predict(r2,getData(es,"known"),type="ratings")
p3 <- predict(r3,getData(es,"known"),type="ratings")

error <- rbind(calcPredictionAccuracy(p1,getData(es,"unknown")),calcPredictionAccuracy(p2,getData(es,"unknown")),calcPredictionAccuracy(p3,getData(es,"unknown")))
rownames(error) <- c("IBCF","UBCF","POPULAR")
error

#ROC Curves and Prec/Recall- Binary outcomes
pretty_palette <- c("#64c4df", '#f7cb31', '#ed5861', '#2B8B77', '#a48bbe', '#8dc63f', '#fc6b27')
es <- evaluationScheme(Recommended_Offers, method="cross-validation", train=0.9, given=1, goodRating=0)
algorithms <- list(
  RANDOM = list(name = "RANDOM", param = NULL),
  POPULAR = list(name = "POPULAR", param = NULL),
  UBCF = list(name = "UBCF", param = NULL),
  IBCF = list(name="IBCF",param= NULL)
)
evlist <- evaluate(es, algorithms, type="ratings")
plot(evlist, "prec/rec", pch=20, col= pretty_palette)
plot(evlist, "prec/rec", annotate=3, col= pretty_palette)
evlist <- evaluate(es, algorithms, n=c(5,10,50,100,200,1000))
plot(evlist, "prec/rec", lty=1, col= pretty_palette)
plot(evlist, pch=20, col= pretty_palette)
avg(evlist)

##Data frame ratings matrix and bind redeemed and recommeneded offers
Recommended_Offers_DF<-getData.frame(Recommended_Offers)

Redeemed_Offers_DF<-as(Real_Rec,"matrix")

Redeemed_Offers_DF<-data.frame(Redeemed_Offers_DF)

Redeemed_Offers_DF<-as(as.matrix(Redeemed_Offers_DF),"realRatingMatrix")

Redeemed_Offers_DF<-getData.frame(Redeemed_Offers_DF)

Red_Rec_Offers<-rbind(Redeemed_Offers_DF,Recommended_Offers_DF)

Red_Rec_Offers$item<-gsub('X',"",Red_Rec_Offers$item)

## Merge ID with Redeemed/Recommended offers
colnames(Red_Rec_Offers)[2]<-c("id")

Red_Rec_Offers_ID<-merge(Red_Rec_Offers,Rec_ID,by=c("id"),all=TRUE)

Red_Rec_Offers_ID<-Red_Rec_Offers_ID[,c("offer_name_a","user","rating")]

Red_Rec_Offers_ID<-unique(Red_Rec_Offers_ID)

## Merge Lift with All Final Ratings
#`lift.(1).csv` <- read.csv("~/Downloads/lift (1).csv.gz")

#lift<-`lift.(1).csv`

Red_Rec_Offers_ID$offer_name_a<- gsub('McCaf_','McCafe',Red_Rec_Offers_ID$offer_name_a)

Red_Rec_Offers_ID$offer_name_a<- gsub('\xfc\xbe\x8c\x86\x90\xbc ','',Red_Rec_Offers_ID$offer_name_a)

Red_Rec_Offers_ID$offer_name_a<-gsub('\x8e','e',Red_Rec_Offers_ID$offer_name_a)

Red_Rec_Offers_ID$offer_name_a<-gsub('\x96','',Red_Rec_Offers_ID$offer_name_a)

Red_Rec_Offers_ID$offer_name_a<- gsub('w/Shakin\xd5','w/Shakin',Red_Rec_Offers_ID$offer_name_a)

Red_Rec_Offers_ID$offer_name_a<- gsub('Frapp_','Frappe',Red_Rec_Offers_ID$offer_name_a)

Red_Rec_Offers_ID$offer_name_a<- gsub('Jalape\xd0os','Jalapenos',Red_Rec_Offers_ID$offer_name_a)

Red_Rec_Offers_ID$offer_name_a<- gsub('\xe9','e',Red_Rec_Offers_ID$offer_name_a)

Red_Rec_Offers_ID$offer_name_a<- gsub('w/Shakin\x92','w/Shakin',Red_Rec_Offers_ID$offer_name_a)

Red_Rec_Offers_ID$offer_name_a<- gsub('Jalape\xf1os','Jalapenos',Red_Rec_Offers_ID$offer_name_a)

#Lift Offer Name Changes to "jalapenos" and "shakin" offers

jalapenos<-lift[lift$OFFR_NM_A=="FREE add Jalape?os to any Sandwich purchase",]

jalapenos$OFFR_NM_A<-"FREE add Jalapenos to any Sandwich purchase"

shakin<-lift[lift$OFFR_NM_A=="FREE S Fries w/Shakin? Flavor McNuggets purch",]

shakin$OFFR_NM_A<-"FREE S Fries w/Shakin Flavor McNuggets purch"

lift<-lift[!lift$OFFR_NM_A=="FREE add Jalape?os to any Sandwich purchase",]

lift<-lift[!lift$OFFR_NM_A=="FREE S Fries w/Shakin? Flavor McNuggets purch",]

jal_sha<-rbind(jalapenos,shakin)

lift$lift <- log(lift$lift+10)

lift<-rbind(jal_sha,lift)

#Merging Lift and Redemptions 

colnames(lift)[2]<-"offer_name_a"

colnames(lift)[6]<-"user"

library(stats)

Red_Rec_Lift_Offers_ID<-merge(Red_Rec_Offers_ID,lift,by=c("offer_name_a","user"), all.x=TRUE)

Red_Rec_Lift_Offers_ID <- Red_Rec_Lift_Offers_ID[,c(1,2,3,10)]

Red_Rec_Lift_Offers_ID$lift[is.na(Red_Rec_Lift_Offers_ID$lift)] <- mean(complete.cases(Red_Rec_Lift_Offers_ID$lift))

Red_Rec_Lift_Offers_ID<-unique(Red_Rec_Lift_Offers_ID)

Red_Rec_Lift_Offers_ID$Offer_User<- paste(Red_Rec_Lift_Offers_ID$offer_name_a,Red_Rec_Lift_Offers_ID$user,sep="_")

Aggregated_Offers <- aggregate(lift~Offer_User,Red_Rec_Lift_Offers_ID,mean)

Red_Rec_Lift_Offers_ID <- merge(Red_Rec_Lift_Offers_ID,Aggregated_Offers,by=c("Offer_User"))

Red_Rec_Lift_Offers_ID<- Red_Rec_Lift_Offers_ID[,c(2,3,4,6)]

Red_Rec_Lift_Offers_ID<-unique(Red_Rec_Lift_Offers_ID)

############################# Adding item discounts to redeemed/lift table########################

Item_Discount <- lift[lift$offer_name_a=="Item Discount",]

Item_Discount<- aggregate(lift~user,Item_Discount,mean)

library(dplyr)

Item_Discount_Redeemeed<-filter(Red_Rec_Offers_ID, grepl('or more', offer_name_a))

Item_Discount_Merge <- merge(Item_Discount,Item_Discount_Redeemeed,by="user")

Item_Discount_Merge <- Item_Discount_Merge[,c(3,1,4,2)]

colnames(Item_Discount_Merge)[1] <- "offer_name_a"

colnames(Red_Rec_Lift_Offers_ID)[4] <- "lift"

Red_Rec_Lift_Offers_ID <- rbind(Item_Discount_Merge,Red_Rec_Lift_Offers_ID)

################################################################################################
## Create Adjusted Ratings with Weights for Lift
Red_Rec_Lift_Offers_ID$adjusted_rating<-log((Red_Rec_Lift_Offers_ID$rating+5000))*Red_Rec_Lift_Offers_ID$lift

Red_Rec_Lift_Offers_ID$adjusted_rating_weighted<-log((Red_Rec_Lift_Offers_ID$rating+5000))*log((3000+Red_Rec_Lift_Offers_ID$lift))

Final_Offer_List<-Red_Rec_Lift_Offers_ID

## Add Offer Type, Breakfast (Y/N), and Ingredients
comp_offer_rating<- Final_Offer_List

#########################################################Offer Exclusions###################################
library(dplyr)

comp_offer_rating <- filter(comp_offer_rating,!grepl("Buy One Big Mac|Buy one Big Mac|Buy one Egg McMuffin|FREE medium fries with purchase of Denali|Buy one McMuffin|Buy one Biscuit|get Hash|Bacon Egg Cheese Biscuit get|Buy one Blueberry Biscuit|Buy one Filet|FREE Hash|Buy one Large Fries|FREE Medium Soft Drink|FREE Medium Soft Drink|FREE M Soft Drink|Free M Soft Drink|FREE M Fries|FREE Medium Fries|Free Medium Fries|FREE Small Smoothie|FREE Small Frappe|Won|Win|Game|Won!|Win!|one Bacon, Egg & Cheese Biscuit|Buy one Buttermilk Ckn Biscuit,",offer_name_a))

#############################################################################################################

names(comp_offer_rating)[1] <-"offer_name"

#Offer_Info <- read.csv("~/Documents/GMA/Recommendations_Mar.csv") #Use Redemption Count SQL Pull

Offer_Info$offer_name_a<- gsub('McCaf_','McCafe',Offer_Info$offer_name_a)

Offer_Info$offer_name_a<- gsub('\xfc\xbe\x8c\x86\x90\xbc ','',Offer_Info$offer_name_a)

Offer_Info$offer_name_a<-gsub('\x8e','e',Offer_Info$offer_name_a)

Offer_Info$offer_name_a<-gsub('\x96','',Offer_Info$offer_name_a)

Offer_Info$offer_name_a<- gsub('w/Shakin\xd5','w/Shakin',Offer_Info$offer_name_a)

Offer_Info$offer_name_a<- gsub('Frapp_','Frappe',Offer_Info$offer_name_a)

Offer_Info$offer_name_a<- gsub('Jalape\xd0os','Jalapenos',Offer_Info$offer_name_a)

Offer_Info$offer_name_a<- gsub('\xe9','e',Offer_Info$offer_name_a)

Offer_Info$offer_name_a<- gsub('w/Shakin\x92','w/Shakin',Offer_Info$offer_name_a)

Offer_Info$offer_name_a<- gsub('Jalape\xf1os','Jalapenos',Offer_Info$offer_name_a)

Offer_Info <- Offer_Info[,c(2:5)]

Offer_Info <- Offer_Info[!duplicated(Offer_Info),]

names(Offer_Info) <- c("offer_name","offer_type","ingredient_type", "breakfast")

#########################################################Offer Exclusions###################################
library(dplyr)

Offer_Info <- filter(Offer_Info,!grepl("Buy One Big Mac|Buy one Big Mac|Buy one Egg McMuffin|FREE medium fries with purchase of Denali|Buy one McMuffin|Buy one Biscuit|get Hash|Bacon Egg Cheese Biscuit get|Buy one Blueberry Biscuit|Buy one Filet|FREE Hash|Buy one Large Fries|FREE Medium Soft Drink|FREE Medium Soft Drink|FREE M Soft Drink|Free M Soft Drink|FREE M Fries|FREE Medium Fries|Free Medium Fries|FREE Small Smoothie|FREE Small Frappe|Won|Win|Game|Won!|Win!|one Bacon, Egg & Cheese Biscuit|Buy one Buttermilk Ckn Biscuit,",offer_name))

#############################################################################################################


comp_offer_rating <- merge(comp_offer_rating, Offer_Info, by="offer_name", all.x = T)

# offer rec with id.csv is the mapping between the SQL Offer Name and Offer ID.

comp_offer_id <-read.csv("~/Documents/GMA/offer rec with id_Apr_v3.csv")

names(comp_offer_id) <- c("offer_name","offer_id")

# The purpose is adding offer id into the rating data set.

comp_offer_rating_id <- merge(comp_offer_rating, comp_offer_id, by="offer_name", all.x = T)

####################################Add in additional offers without redemptions, if necessary###################################
Missing_Redemptions_Functions.for.Types <- read.csv("~/Documents/Missing_Redemptions_Functions for Types_June.csv")

Missing_Redemptions_Functions.for.Types$rating <- 0

#Missing_Redemptions_Functions.for.Types <- filter(Missing_Redemptions_Functions.for.Types,!grepl("Buy One Big Mac|Buy one Big Mac|Buy one Egg McMuffin|FREE medium fries with purchase of Denali|Buy one McMuffin|Buy one Biscuit|get Hash|Bacon Egg Cheese Biscuit get|Buy one Blueberry Biscuit|Buy one Filet|FREE Hash|Buy one Large Fries|FREE Medium Soft Drink|FREE Medium Soft Drink|FREE M Soft Drink|Free M Soft Drink|FREE M Fries|FREE Medium Fries|Free Medium Fries|FREE Small Smoothie|FREE Small Frappe|Won|Win|Game|Won!|Win!|one Bacon, Egg & Cheese Biscuit|Buy one Buttermilk Ckn Biscuit,",offer_name))

co_ops <- data.frame(unique(comp_offer_rating$user))

test<- merge(co_ops,Missing_Redemptions_Functions.for.Types,all.x=TRUE)

colnames(test)[1] <- "co_op"

colnames(comp_offer_rating_id)[2] <- "co_op"

test <- test[,c(2,1,10,4,5,6,7,8,9,3)]

colnames(test)[4] <- "lift.y"

comp_offer_rating_id <- rbind(comp_offer_rating_id,test)

comp_offer_rating_id$offer_id <- as.character(comp_offer_rating_id$offer_id)

comp_offer_rating_id <- unique(comp_offer_rating_id)

##########################################################################
# Now we have an list of offer rating with id

#offer_lib <- read.csv("~/Desktop/Scripts and Data/Optimize Offer/Offers/offer_lib_2016.csv", row.names=1)

coop_offer_lib <- offer_lib[,c(1,2,619)]

names(coop_offer_lib) <- c("co_op", "co_op_id", "selected_lib") # change all the 1 into X

coop_offer_lib$co_op <- gsub("1", "X", coop_offer_lib$co_op)

coop_offer_lib$co_op <- gsub("T.C. Metzner", "T.C. METZNER", coop_offer_lib$co_op)

coop_offer_lib$co_op <- gsub("BOSTON ", "BOSTON", coop_offer_lib$co_op)

names(comp_offer_rating_id)[2]<-"co_op"

coop_rating_lib <- merge(comp_offer_rating_id, coop_offer_lib, by="co_op", all.x = T)

sum(is.na(coop_rating_lib$co_op_id)) / length(coop_rating_lib$co_op_id) # check how many NA are in there

#fix the incorrect offer

coop_rating_lib$offer_id <- paste(",", coop_rating_lib$offer_id, ",", sep="")

coop_rating_lib$selected_lib <- paste(",", coop_rating_lib$selected_lib, ",", sep="")

coop_rating_lib$in_lib <- apply(coop_rating_lib[,c("offer_id","selected_lib")], 1, function(x){grepl(x[1],x[2])}) # create checker

table(coop_rating_lib$in_lib)

# recode three category

# library(plyr)

# coop_rating_lib$ingredient_type <- revalue(coop_rating_lib$ingredient_type, c("beef"="Beef", "0" = "Others"))

# Start genrate ranks

# create four ranks col for selection and forming into a new file.

coop_rating_lib <- coop_rating_lib[ which(coop_rating_lib$in_lib == T), ]

############# Week 4 & 5 Duplications ################
library(plyr)
count<-count(coop_rating_lib,"co_op")

#Unique_Offers <- data.frame(unique(comp_offer_rating_id[,c(1,10)]))
#write.csv(count,"Incomplete_Coops.csv")

#count$counts <- ifelse(count$freq<16,1,0) ## 16 for 4 Week

count$counts <- ifelse(count$freq<25,1,0) # 21 for Week 5

coop_rating_lib <- merge(coop_rating_lib, count[,c("co_op","counts","freq")], by.all=TRUE)

coop_ratings_lib_count <- coop_rating_lib[coop_rating_lib$counts==1,]

coop_ratings_lib_count <- coop_ratings_lib_count[rep(row.names(coop_ratings_lib_count), each= 30-coop_ratings_lib_count$freq),]

coop_ratings_lib_count$dup <- ifelse(duplicated(coop_ratings_lib_count),1,0)

coop_ratings_lib_count$adjusted_rating_weighted[which(coop_ratings_lib_count$dup==1)] <- 0

coop_ratings_lib_count <- coop_ratings_lib_count[,c(1:13)]

coop_rating_lib<- coop_rating_lib[!coop_rating_lib$counts==1,]

coop_rating_lib <- coop_rating_lib[,c(1:13)]

coop_rating_lib <- rbind(coop_ratings_lib_count, coop_rating_lib)

#coop_rating_lib <- unique(coop_rating_lib)

## write.csv(coop_rating_lib, "coop_rating_lib_d.csv") # output the final pool

############## V2 Optimization starts here #####################

# Create Three Slots

coop_rating_lib$s1 <- ifelse(coop_rating_lib$breakfast == "Yes", 1, 0)

coop_rating_lib$s2 <- ifelse(coop_rating_lib$ingredient_type == "Beef" | coop_rating_lib$ingredient_type == "beef" | coop_rating_lib$ingredient_type == "Chicken", 1, 0)

coop_rating_lib$s3 <- ifelse(coop_rating_lib$s1 == 0 & coop_rating_lib$s2 == 0, 1, 0) # The third slots function as an other


# Creating ranks

coop_rating_lib$s1_rk <- ave(coop_rating_lib$adjusted_rating_weighted, coop_rating_lib$co_op, coop_rating_lib$s1, FUN = function(x) rank(-x, ties.method = "first"))

coop_rating_lib$s2_rk <- ave(coop_rating_lib$adjusted_rating_weighted, coop_rating_lib$co_op, coop_rating_lib$s2, FUN = function(x) rank(-x, ties.method = "first"))

coop_rating_lib$s3_rk <- ave(coop_rating_lib$adjusted_rating_weighted, coop_rating_lib$co_op, coop_rating_lib$s3, FUN = function(x) rank(-x, ties.method = "first"))

# hand coding the paired sch_id

coop_rating_lib$sch_id_5wk <- ifelse(coop_rating_lib$s1 == 1 & coop_rating_lib$s1_rk == 1 | coop_rating_lib$s2 == 1 & coop_rating_lib$s2_rk == 2 | coop_rating_lib$s2 == 1 & coop_rating_lib$s3_rk == 8 | coop_rating_lib$s3 == 1 & coop_rating_lib$s3_rk == 9 | coop_rating_lib$s1 == 1 & coop_rating_lib$s1_rk == 6 , 1,
                                     ifelse(coop_rating_lib$s1 == 1 & coop_rating_lib$s1_rk == 2 | coop_rating_lib$s2 == 1 & coop_rating_lib$s2_rk == 3 | coop_rating_lib$s2 == 1 & coop_rating_lib$s3_rk == 8 | coop_rating_lib$s3 == 1 & coop_rating_lib$s3_rk == 10 | coop_rating_lib$s1 == 1 & coop_rating_lib$s1_rk == 7 , 2,
                                            ifelse(coop_rating_lib$s1 == 1 & coop_rating_lib$s1_rk == 3 | coop_rating_lib$s2 == 1 & coop_rating_lib$s2_rk == 4 | coop_rating_lib$s2 == 1 & coop_rating_lib$s3_rk == 7 | coop_rating_lib$s3 == 1 & coop_rating_lib$s3_rk == 6 | coop_rating_lib$s1 == 1 & coop_rating_lib$s1_rk == 8 , 3,
                                                   ifelse(coop_rating_lib$s1 == 1 & coop_rating_lib$s1_rk == 4 | coop_rating_lib$s2 == 1 & coop_rating_lib$s2_rk == 5 | coop_rating_lib$s2 == 1 & coop_rating_lib$s3_rk == 6 | coop_rating_lib$s3 == 1 & coop_rating_lib$s3_rk == 7 | coop_rating_lib$s1 == 1 & coop_rating_lib$s1_rk == 9 , 4,
                                                          ifelse(coop_rating_lib$s1 == 1 & coop_rating_lib$s1_rk == 5 | coop_rating_lib$s2 == 1 & coop_rating_lib$s2_rk == 1 | coop_rating_lib$s2 == 1 & coop_rating_lib$s3_rk == 9 | coop_rating_lib$s3 == 1 & coop_rating_lib$s3_rk == 8 | coop_rating_lib$s1 == 1 & coop_rating_lib$s1_rk == 10 , 5,
                                                                 0)))))
# 5 Offers in a 4 Week
coop_rating_lib$sch_id_4wk <- ifelse(coop_rating_lib$s1 == 1 & coop_rating_lib$s1_rk == 1 | coop_rating_lib$s2 == 1 & coop_rating_lib$s2_rk == 2 | coop_rating_lib$s3 == 1 & coop_rating_lib$s3_rk == 3 | coop_rating_lib$s3 == 1 & coop_rating_lib$s3_rk == 8 | coop_rating_lib$s3 == 1 & coop_rating_lib$s3_rk == 12, 1,
                                     ifelse(coop_rating_lib$s1 == 1 & coop_rating_lib$s1_rk == 2 | coop_rating_lib$s2 == 1 & coop_rating_lib$s2_rk == 3 | coop_rating_lib$s3 == 1 & coop_rating_lib$s3_rk == 4 | coop_rating_lib$s3 == 1 & coop_rating_lib$s3_rk == 5| coop_rating_lib$s3 == 1 & coop_rating_lib$s3_rk == 9, 2,
                                            ifelse(coop_rating_lib$s1 == 1 & coop_rating_lib$s1_rk == 3 | coop_rating_lib$s2 == 1 & coop_rating_lib$s2_rk == 4 | coop_rating_lib$s3 == 1 & coop_rating_lib$s3_rk == 2 | coop_rating_lib$s3 == 1 & coop_rating_lib$s3_rk == 6 | coop_rating_lib$s3 == 1 & coop_rating_lib$s3_rk == 10, 3,
                                                   ifelse(coop_rating_lib$s1 == 1 & coop_rating_lib$s1_rk == 4 | coop_rating_lib$s2 == 1 & coop_rating_lib$s2_rk == 1 | coop_rating_lib$s3 == 1 & coop_rating_lib$s3_rk == 1 | coop_rating_lib$s3 == 1 & coop_rating_lib$s3_rk == 7 | coop_rating_lib$s3 == 1 & coop_rating_lib$s3_rk == 11, 4,
                                                          0))))

## Removing Excess Library
count <- count(coop_rating_lib,"co_op")

coop_rating_lib2 <- merge(count,coop_rating_lib, by="co_op")

coop_rating_lib2 <- coop_rating_lib2[!(coop_rating_lib2$adjusted_rating_weighted==0 & coop_rating_lib2$sch_id_5wk==0 & coop_rating_lib2$freq > 35),]

coop_rating_lib2 <- coop_rating_lib2[,-2]

count2 <- count(coop_rating_lib2,"co_op")


## New Count of Offers

count <- count(coop_rating_lib, c("co_op", "sch_id_4wk"))

count <- count(coop_rating_lib2,"co_op")

write.csv(coop_rating_lib2, "coop_rating_lib_June_v2.csv", row.names = F)

############## Above finish!

# test to see the count for assignment

library(plyr)

count <- count(coop_rating_lib, c("co_op", "sch_id_5wk"))

test <- subset(count, sch_id_4wk >0 & sch_id_4wk < 5 & freq < 4)

count <- count(unique(test$co_op))


## Missing Co-Op Add
GUAM <- comp_offer_rating_id[comp_offer_rating_id$co_op=="HAWAII",]
GUAM$co_op <- "GUAM"
comp_offer_rating_id <- rbind(comp_offer_rating_id,GUAM)

#count offers less than full weeks

count2 <- count[!count$sch_id_5wk=="0",]
count2 <- count(count2, c("freq",""))

offer_match_Apr <- read.csv("~/Documents/offers_lib_apr_v3.csv")
offers_missing_id <- read.csv("~/Documents/offers_apr_v3.csv")
offers_match <- agrepMerge(offer_match_Apr,offers_missing_id)

offer_match_Apr$offer_name<- gsub('McCaf_','McCafe',offer_match_Apr$offer_name)

offer_match_Apr$offer_name<- gsub('\xfc\xbe\x8c\x86\x90\xbc ','',offer_match_Apr$offer_name)

offer_match_Apr$offer_name<-gsub('\x8e','e',offer_match_Apr$offer_name)

offer_match_Apr$offer_name<-gsub('\x96','',offer_match_Apr$offer_name)

offer_match_Apr$offer_name<- gsub('w/Shakin\xd5','w/Shakin',offer_match_Apr$offer_name)

offer_match_Apr$offer_name<- gsub('Frapp_','Frappe',offer_match_Apr$offer_name)

offer_match_Apr$offer_name<- gsub('Jalape\xd0os','Jalapenos',offer_match_Apr$offer_name)

offer_match_Apr$offer_name<- gsub('\xe9','e',offer_match_Apr$offer_name)

offer_match_Apr$offer_name<- gsub('w/Shakin\x92','w/Shakin',offer_match_Apr$offer_name)

offer_match_Apr$offer_name<- gsub('w/Shakin\xcd','w/Shakin',offer_match_Apr$offer_name)

offer_match_Apr$offer_name<- gsub('Jalape\xf1os','Jalapenos',offer_match_Apr$offer_name)

offer_lib_Apr <- filter(offer_lib_Apr,!grepl("Buy One Big Mac|Buy one Big Mac|Buy one Egg McMuffin|FREE medium fries with purchase of Denali|Buy one McMuffin|Buy one Biscuit|get Hash|Bacon Egg Cheese Biscuit get|Buy one Blueberry Biscuit|Buy one Filet|FREE Hash|Buy one Large Fries|FREE Medium Soft Drink|FREE Medium Soft Drink|FREE M Soft Drink|Free M Soft Drink|FREE M Fries|FREE Medium Fries|Free Medium Fries|FREE Small Smoothie|FREE Small Frappe|Won|Win|Game|Won!|Win!|one Bacon, Egg & Cheese Biscuit|Buy one Buttermilk Ckn Biscuit,",offer_name))

