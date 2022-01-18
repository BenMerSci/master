library(randomForest)
library(caret)

# load the data
dataset <- readRDS("data/clean/dataset.RDS")
dataset <- dataset[-which(grepl("plankton", dataset$prey)),]

# randomForest model
## Dataset for randomForest
#RF_dataset <- dataset[,c("pred_flow","biomass_prey","bodymass_prey","abund_prey","metabolism_prey","class_prey","biomass_pred","bodymass_pred","abund_pred","metabolism_pred","class_pred","ecosystem_type","habitat_type","water_temperature","air_temperature","dimensionality")]
RF_datasetsub <- dataset[,c("pred_flow","biomass_prey","abund_prey","metabolism_prey","biomass_pred","abund_pred","metabolism_pred","ecosystem_type","habitat_type","water_temperature","air_temperature","dimensionality")]

m1 <- randomForest(
  formula = pred_flow ~ .,
  data    = RF_datasetsub,
  ntree = 500,
  mtry = 2
)
plot(m1)
rf_varImp <- varImpPlot(m1)

ggsave("../master_talk/images/rf_var.png",plot = varImpPlot(m1), width = 8, height = 8, dpi = "retina")


model_tuned <- tuneRF(
               x=RF_datasetsub[,-1], #define predictor variables
               y=RF_datasetsub$pred_flow, #define response variable
               ntreeTry=500,
               mtryStart=4, 
               stepFactor=1.5,
               improve=0.01,
               trace=FALSE #don't show real-time progress
               )

saveRDS(m1, "result/rf_mod.RDS")
m1




# Caret
model <- train(RF_datasetsub[,-1], RF_datasetsub[,1], method = "rf")



ctrl <- trainControl(
  method = "cv",
  number = 10,
)
inTraining <- createDataPartition(RF_datasetsub$pred_flow, p = .80, list = FALSE)
training <- RF_datasetsub[inTraining,]
testing  <- RF_datasetsub[-inTraining,]

model <- train(training[,-1], training[,1], method = "rf",trControl = ctrl)
model

test.features = subset(testing, select=-c(pred_flow))
test.target = subset(testing, select=pred_flow)[,1]

predictions = predict(model, newdata = test.features)

# RMSE
sqrt(mean((test.target - predictions)^2))
cor(test.target, predictions) ^ 2
