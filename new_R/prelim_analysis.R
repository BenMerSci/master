# Read the dataset
dataset <- readRDS("data/clean/new_dataset.RDS")

# Check bodymasses
hist(dataset$bodymass_min_prey)
hist(dataset$bodymass_mean_prey)
hist(dataset$bodymass_max_prey)
hist(dataset$bodymass_min_predator)
hist(dataset$bodymass_mean_predator)
hist(dataset$bodymass_max_predator)

boxplot(dataset$bodymass_min_prey)
boxplot(dataset$bodymass_mean_prey)
boxplot(dataset$bodymass_max_prey)
boxplot(dataset$bodymass_min_predator)
boxplot(dataset$bodymass_mean_predator)
boxplot(dataset$bodymass_max_predator)