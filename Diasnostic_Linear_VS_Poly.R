###Compare model fit using R-squared###

# winged insect # 
dat_insw_flight <- dat_insw[dat_insw$mr_type == "Flight", ] 
dat_insw_loco <- dat_insw[dat_insw$mr_type == "Locomotion", ] 
dat_insw_rest <- dat_insw[dat_insw$mr_type == "Rest", ] 

# flight 
linear_model <- lm(log_mr ~ log_mass, data = dat_insw_flight)
summary(linear_model)$r.squared
summary(linear_model)
confint(linear_model, level = 0.95)

poly_model <- lm(log_mr ~ poly(log_mass, 2), data = dat_insw_flight)
summary(poly_model)$r.squared # not able to fit

# locomiotion
linear_model <- lm(log_mr ~ log_mass, data = dat_insw_loco)
summary(linear_model)$r.squared
summary(linear_model)
confint(linear_model, level = 0.95)

poly_model <- lm(log_mr ~ poly(log_mass, 2), data = dat_insw_loco)
summary(poly_model)$r.squared

# rest
linear_model <- lm(log_mr ~ log_mass, data = dat_insw_rest)
summary(linear_model)$r.squared
summary(linear_model)
confint(linear_model, level = 0.95)

poly_model <- lm(log_mr ~ poly(log_mass, 2), data = dat_insw_rest)
summary(poly_model)$r.squared 

# wingless insect # 
dat_insnw_loco <- dat_insnw[dat_insnw$mr_type == "Locomotion", ] 
dat_insnw_rest <- dat_insnw[dat_insnw$mr_type == "Rest", ] 

# locomiotion
linear_model <- lm(log_mr ~ log_mass, data = dat_insnw_loco)
summary(linear_model)$r.squared
summary(linear_model)
confint(linear_model, level = 0.95)

poly_model <- lm(log_mr ~ poly(log_mass, 2), data = dat_insnw_loco)
summary(poly_model)$r.squared 

# rest
linear_model <- lm(log_mr ~ log_mass, data = dat_insnw_rest)
summary(linear_model)$r.squared
summary(linear_model)
confint(linear_model, level = 0.95)

poly_model <- lm(log_mr ~ poly(log_mass, 2), data = dat_insnw_rest)
summary(poly_model)$r.squared 

# spider # 
dat_sp_loco <- dat_sp[dat_sp$mr_type == "Locomotion", ] 
dat_sp_rest <- dat_sp[dat_sp$mr_type == "Rest", ] 

# locomiotion
linear_model <- lm(log_mr ~ log_mass, data = dat_sp_loco)
summary(linear_model)$r.squared
summary(linear_model)
confint(linear_model, level = 0.95)

poly_model <- lm(log_mr ~ poly(log_mass, 2), data = dat_sp_loco)
summary(poly_model)$r.squared 

# rest
linear_model <- lm(log_mr ~ log_mass, data = dat_sp_rest)
summary(linear_model)$r.squared
summary(linear_model)
confint(linear_model, level = 0.95)

poly_model <- lm(log_mr ~ poly(log_mass, 2), data = dat_sp_rest)
summary(poly_model)$r.squared 

# when both model fit then we are able to compare them #
install.packages("caret")
library(caret)

# Split data into training and testing sets
set.seed(123)
trainIndex <- createDataPartition(dat_sp_rest$log_mr, p = 0.8, list = FALSE) #here we change the dataset according to the category
train_data <- dat_sp_rest[trainIndex, ]
test_data <- dat_sp_rest[-trainIndex, ]

# Fit models on training data
linear_model_cv <- lm(log_mr ~ log_mass, data = train_data)
poly_model_cv <- lm(log_mr ~ poly(log_mass, 2), data = train_data)

# Predict and calculate RMSE (Root Mean Squared Error) on the test data
linear_pred <- predict(linear_model_cv, newdata = test_data)
poly_pred <- predict(poly_model_cv, newdata = test_data)

linear_rmse <- sqrt(mean((test_data$log_mr - linear_pred)^2))
poly_rmse <- sqrt(mean((test_data$log_mr - poly_pred)^2))

# Compare RMSE values
linear_rmse
poly_rmse

# AIC and BIC for the linear model
AIC(linear_model)
BIC(linear_model)

# AIC and BIC for the polynomial model
AIC(poly_model)
BIC(poly_model)

# Residuals for linear model
plot(linear_model$residuals)

# Residuals for polynomial model
plot(poly_model$residuals)

# Compare linear vs polynomial models
anova(linear_model, poly_model)

######### Polynomial Regression #########
ggplot(dat_sp, aes(x=log_mass, y=log_mr, group= mr_type, color=mr_type)) + geom_point() +
  geom_smooth(method="lm", formula = y ~ poly(x, 2), se=FALSE, fullrange=TRUE) + scale_x_continuous(limits=c(0, 5)) +  scale_y_continuous(limits=c(-1, 6)) +
  ylab("log oxygen consumption") + xlab("log body size") + theme(plot.title = element_text(size = 14, family = "Tahoma", face = "bold"), 
                                                                 text = element_text(size = 12, family = "Tahoma"), 
                                                                 axis.title = element_text(face="bold"), axis.text.x=element_text(size = 11), 
                                                                 legend.position = "top") + scale_color_manual(values=c("#CC9900","black")) + theme_few()

ggplot(dat_insnw, aes(x=log_mass, y=log_mr, group= mr_type, color=mr_type)) + geom_point() +
  geom_smooth(method="lm", formula = y ~ poly(x, 2), se=FALSE, fullrange=TRUE) + scale_x_continuous(limits=c(0, 5)) +  scale_y_continuous(limits=c(-1, 6)) +
  ylab("log oxygen consumption") + xlab("log body size") + theme(plot.title = element_text(size = 14, family = "Tahoma", face = "bold"), 
                                                                 text = element_text(size = 12, family = "Tahoma"), 
                                                                 axis.title = element_text(face="bold"), axis.text.x=element_text(size = 11), 
                                                                 legend.position = "top") + scale_color_manual(values=c("#CC9900","black")) + theme_few()

ggplot(dat_insw, aes(x=log_mass, y=log_mr, group= mr_type, color=mr_type)) + geom_point() +
  geom_smooth(method="lm", formula = y ~ poly(x, 2), se=FALSE, fullrange=TRUE) + scale_x_continuous(limits=c(0, 5)) +  scale_y_continuous(limits=c(-1, 6)) +
  ylab("log oxygen consumption") + xlab("log body size") + theme(plot.title = element_text(size = 14, family = "Tahoma", face = "bold"), 
                                                                 text = element_text(size = 12, family = "Tahoma"), 
                                                                 axis.title = element_text(face="bold"), axis.text.x=element_text(size = 11), 
                                                                 legend.position = "top") + scale_color_manual(values=c("#669933","#CC9900","black")) + theme_few()


