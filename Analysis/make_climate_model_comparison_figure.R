model_comp <- readxl::read_xlsx("maca_model_comparison.xlsx", skip = 7)

model_comp45 <- model_comp %>%
  mutate(Model = paste0(Model, " RCP 4.5"),
         RCP = 4.5,
         X = `X_RCP45(in)`,
         Y = `Y_RCP45(°F)`) %>%
  dplyr::select(Model, RCP, X, Y)
model_comp85 <- model_comp %>%
  mutate(Model = paste0(Model, " RCP 8.5"),
         RCP = 8.5,
         X = `X_RCP85(in)`,
         Y = `Y_RCP85(°F)`) %>%
  dplyr::select(Model, RCP, X, Y)

model_comp2 <- rbind(model_comp45, model_comp85)  %>%
  mutate(RCP = as.factor(RCP)) %>%
  mutate(X = X * 25.4,
         Y = Y / 1.8)

ggplot(model_comp2, aes(x = X, y = Y)) +
  geom_point(aes(colour = RCP)) + 
  geom_text(data = model_comp2[c(5, 25, 39, 40), ], 
            aes(x = X, y = Y, label = Model)) + 
  xlab("Change in summer precipitation (mm)") +
  ylab("Change in winter temperature (degrees C)")+ 
  scale_x_continuous(expand = expansion(mult = 0.2))
