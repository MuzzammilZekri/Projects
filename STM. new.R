library(stm)

processed <- textProcessor(AIRETEST$Abstract, metadata = AIRETEST)

out <- prepDocuments(processed$documents, processed$vocab, processed$meta)

docs <- out$documents

vocab <- out$vocab

meta <-out$meta

head(out$meta$Year)

summary(out$meta$Year)

str(out$meta$year)

length(out$meta$Year)

out$meta$year <- AIRETEST$Year

length(AIRETEST$Year) == nrow(out$meta)

out$meta$Year <- as.numeric(as.character(out$meta$Year))

str(out$meta)

k_result <- searchK(documents = out$documents,
                    vocab = out$vocab,
                    prevalence = ~ s(Year),
                    K = 5:25,
                    max.em.its = 150, data = meta,
                    init.type = "Spectral")

plot(k_result)

airestm <- stm(documents = out$documents, vocab = out$vocab,
                 K = 15,
                 max.em.its = 150, data = out$meta,
                 init.type = "Spectral", verbose = FALSE)

plot(airestm, type = "summary", xlim = c(0, .6))

labelTopics(airestm)

findThoughts(airestm, texts = AIRETEST$Abstract, n = 5, topics = 1)

stm_k <- manyTopics(documents = out$documents,
                    vocab = out$vocab,
                    data = out$meta,
                    prevalence = ~ Year,
                    K = 5:25,
                    runs = 2,
                    max.em.its = 150,
                    init.type = "Spectral", verbose = TRUE)

plot(stm_k)
length(stm_k$out)

names(stm_k)
labelTopics(stm_k)
str(stm_k$semcoh)
str(stm_k$exclusivity)

library(ggplot2)

K_vals <- sapply(stm_k$out, function(x) x$settings$dim$K)
semcoh_vals <- sapply(stm_k$semcoh, mean)
exclusivity_vals <- sapply(stm_k$exclusivity, mean)

plot_df <- data.frame(K = K_vals,
                      SemanticCoherence = semcoh_vals,
                      Exclusivity = exclusivity_vals)

ggplot(plot_df, aes(x = SemanticCoherence, y = Exclusivity, label = K)) +
  geom_point() +
  geom_text(nudge_y = 0.02, size = 3) +
  labs(title = "Semantic Coherence vs Exclusivity",
       x = "Semantic Coherence", y = "Exclusivity") +
  theme_minimal() + theme(axis.line = element_line(colour = "black")) +
  geom_smooth(formula = y ~ x, method = "lm", col= "red")

stm_kk <- stm(documents = out$documents,
                    vocab = out$vocab,
                    data = out$meta,
                    K = 15,
                    max.em.its = 150,
                    init.type = "Spectral", verbose = TRUE)

plotModels(stm_k)

plot(k_result, 
     n = 5, 
     text.cex = .8)


plotModels(stm_k)
names(stm_k)
length(stm_k$runout)

airestmk <- stm(documents = out$documents, vocab = out$vocab,
               K = 9,
               max.em.its = 150, data = out$meta,
               init.type = "Spectral", verbose = FALSE)

plot(airestmk, type = "summary", xlim = c(0, .3))

labelTopics(airestmk)

findThoughts(airestmk, texts = AIRETEST$Abstract, n = 5, topics = 10)

plot.STM(airestmk, type = "summary")

proportions_table <- make.dt(airestmk)

airestmkcorr <- topicCorr(airestmk)
library(igraph)
plot(airestmkcorr)

best_model <- stm_k$out[[11]]
theta <- best_model$theta
topic_props <- colMeans(theta) * 100
library(ggplot2)
topic_table <- data.frame(
  Topic = factor(1:length(topic_props)),
  Proportion = topic_props
)

ggplot(topic_table, aes(x = Topic, y = Proportion)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  geom_text(aes(label = paste0(round(Proportion, 1), "%")), vjust = -0.5, size = 3.5) +
  labs(title = "Average Topic Proportions",
       x = "Topic",
       y = "Proportion (%)") +
  theme_minimal()

theta <-airestmk$theta

set.seed(831)
stm_ee <- estimateEffect(1:10~Year,
                         stmobj = airestmk, 
                         metadata = out$meta, 
                         uncertainty="Global")
plot(x = stm_ee, 
     covariate = "Year", 
     topic = c(4),
     model = airestmk,
     xlab = "Year",
     main = "what",
     method = "continuous",
     printlegend = FALSE,
     linecol = "red",
     xlim = c(2010,2025))

plot(x = stm_ee, 
     covariate = "Year", 
     topic = c(9),
     model = airestmk,
     xlab = "Year",
     main = "what",
     method = "continuous",
     printlegend = FALSE,
     linecol = "red")

     topics = k_result$results$K,
     xlab = "semantic coherence",
     ylab = "exclusivity",
     labels = k_result$results$K)
processed <- prepDocuments(docs, vocab, meta)  # Replace with your data

k_seq <- c(5, 10, 15, 20, 25, 30)
models <- lapply(k_seq, function(k) {
  stm(documents = processed$documents,
      vocab = processed$vocab,
      K = k,
      data = processed$meta,
      max.em.its = 75,
      init.type = "Spectral")
})

processed <- textProcessor(AIRETESTstm$Abstract, metadata = AIRETESTstm)

out <- prepDocuments(processed$documents, processed$vocab, processed$meta)

docs <- out$documents

vocab <- out$vocab

meta <-out$meta

head(out$meta$Year)

summary(out$meta$Year)

str(out$meta$year)

length(out$meta$Year)

out$meta$year <- AIRETEST$Year

length(AIRETEST$Year) == nrow(out$meta)

out$meta$Year <- as.numeric(as.character(out$meta$Year))

str(out$meta)

k_result <- searchK(documents = out$documents,
                    vocab = out$vocab,
                    prevalence = ~ s(Year),
                    K = 5:25,
                    max.em.its = 150, data = meta,
                    init.type = "Spectral")

plot(k_result)

airestm <- stm(documents = out$documents, vocab = out$vocab,
               K = 15,
               max.em.its = 150, data = out$meta,
               init.type = "Spectral", verbose = FALSE)

plot(airestm, type = "summary", xlim = c(0, .6))

labelTopics(airestm)

findThoughts(airestm, texts = AIRETEST$Abstract, n = 5, topics = 1)

stm_ke <- manyTopics(documents = out$documents,
                    vocab = out$vocab,
                    data = out$meta,
                    prevalence = ~ Year,
                    K = 5:25,
                    runs = 2,
                    max.em.its = 150,
                    init.type = "Spectral", verbose = TRUE)

plot(stm_ke)
length(stm_ke$out)

names(stm_ke)
labelTopics(stm_ke)
str(stm_ke$semcoh)
str(stm_ke$exclusivity)

library(ggplot2)

K_vals <- sapply(stm_ke$out, function(x) x$settings$dim$K)
semcoh_vals <- sapply(stm_ke$semcoh, mean)
exclusivity_vals <- sapply(stm_k$exclusivity, mean)

plot_df <- data.frame(K = K_vals,
                      SemanticCoherence = semcoh_vals,
                      Exclusivity = exclusivity_vals)

ggplot(plot_df, aes(x = SemanticCoherence, y = Exclusivity, label = K)) +
  geom_point() +
  geom_text(nudge_y = 0.02, size = 3) +
  labs(title = "Semantic Coherence vs Exclusivity",
       x = "Semantic Coherence", y = "Exclusivity") +
  theme_minimal() + theme(axis.line = element_line(colour = "black")) +
  geom_smooth(formula = y ~ x, method = "lm", col= "red")

stm_kk <- stm(documents = out$documents,
              vocab = out$vocab,
              data = out$meta,
              K = 15,
              max.em.its = 150,
              init.type = "Spectral", verbose = TRUE)

plotModels(stm_k)

plot(k_result, 
     n = 5, 
     text.cex = .8)


plotModels(stm_k)
names(stm_k)
length(stm_k$runout)

airestmkw <- stm(documents = out$documents, vocab = out$vocab,
                K = 9,
                max.em.its = 150, data = out$meta,
                init.type = "Spectral", verbose = FALSE)

plot(airestmkw, type = "summary", xlim = c(0, .3))

labelTopics(airestmkw)

findThoughts(airestmkw, texts = AIRETESTstm$Abstract, n = 15, topics = 2)

plot.STM(airestmkw, type = "summary")

proportions_table <- make.dt(airestmkw)

airestmkcorr <- topicCorr(airestmkw)
library(igraph)
plot(airestmkcorr)

best_model <- stm_k$out[[11]]
theta <- best_model$theta
topic_props <- colMeans(theta) * 100
library(ggplot2)
topic_table <- data.frame(
  Topic = factor(1:length(topic_props)),
  Proportion = topic_props
)

ggplot(topic_table, aes(x = Topic, y = Proportion)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  geom_text(aes(label = paste0(round(Proportion, 1), "%")), vjust = -0.5, size = 3.5) +
  labs(title = "Average Topic Proportions",
       x = "Topic",
       y = "Proportion (%)") +
  theme_minimal()

theta <-airestmkw$theta
theta_df <- as.data.frame(theta)
topic_props <- airestmkw$theta
# metadata is the dataframe you used with STM that includes a 'year' column
topic_df <- as.data.frame(topic_props)
topic_df$year <- processed$meta$Year
topic_df$year <- out$meta$Year
library(tidyr)
library(dplyr)

# Reshape for easier summarization
topic_df_long <- topic_df %>%
  pivot_longer(cols = starts_with("V"), names_to = "topic", values_to = "proportion")

# Calculate average proportion per topic per year
yearly_topic_props <- topic_df_long %>%
  group_by(year, topic) %>%
  summarise(mean_prop = mean(proportion), .groups = "drop") %>%
  pivot_wider(names_from = topic, values_from = mean_prop)

topic_props <- airestmkw$theta  # Topic proportions
write.csv(topic_props, "topic_proportions.csv")
getwd()
write.csv(yearly_topic_props, "topic_proportions_by_year.csv", row.names = FALSE)

library(readxl)
data <- read_excel("heatmap.xlsx")

library(dplyr)

# Convert to percentages
data_percent <- data %>%
  group_by(year) %>%
  mutate(Percentage = count / sum(count)) %>%
  ungroup()

# Get total article count for line chart
total_articles <- data %>%
  group_by(year) %>%
  summarise(Total = sum(count))


library(ggplot2)

# Base area chart
p <- ggplot(data_percent, aes(x = year, y = Percentage, fill = topic)) +
  geom_area(position = "stack", color = NA) +
  scale_y_continuous(labels = scales::percent, sec.axis = sec_axis(~.*max(total_articles$Total), name = "Number of articles")) +
  theme_minimal() +
  labs(x = "Period", y = "Percentage", title = "Topic Distribution Over Time") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

p <- ggplot(data_percent, aes(x = year, y = Percentage, fill = topic)) +
  geom_area(position = "stack", color = NA) +
  scale_x_continuous(breaks = seq(min(data_percent$year), max(data_percent$year), 1)) +  # Show every year
  scale_y_continuous(labels = scales::percent,
                     sec.axis = sec_axis(~.*max(total_articles$Total), name = "Number of articles")) +
  theme_minimal() +
  labs(x = "Period", y = "Percentage", title = "Topic Distribution Over Time") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

library(ggplot2)

p <- ggplot(data_percent, aes(x = year, y = Percentage, fill = topic)) +
  geom_area(position = "stack", color = NA) +
  scale_fill_brewer(palette = "Set3") +  # 👈 Change palette here
  scale_y_continuous(
    labels = scales::percent,
    sec.axis = sec_axis(~.*max(total_articles$Total), name = "Number of articles")
  ) +
  scale_x_continuous(breaks = seq(min(data_percent$year), max(data_percent$year), 1)) +
  theme_minimal() +
  labs(x = "Period", y = "Percentage", title = "Topic Distribution Over Time") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

p <- ggplot(data_percent, aes(x = year, y = Percentage, fill = topic)) +
  geom_area(position = "stack", color = NA) +
  scale_fill_brewer(palette = "Set3") +  # Or use scale_fill_manual(...)
  scale_y_continuous(
    labels = scales::percent,
    sec.axis = sec_axis(~.*max(total_articles$Total), name = "Number of articles")
  ) +
  scale_x_continuous(breaks = seq(min(data_percent$year), max(data_percent$year), 2)) +
  theme_minimal() +
  labs(x = "Period", y = "Percentage", title = "Topic Distribution Over Time") +
  theme(axis.text.x = element_text(angle = 90, hjust = 0.5))


table(data_percent$year)


print(p)

library(tidyr)
library(dplyr)

# Get full year range
all_years <- seq(min(data_percent$year), 2024, 1)  # Adjust end year

# Get all topics
all_topics <- unique(data_percent$topic)

# Create a full grid
full_grid <- expand.grid(year = all_years, topic = all_topics)

# Merge with your actual data
data_percent_complete <- full_grid %>%
  left_join(data_percent, by = c("year", "topic")) %>%
  mutate(Percentage = ifelse(is.na(Percentage), 0, Percentage))

summary(data_percent)
p <- ggplot(data_percent_complete, aes(x = year, y = Percentage, fill = topic)) +
  geom_area(position = "stack", color = NA) +
  scale_fill_brewer(palette = "Set2") +
  scale_y_continuous(
    labels = scales::percent,
    sec.axis = sec_axis(~.*max(total_articles$Total), name = "Number of articles")
  ) +
  scale_x_continuous(breaks = seq(min(data_percent_complete$year), max(data_percent_complete$year), 1)) +
  theme_minimal() +
  labs(x = "Period", y = "Percentage", title = "Topic Distribution Over Time") +
  theme(axis.text.x = element_text(angle = 90, hjust = 0.5))



data <- read_excel("heatmap2.xlsx")
library(tidyr)
data_long <- pivot_longer(data, cols = -year, names_to = "Topic", values_to = "Proportion")
ggplot(data_long, aes(x = Topic, y = factor(year), fill = Proportion)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "steelblue") +
  labs(x = "Topic", y = "year", title = "Topic Proportion Heatmap") +
  theme_minimal()

ggplot(data_long, aes(x = factor(year), y = Topic, fill = Proportion)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "darkblue", high = "maroon") +
  labs(x = "Year", y = "Topic", title = "Topic Proportion Heatmap") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for clarity



p + geom_line(data = total_articles, aes(x = Period, y = total / max(total)), 
              inherit.aes = FALSE, color = "black", linetype = "dotted", size = 0.7)


set.seed(831)
stm_eek <- estimateEffect(1:9~Year,
                         stmobj = airestmkw, 
                         metadata = out$meta, 
                         uncertainty="Global")
plot(x = stm_eek, 
     covariate = "Year", 
     topic = c(4),
     model = airestmkw,
     xlab = "Year",
     main = "what",
     method = "continuous",
     printlegend = FALSE,
     linecol = "red",
     xlim = c(2010,2025))

plot(x = stm_eek, 
     covariate = "Year", 
     topic = c(8),
     model = airestmkw,
     xlab = "Year",
     main = "Virtual/Augmented Reality Application",
     method = "continuous",
     printlegend = FALSE,
     linecol = "red")

summary(AIRETESTstm$Year)
table(AIRETESTstm$Year)
