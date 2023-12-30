library(tidyverse)
library(shadowtext)

theme_set(theme_void(base_size = 6))


dat <- read_tsv("cluster-stats.tsv", col_names = FALSE) |> 
  rename(
    cluster = X1,
    size = X2, 
    age = X3,
    males = X4,
    females = X5,
    hr_ihd = X6,
    hr_ihd_pval = X7,
    hr_oth = X8,
    hr_oth_pval = X9
  )

dat <- dat |> 
  separate_wider_delim(
    cols = age, delim = " ", names = c("age", "age_sd")
  ) |> 
  mutate(age = as.numeric(age))

pdat <- dat |> 
  mutate(
    id = row_number() - 1,
    col = id %% 11,
    row = 2 - (id %/% 11),
    sex = males / (males + females)
  )  |> 
  filter(cluster != "NA*")

psize <- pdat |> 
  ggplot(aes(col, row, fill = size)) +
  geom_tile(color = "white", linewidth = 1) +
  geom_text(aes(label = cluster)) +
  coord_equal() +
  scale_fill_viridis_c(
    guide = guide_colorbar(direction = "horizontal")  
  ) +
  theme(legend.position = "bottom")

page <- pdat |> 
  ggplot(aes(col, row, fill = age)) +
  geom_tile(color = "white", linewidth = 1) +
  geom_text(aes(label = cluster)) +
  coord_equal() +
  scale_fill_viridis_c(
    guide = guide_colorbar(direction = "horizontal")  
  ) +
  theme(legend.position = "bottom")
  
psex <- pdat |> 
  ggplot(aes(col, row, fill = sex)) +
  geom_tile(color = "white", linewidth = 1) +
  geom_text(aes(label = cluster)) +
  coord_equal() +
  scale_fill_gradient2(
    midpoint = 0.5,
    guide = guide_colorbar(direction = "horizontal")  
  ) +
  theme(legend.position = "bottom")

pihd <- pdat |> 
  ggplot(aes(col, row, fill = hr_ihd)) +
  geom_tile(color = "white", linewidth = 1) +
  geom_text(aes(label = cluster)) +
  coord_equal() +
  scale_fill_gradient2(
    midpoint = 1,
    guide = guide_colorbar(direction = "horizontal")  
  ) +
  theme(legend.position = "bottom")

poth <- pdat |> 
  ggplot(aes(col, row, fill = hr_oth)) +
  geom_tile(color = "white", linewidth = 1) +
  geom_text(aes(label = cluster)) +
  coord_equal() +
  scale_fill_gradient2(
    midpoint = 1,
    guide = guide_colorbar(direction = "horizontal")  
  ) +
  theme(legend.position = "bottom")



W <- 3
H <- 2
S <- 3

ggsave("plots/clusters-with-age.svg", plot = page, 
       width = W, height = H, scale = S, unit = "cm")
ggsave("plots/clusters-with-sex.svg", plot = psex,
       width = W, height = H, scale = S, unit = "cm")
ggsave("plots/clusters-with-ihd.svg", plot = pihd,
       width = W, height = H, scale = S, unit = "cm")
ggsave("plots/clusters-with-oth.svg", plot = poth,
       width = W, height = H, scale = S, unit = "cm")
ggsave("plots/clusters-with-siz.svg", plot = psize,
       width = W, height = H, scale = S, unit = "cm")
