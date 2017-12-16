library(dplyr)
library(corrplot)
library(ggplot2)

#### Generic helper functions ####

df.numeric.cols <- function(df)
  df.types.cols(df, types = c("numeric", "integer"))
  
df.types.cols <- function(df, types) {
  col.classes <- sapply(df, class)
  return(df[, names(col.classes)[col.classes %in% types]])
}

df.column.pairs <- function(df) {
  expand.grid(names(df), names(df)) %>%
    mutate(Var1 = as.character(Var1), Var2 = as.character(Var2)) %>%
    filter(Var1 > Var2)
}


#### Correlation pairwise variables ####

df.cor <- function(df) {
  df %>%
    df.numeric.cols() %>%
    cor(use = "pairwise.complete.obs")
}

df.corrplot <- function(df, mixed = T) {
  M <- df %>% df.cor()
  if (dim(M)[1] > 20) {
    colnames(M) <- NULL
  }
  if (mixed) {
    corrplot.mixed(M, tl.pos = "lt", diag = "l")
  } else {
    corrplot(M)
  }
}


#### Scatter charts pairwise variables ####

df.scatter <- function(df, jitter = 0.1) {
  df %>%
    df.numeric.cols() %>%
    df.column.pairs() %>%
    apply(1, function(row)
      ggplot(df) +
        aes_string(row[1], row[2]) +
        annotate("rect", fill = "lightgreen", alpha = 0.2,
                 xmin = mean(df[[row[1]]], na.rm = T) - 3 * sd(df[[row[1]]], na.rm = T),
                 xmax = mean(df[[row[1]]], na.rm = T) + 3 * sd(df[[row[1]]], na.rm = T),
                 ymin = mean(df[[row[2]]], na.rm = T) - 3 * sd(df[[row[2]]], na.rm = T),
                 ymax = mean(df[[row[2]]], na.rm = T) + 3 * sd(df[[row[2]]], na.rm = T)
                 ) +
        geom_point(aes(mean(df[[row[1]]], na.rm = T), mean(df[[row[2]]], na.rm = T)),
                   color = "darkgreen", shape = "+", size = 10) +
        geom_point() +
        geom_smooth(method = "lm") +
        geom_jitter(width = jitter)
    )
}

