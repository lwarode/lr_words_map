 ggplot2 theme I want to have roughly applied (please change slightly if it fits better with the visual goal of the App)
theme_academia <- function() {


  font <- "EB Garamond"
  # font <- "CMU Serif"
  # font <- "ComputerModern"
  # font <- "Times"
  # font <- "Corbel"

  # base theme
  theme_bw() %+replace%

    theme(

      text = element_text(family = font),
      legend.text = element_text(size = 9),
      legend.title = element_blank(),
      legend.box.background = element_rect(colour = "black", fill = "white", linetype = "solid"),
      # legend.position = c(0.85, 0.85),

      # grid lines
      panel.grid.major = element_line(color = "grey60", size = 0.2),
      panel.grid.minor = element_line(color = "grey80", size = 0.1),

      # faceting
      strip.background = element_blank(),
      strip.text = element_text(
        color = "black",
        # relative size of ƒacet titles
        size = rel(1),
        # margin of facet titles
        vjust = 1
      )

    )
}

# The exact party colors one should use 
party_colors <- c("#138BD8", "#000000", "#a4c8eb", "#FFEE0A", "#529222", "#AE1862", "#FF820A", "#E30019")
names(party_colors) <- c("AfD", "CDU", "CSU", "FDP", "Greens", "The Left", "PIRATEN", "SPD")

# Core plot function for Figure 5 and 6
lr_pos_sem_topwords_fun <- function(df, pos = "left", sem = "right", n = 10, sd_factor = 0.5, stopwords_exclusion = T) {
  if (!exists("stopwordsplus")) {
    stop("stopwords_plus not found in environment")
  }

  if (pos == "left" & sem == "right") {
    top_df <- df %>%
      filter(
        association_score > (mean(association_score, na.rm = T) + sd_factor * sd(association_score, na.rm = T)),
        lr_self_mean < (mean(lr_self_mean, na.rm = T) - sd_factor * sd(lr_self_mean, na.rm = T))
      )
    if (stopwords_exclusion) {
      top_df <- top_df %>%
        filter(!word %in% stopwordsplus)
    }
    top_words <- top_df %>%
      arrange(desc(total_freq)) %>%
      slice_head(n = n) %>%
      pull(word)
  }
  if (pos == "right" & sem == "right") {
    top_df <- df %>%
      filter(
        association_score > (mean(association_score, na.rm = T) + sd_factor * sd(association_score, na.rm = T)),
        lr_self_mean > (mean(lr_self_mean, na.rm = T) + sd_factor * sd(lr_self_mean, na.rm = T))
      )
    if (stopwords_exclusion) {
      top_df <- top_df %>%
        filter(!word %in% stopwordsplus)
    }
    top_words <- top_df %>%
      arrange(desc(total_freq)) %>%
      slice_head(n = n) %>%
      pull(word)
  }
  if (pos == "left" & sem == "left") {
    top_df <- df %>%
      filter(
        association_score < (mean(association_score, na.rm = T) - sd_factor * sd(association_score, na.rm = T)),
        lr_self_mean < (mean(lr_self_mean, na.rm = T) - sd_factor * sd(lr_self_mean, na.rm = T))
      )
    if (stopwords_exclusion) {
      top_df <- top_df %>%
        filter(!word %in% stopwordsplus)
    }
    top_words <- top_df %>%
      arrange(desc(total_freq)) %>%
      slice_head(n = n) %>%
      pull(word)
  }
  if (pos == "right" & sem == "left") {
    top_df <- df %>%
      filter(
        association_score < (mean(association_score, na.rm = T) - sd_factor * sd(association_score, na.rm = T)),
        lr_self_mean > (mean(lr_self_mean, na.rm = T) + sd_factor * sd(lr_self_mean, na.rm = T))
      )
    if (stopwords_exclusion) {
      top_df <- top_df %>%
        filter(!word %in% stopwordsplus)
    }
    top_words <- top_df %>%
      arrange(desc(total_freq)) %>%
      slice_head(n = n) %>%
      pull(word)
  }
  return(top_words)
}

# Only select and plot topwords
# highlight only specific associations
topword_plot_pos_sem_fun <- function(df, top_n = 50, rem_stopword = T, custom_words = NULL) {
  if (rem_stopword) {
    word_counts_left <- df %>%
      filter(str_length(word) > 2) %>%
      filter(!word %in% stopwordsplus) %>%
      arrange(desc(left_freq)) %>%
      select(word, left_freq, total_freq)

    word_counts_right <- df %>%
      filter(str_length(word) > 2) %>%
      filter(!word %in% stopwordsplus) %>%
      arrange(desc(right_freq)) %>%
      select(word, right_freq, total_freq)
  } else {
    word_counts_left <- df %>%
      filter(str_length(word) > 2) %>%
      arrange(desc(left_freq)) %>%
      select(word, left_freq, total_freq)

    word_counts_right <- df %>%
      filter(str_length(word) > 2) %>%
      arrange(desc(right_freq)) %>%
      select(word, right_freq, total_freq)
  }

  if (!is.null(custom_words)) {
    word_counts_left <- word_counts_left %>%
      filter(word %in% custom_words)
    word_counts_right <- word_counts_right %>%
      filter(word %in% custom_words)
  }

  top_df <- df %>%
    filter(word %in% c(word_counts_left$word[1:top_n], word_counts_right$word[1:top_n]))

  y_mean_global <- mean(df$association_score, na.rm = TRUE)
  y_mean_global

  x_mean_global <- mean(df$lr_self_mean, na.rm = TRUE)
  x_mean_global

  y_mean <- mean(top_df$association_score, na.rm = TRUE)
  y_mean

  x_mean <- mean(top_df$lr_self_mean, na.rm = TRUE)
  x_mean

  pl_top_df <- top_df %>%
    mutate(eng_word_transl = ifelse(eng_word_transl == "AfD", "afd", eng_word_transl)) %>%
    mutate(eng_word_transl = ifelse(eng_word_transl == "steer", "taxes", eng_word_transl)) %>%
    mutate(eng_word_transl = ifelse(word == "wirtschaft", "economy", eng_word_transl)) %>%
    ggplot(aes(x = lr_self_mean, y = association_score,
               label = paste0(word, " ('", eng_word_transl, "')"))) +
    theme_academia() +
    labs(
      x = "Left-Right Self-Placement (Mean)",
      y = "Left-Right Semantic Association Score (Left: -1, Right: +1)",
    ) +
    geom_hline(yintercept = y_mean, linetype = "dashed", colour = "grey20") +
    geom_hline(yintercept = y_mean_global, linetype = "dashed", colour = "grey60") +
    geom_vline(xintercept = x_mean, linetype = "dashed", colour = "grey20") +
    geom_vline(xintercept = x_mean_global, linetype = "dashed", colour = "grey60") +
    geom_point() +
    geom_text_repel(aes(family = "EB Garamond"), show.legend = F, max.overlaps = 42, max.time = 4.2) +
    scale_x_continuous(breaks = seq(2, 8, 1), limits = c(2, 8)) +
    scale_y_continuous(breaks = seq(-1, 1, .25), limits = c(-1.075, 1.075))

  return(pl_top_df)
}

top_50_global_left <- lr_pos_sem %>%
  filter(str_length(word) > 2) %>%
  filter(!word %in% stopwordsplus) %>%
  arrange(desc(left_freq)) %>%
  slice_head(n = 50) %>%
  pull(word)

top_50_global_right <- lr_pos_sem %>%
  filter(str_length(word) > 2) %>%
  filter(!word %in% stopwordsplus) %>%
  arrange(desc(right_freq)) %>%
  slice_head(n = 50) %>%
  pull(word)

# Neutral words plot function
neutral_term_lr_plot_fun <- function(df_left, df_right, term, output = "plot", mean_label = T, mean_dist = T, manual_transl = NULL) {
  # df_pl <- df %>%
  #   mutate(neutral_lr = case_when(
  #     str_detect(left_desc, term) ~ "Left",
  #     str_detect(right_desc, term) ~ "Right",
  #     str_detect(left_desc, term) & str_detect(right_desc, term) ~ "Left and Right",
  #     T ~ NA,
  #   )) %>%
  #   filter(str_detect(left_desc, term) | str_detect(right_desc, term))
  df_pl_left <- df_left %>%
    filter(left_words == term) %>%
    mutate(neutral_lr = "Left") %>%
    select(word = left_words, lr_self, neutral_lr)

  df_pl_right <- df_right %>%
    filter(right_words == term) %>%
    mutate(neutral_lr = "Right") %>%
    select(word = right_words, lr_self, neutral_lr)

  df_pl <- rbind(
    df_pl_left,
    df_pl_right
  )

  left_mean <- df_pl_left %>% summarise(mean_lr = mean(lr_self, na.rm = T)) %>% pull()
  right_mean <- df_pl_right %>% summarise(mean_lr = mean(lr_self, na.rm = T)) %>% pull()

  if (is.null(manual_transl)) {
    title_term <- paste0(
      term,
      " ('",
      polyglotr::google_translate(term, "en", "de"),
      "')"
    )
  } else {
    title_term <- paste0(
      term,
      " ('",
      manual_transl,
      "')"
    )
  }

  pl <- df_pl %>%
    ggplot(aes(x = lr_self, fill = neutral_lr)) +
    scale_fill_manual(name = "Association\nwith:", values = c("red", "blue")) +
    geom_density(alpha = .35) +
    geom_vline(xintercept = left_mean, linetype = 2, color = "red", alpha = .7) +
    geom_vline(xintercept = right_mean, linetype = 2, color = "blue", alpha = .7) +
    scale_x_continuous(breaks = seq(1, 11, 1), limits = c(1, 11)) +
    theme_academia() +
    theme(
      axis.ticks.y = element_blank(),
      axis.text.y = element_blank(),
      legend.title = element_text(),
      plot.title = element_text(size = 20),
    ) +
    labs(title = title_term, x = "Left-Right Self-Placement", y = NULL)

  if (mean_label) {
    if (left_mean < right_mean) {
      pl <- pl + annotate(
        geom = "text",
        family = "EB Garamond",
        x = left_mean - 1,
        y = -0.0085,
        label = paste0("bar(x) == ", round(left_mean, 2)),
        parse = T,
        color = "red",
        size = 5
      ) +
        annotate(
          geom = "text",
          family = "EB Garamond",
          x = right_mean + 1,
          y = -0.0085,
          label = paste0("bar(x) == ", round(right_mean, 2)),
          parse = T,
          color = "blue",
          size = 5
        )
    } else if (left_mean > right_mean) {
      pl <- pl + annotate(
        geom = "text",
        family = "EB Garamond",
        x = left_mean + 1,
        y = -0.0085,
        label = paste0("bar(x) == ", round(left_mean, 2)),
        parse = T,
        color = "red",
        size = 5
      ) +
        annotate(
          geom = "text",
          family = "EB Garamond",
          x = right_mean - 1,
          y = -0.0085,
            label = paste0("bar(x) == ", round(right_mean, 2)),
          parse = T,
          color = "blue",
          size = 5
        )

    }

  }

  if (mean_dist) {
    pl <- pl + annotate(
      geom = "segment",
      x = left_mean,
      xend = right_mean,
      y = 0.0085,
      color = "black",
      linetype = 1,
      size = 0.35,
      # arrow = arrow(ends = "both", type = "closed", length = unit(0.25, "cm"))
    ) +
      annotate(
        geom = "text",
        family = "EB Garamond",
        x = (left_mean + right_mean) / 2,
        y = 0.017,
        label = paste0("Dist.: ", round(abs(left_mean - right_mean), 2)),
        color = "black",
        size = 5
      )
    }

  if (output == "plot") {
    return(pl)
  } else if (output == "df") {
    return(df_pl)
  }
}

# Figure 5 from Paper
ggsave(
  plot = topword_plot_pos_sem_fun(lr_pos_sem, 35) ,
  "figures/pl_top_35_lr_pos_sem_APA.png",
  width = 9,
  height = 9,
  dpi = 300
)


# Figure 6 from Paper
# quadrantic representations
pl_top_15_quadrant_lr_pos_sem <- topword_plot_pos_sem_fun(
  df = lr_pos_sem %>%
    mutate(
      eng_word_transl = ifelse(eng_word_transl == "AfD", "afd", eng_word_transl),
      eng_word_transl = ifelse(eng_word_transl == "steer", "taxes", eng_word_transl),
      eng_word_transl = ifelse(word == "wirtschaft", "economy", eng_word_transl),
      eng_word_transl = ifelse(word == "heimat", "home[land]", eng_word_transl),
      eng_word_transl = ifelse(word == "bürgerlich", "bourgeios", eng_word_transl)
      ),
  custom_words = c(
    lr_pos_sem_topwords_fun(lr_pos_sem, pos = "left", sem = "right", n = 15, sd_factor = 0.5),
    lr_pos_sem_topwords_fun(lr_pos_sem, pos = "right", sem = "right", n = 15, sd_factor = 0.5),
    lr_pos_sem_topwords_fun(lr_pos_sem, pos = "left", sem = "left", n = 15, sd_factor = 0.5),
    lr_pos_sem_topwords_fun(lr_pos_sem, pos = "right", sem = "left", n = 15, sd_factor = 0.5)
  )
)

# Figure 7 from Paper (but only visualize the one selected word!)
pl_top_6_neutral_lr <-
  neutral_term_lr_plot_fun(df_exp_left, df_exp_right, "staat") +
  neutral_term_lr_plot_fun(df_exp_left, df_exp_right, "menschen") +
  neutral_term_lr_plot_fun(df_exp_left, df_exp_right, "politik", manual_transl = "politics/policy") +
  neutral_term_lr_plot_fun(df_exp_left, df_exp_right, "freiheit") +
  neutral_term_lr_plot_fun(df_exp_left, df_exp_right, "wirtschaft", manual_transl = "economy") +
  neutral_term_lr_plot_fun(df_exp_left, df_exp_right, "verantwortung") +
  plot_layout(guides = "collect", axes = "collect_y", nrow = 2, axis_titles = "collect")

pl_top_6_neutral_lr

ggsave(
  plot = pl_top_6_neutral_lr,
  "figures/pl_top_6_neutral_lr_APA.png",
  width = 13,
  height = 7.5,
  dpi = 600
)

