# Introduction

## Gender measure

In social science, gender is typically measured by asking individuals to classify 
themselves in binary categories (woman/man, with non-binary as an alternative). This 
measurement presents two major limitations. First, this measurement causes an 
inclusion issue since it does not take into account gender minorities that identify with either of them (Bauer et al, 2017; Bondé, 2017); second, this measurement should give insights into gender behavior but this assumption essentialized gender roles and missed all individuals who would differ in their assigned roles or behavior (Cameron & Stinson, 2019). Despite these limits, analyzing gender trends remains necessary to highlight inequality, as it is a consequence of the social structure. Therefore, it is important to find other indicators than the measure of women/men. 


# purpose of my research

This study proposes an alternative approach: rather than using gender identity as a predictor of political attitudes, I use attitudes about gender roles and norms to predict attitudes towards politically sensitive topics. This shift from “who you are” to “what you believe” offers a more nuanced and inclusive framework. For example, Alvarez and Parini found that some political attitudes predict better behavior than the gender binary indicator (2005). So, for this project, I propose to focus instead of the traditional approach that analyzes the individual’s identity and experience to predict attitude, to use the attitude  already constructed by the individuals to predict behavior or other political attitudes.


# Method

## Theoretical framework

### Item selection for the gender ideology scale[^note1]

This study develops a gender ideology scale to predict attitudes towards politically 
sensitive topics by using gender indicators.* From the MOSAiCH survey, I focus on gender ideology attitude that has a clear link with political opinion. Therefore, in questionnaire 1, I selected items (FAM1a to FAM14; FAM21 to FAM27) measuring gender role attitudes (Glick & Fiske, 1996; 1999), family form, meaning of marriage and children (FSO, 2019, World Values Survey 2021); and in questionnaire 2, I selected items (FAMS9_wp2 to FAMS32-wp2). Measuring gender stereotypes, sexism (Swim et al., 1995; Levant et al, 2013, Touglas et al., 1995; Walter, 2018; Zehnter et alm 2021), and homophobia (Chonody, 2013). Finally, I added the items of conservatism vs progressivism (items: FAMS45a_wp2 to FAMS46c_wp2) that may mediate political attitude (Niessen et al, 2019). I chose this panel to have representative elements that constitute the perception of the social rules of society in the sense of how men and women should behave, what constitutes a family and love, and the possibility of changing these criteria. 

[^note1]: As a 2-week miniproject, I largely selected the items by relevant theme. For an official project, a selection more literature-grounded would be needed (plus additional coders to validate the selection).

### Dependent variables

To assess the predictability of this scale, I decided to use the items of attitudes towards Assisted Reproductive Technologies (ART) in Switzerland because it is a complex political subject that touches indirectly the attitude about familial shape, sexual orientation, beyond the tradition, morality, and economy (Büchler 2017; Yamatoto et al., 2018 ; Kostenzer et al., 2021). These variables are also interesting because Daniluk and Koert observed a nuanced attitude between the differences without explaining the absence of a correlation link with gender or the knowledge of these technologies (2012; 2013). 

So, this study, by using a gender ideology scale instead of only binary gender identity, could better explain the difference among the individuals with control variables including 1) knowledge of ART, 2) parental status, and 3) intention to have children. 

In terms of scale, in addition to the gender indicator measurement and the gender ideology attitude, the socio-demographic information also needs to be taken into consideration, as it also influences gender practice. For example, Salah et al observed that in Switzerland, the housework sharing was more explained by the economic capital than by the individual’s ideology (2017). 

So, this study will fill a gap concerning the definition of political attitude using gender indicators. and explain the difference in adhesion for ART. 


## Research question 

To what extent do gender ideology attitudes predict attitudes towards Assisted Reproductive Technologies (ART) in Switzerland, beyond the explanatory power of gender and socio-demographic factors? 

Specifically, this study aims to:
1) develop and validate a multidimensional gender ideology scale; 
2) compare the predictive power of gender ideologies vs gender vs socio-demographic variables; 
3) assess whether gender ideology mediates the relationship between gender and ART attitude. 

### Hypothesis

The hypothesis is that Gender ideology factors will explain significantly more variance than binary gender alone, and will retain significant predictive power even after controlling for socio-demographic and ART control variables. 


## Modeling data: 

This project is divided into two parts: 

1)constructs a political attitude scale; 
2) comparison of this scale to other measurements (gender and politico-socio
demographic variables) to observe how this scale predicts the Technology of assisted 
reproduction. 

In this document, I focus on the first part. 

I use some r packages: (explain why)

* haven (to read data as they are in .sav)
* labelled (to manage NA according to user-defined missing values)
* ggplot2 (general graphic visualization)
* dplyr, tidyr, purrr (to manipulate, reshape, and summarise the data)
* polycor (to calculate polychoric correlations between ordinal Likert items)
* psych (to compute the KMO sampling adequacy test, run the exploratory 
  factor analysis, and calculate Cronbach's alpha for internal consistency)



## sample

The total sample from participants who responded to both questionnaires 
(N=1684) was randomly split into two independent subsamples, one for exploratory factor analysis (EFA, n=842) and one reserved for confirmatory factor analysis (CFA, n = 842). 
This cross-validation approach ensures that the factorial structure identified is not due to overfitting. 




## data cleaning

To determine my gender ideology scale, I selected the themes based on the literature that I mentioned and filtered the questions with only ordinal scales of 4 to 7 levels for the simplicity of the analysis. A first filter has been apply to make sure only participant who join both questionnaires are selected.

```{r}


# import dataset
library(haven)
dataset_sav <- read_sav("2161_MOSAiCH2022_Data_E_v1.0.sav")
df <- data.frame(dataset_sav)

# select participants 
df_complet <- df %>%
  filter(!is.na(complr_wp2))

# make sure the number of participants is right: (should be 1684)
cat("Nombre de répondants complets:", nrow(df_complet), "\n")

# attribute NA
library(labelled)
df_complet <- df_complet %>%
  mutate(across(everything(), ~ user_na_to_na(.)))

# manual selection from the Item selection for the gender ideology scale mentionned above:
famille_vars <- df_complet %>% 
  select(matches("^FAM([1-9][a-z]?|1[0-4][a-z]?|2[1-7][a-z]?)$")) %>%
  mutate(across(everything(), ~ as_factor(., ordered = TRUE)))

ideologie_vars <- df_complet %>% 
  select(matches("^FAMS([9]|[1-2][0-9]|3[0-2])_wp2$")) %>%
  mutate(across(everything(), ~ as_factor(., ordered = TRUE)))

tradition_vars <- df_complet %>% 
  select(matches("^FAMS(4[5-6])[a-z]?_wp2$")) %>%
  mutate(across(everything(), ~ as_factor(., ordered = TRUE)))

# --- Combiner tous les items ---
all_items <- bind_cols(
  famille_vars,
  ideologie_vars,
  tradition_vars
)

cat("total items:", ncol(all_items), "\n")
# total items: 73

# SPLIT EFA / CFA 
set.seed(123)
n <- nrow(all_items)
indices <- sample(1:n, size = n, replace = FALSE)
half_n <- floor(n / 2)

all_items_efa <- all_items[indices[1:half_n], ]
all_items_cfa <- all_items[indices[(half_n + 1):n], ]

cat("EFA sample: n =", nrow(all_items_efa), "\n")
cat("CFA sample : n =", nrow(all_items_cfa), "\n")

# select only scale from scale of 4 to 7 for simplify the calculation and use hetcore().
all_items_efa <- all_items_efa %>%
  select(where(~ n_distinct(., na.rm = TRUE) >= 4 & n_distinct(., na.rm = TRUE) <= 7))
#n_dstrict = more consise equivalent to nrow(unique)
# ~ in tidyverse means function(x) n_distinct(x)
# "." very tested collumn (like x)
cat("Total number of items after filtering:", ncol(all_items_efa), "\n")



```


Data cleaning involved two criteria:
1) Items whose standard deviation fell below 30% of the theoretical maximum standard deviation (assuming a uniform distribution across response categories) were removed due to insufficient variability.
2) Items with polychoric correlations* > 0.85 were examined for redundancy, retaining only the most theoretically relevant item from each highly correlated pair. 

* Classical correlation assumes continuous variables. This is not the case with Likert scales so I use polychoric correlations to solve this issue. Polychoric correlations estimate various possible values of correlation and selects the one that would make the observed contingency table the most likely. Furthermore, items with insufficient variability (where responses were heavily concentrated on one or two response categories) were excluded, as they provide only limited ability to distinguish between respondents and undermine the estimation of polychoric correlations.




```{r}
# Identify with weak variance
library(dplyr)
library(tidyr)
library(purrr)
library(polycor)



identify_weak_variance <- function(data) {
  
  relative_variance <- data %>%
    reframe(across(everything(), function(x) {
      n_unique <- length(unique(na.omit(x)))
      max_val <- max(as.numeric(x), na.rm = TRUE)
      min_val <- min(as.numeric(x), na.rm = TRUE)
      range_val <- max_val - min_val
      sd_val <- sd(as.numeric(x), na.rm = TRUE)
      
      var_max_theoric <- (range_val^2) / 12
      sd_max_theoric <- sqrt(var_max_theoric)
      ratio <- if (sd_max_theoric > 0) sd_val / sd_max_theoric else 0
      
      tibble(
        sd = sd_val,
        n_categories = n_unique,
        range = range_val,
        ratio_variance = ratio
      )
    })) %>%
    pivot_longer(everything(), names_to = "item") %>%
    unnest(cols = value)
  
  weak_variance <- relative_variance %>%
    filter(ratio_variance < 0.30) %>%
    arrange(ratio_variance)
  
  return(list(
    details = relative_variance,
    weak_items = weak_variance
  ))
}


cat("\n=== VARIANCE ANALYSIS ===\n")
result_variance <- identify_weak_variance(all_items_efa)


library(labelled)

# extract the labels from the .sav file
labels_map <- var_label(df_complet)



if(nrow(result_variance$weak_items) > 0) {
  print(result_variance$weak_items)
  
  # Show with labels
  items_weak_var_details <- result_variance$weak_items %>%
    mutate(
      Label = labels_map[item],
      Label = ifelse(is.na(Label), item, Label)
    ) %>%
    select(item, Label, ratio_variance, sd, n_categories, range)
  
  print(items_weak_var_details)
  
  items_weak_variance <- result_variance$weak_items$item
  cat("\nNumber of items with low variance:", length(items_weak_variance), "\n")
  
} else {
  cat("No items with low variance were detected.\n")
  items_weak_variance <- character(0)
}

# Remove items with weak variance
all_items_clean <- all_items_efa %>%
  select(-any_of(items_weak_variance))

cat("\nItems after cleaning:", ncol(all_items_clean), "\n")



# correlation matrix
cat("\n=== CALCULATION OF POLYCHORIC CORRELATIONS ===\n")


# Calculate polychoric correlations
poly_cor <- hetcor(all_items_clean, use = "pairwise.complete.obs")
cor_matrix <- poly_cor$correlations

# save
saveRDS(cor_matrix, "correlation_matrix.rds")



# identify redundancy

correlated_pairs <- function(cor_matrix, threshold = 0.70) {
  
  high_cor_indices <- which(abs(cor_matrix) > threshold & 
                             abs(cor_matrix) < 1, 
                           arr.ind = TRUE)
  
  if(length(high_cor_indices) == 0) {
    cat("no correlation >", threshold, "\n")
    return(NULL)
  }
  
  pairs <- tibble(
    Item1 = rownames(cor_matrix)[high_cor_indices[,1]],
    Item2 = colnames(cor_matrix)[high_cor_indices[,2]],
    Correlation = cor_matrix[high_cor_indices]
  )
  
  
  # remove duplicates
  pairs <- pairs %>%
    filter(Item1 < Item2) %>%
    arrange(desc(abs(Correlation)))
  
  # add label
  pairs$Label1 <- ifelse(
    is.na(labels_map[pairs$Item1]), 
    pairs$Item1,
    labels_map[pairs$Item1]
  )
  pairs$Label2 <- ifelse(
    is.na(labels_map[pairs$Item2]), 
    pairs$Item2,
    labels_map[pairs$Item2]
  )
  
  return(pairs)
}


cat("\n=== PAIRS WITH A VERY STRONG CORRELATION (r > 0.85) ===\n")
pairs_085 <- correlated_pairs(cor_matrix, 0.85)

if(!is.null(pairs_085)) {
  cat("Number of pairs found:", nrow(pairs_085), "\n\n")
  print(pairs_085 %>% select(Item1, Item2, Correlation), n = 50)
} else {
  cat("no pair > 0.85\n")
}

# which one to remove ?

suggest_remove <- function(pairs_df, cor_matrix) {
  
  if(is.null(pairs_df) || nrow(pairs_df) == 0) {
    return(NULL)
  }
  
  suggestions <- pairs_df %>%
    rowwise() %>%
    mutate(
      # Average correlation with all other items
      mean_cor_item1 = mean(abs(cor_matrix[Item1, 
                                           !colnames(cor_matrix) %in% c(Item1, Item2)]), 
                           na.rm = TRUE),
      mean_cor_item2 = mean(abs(cor_matrix[Item2, 
                                           !colnames(cor_matrix) %in% c(Item1, Item2)]), 
                           na.rm = TRUE),
      
      Item_keep = ifelse(mean_cor_item1 > mean_cor_item2, Item1, Item2),
      Item_remove = ifelse(mean_cor_item1 > mean_cor_item2, Item2, Item1),
      
      Reason = sprintf("mean_r_kept = %.3f vs mean_r_removed = %.3f", 
                      pmax(mean_cor_item1, mean_cor_item2),
                      pmin(mean_cor_item1, mean_cor_item2))
    ) %>%
    ungroup()
  
  return(suggestions)
}

suggestions_085 <- NULL

if(!is.null(pairs_085)) {
  suggestions_085 <- suggest_remove(pairs_085, cor_matrix)
  
  if(!is.null(suggestions_085)) {
    cat("\n=== REMOVAL SUGGESTIONS (r > 0.85) ===\n")
    print(suggestions_085 %>% 
          select(Item_remove, Item_keep, Correlation, Reason))
  }
}


# FINAL REMOVAL LIST

items_suggested_removal <- character(0)

if(!is.null(suggestions_085)) {
  items_suggested_removal <- suggestions_085 %>%
    pull(Item_remove) %>%
    unique()
  
  cat("\n=== ITEMS SUGGESTED FOR REMOVAL ===\n")
  cat("Total:", length(items_suggested_removal), "items\n\n")
  
  removal_details <- data.frame(
    Item = items_suggested_removal,
    Label = ifelse(is.na(labels_map[items_suggested_removal]), 
                  items_suggested_removal,
                  labels_map[items_suggested_removal])
  )
  print(removal_details)
} else {
  cat("\n=== NO REDUNDANT ITEMS (r > 0.85) ===\n")
}


# final dataset:

all_items_final <- all_items_clean %>%
  select(-any_of(items_suggested_removal))


cat("Initial items:                 ", ncol(all_items_efa), "\n")
cat("Removed (low variance):        ", length(items_weak_variance), "\n")
cat("Removed (redundancy r>0.85):   ", length(items_suggested_removal), "\n")
cat("Final items:                   ", ncol(all_items_final), "\n")
cat("\nPercentage retained:", 
    round(100 * ncol(all_items_final) / ncol(all_items_efa), 1), "%\n")



# Note that the redundancy-removal procedure evaluates highly correlated item pairs independently; it does not fully account for potential correlation chains (e.g., item A highly correlated with B, and B with C, without A and C being directly correlated above threshold), which could be verified through iterative or graph-based redundancy checks in a more exhaustive analysis.


```


The KMO tests validated the data for an EFA, so I did a factorial analysis with the “oblimin” rotation and the “pa” method because of the non-normal distribution and the likert scale nature. After comparing the model with different factors, the 4-factor solution provided the best fit to the data. Finally, after loading an analysis of < 0.4 and a qualitative analysis by theme consistency, 18 items remain in 4 factors (F1 = Gender behavior, F2 = Family structure (2 items reverse), F3 = conservationism, F4 = attitude towards women.The 4 factors explain 59% of the total variance, with factors loading more than 0.6, indicating a good representation of the items by their factors. The communality varies between 0.32 to 0.78, acceptable in an exploratory study. Overall, the adjustment indices indicated an acceptable modelisation with a low RMSR (0.03), TLI close to the 0.9 threshold (TLI = 0.895), and RMSEA of 0.082, which corresponds to an acceptable adjustment. The factor correlation (0.47-0.66) shows that the identified dimensions are linked but distinct, fitting the Oblimin rotation. For each factor, the alpha chronach was between > 0.8 and < 0.9, showing a good interne consistency without redundant variables.


```{r}
library(psych)


# STEP 1: EXPLORATORY EFA

cor_matrix_candidates <- hetcor(all_items_final, use = "pairwise.complete.obs")$correlations

# KMO test on the full candidate set 
kmo_result <- KMO(cor_matrix_candidates)
kmo_result
cat("\nOverall MSA:", round(kmo_result$MSA, 3), "\n")

# Exploratory factor analysis, 4-factor solution
efa_candidates <- fa(r = cor_matrix_candidates,
                      nfactors = 4,
                      n.obs = nrow(all_items_final),
                      rotate = "oblimin",
                      fm = "pa")

print(efa_candidates$loadings, cutoff = 0.4)

# --> Inspect loadings here: identify items with loading < 0.4 on every factor,
# and items cross-loading on multiple factors, or thematically inconsistent
# with their assigned factor. This qualitative + statistical review determines
# which items are retained below.


#FINAL ITEM SET RETAINED AFTER REVIEW


final_items <- c(
  # F1 = Gender behavior
  "FAMS14_wp2", "FAMS11_wp2", "FAMS12_wp2", "FAMS15_wp2", "FAMS17_wp2",
  # F2 = Family structure
  "FAM1b", "FAM1c", "FAM5b", "FAM1a",
  # F3 = Conservationism
  "FAMS45b_wp2", "FAMS45c_wp2", "FAMS45a_wp2",
  # F4 = Attitude towards women
  "FAMS25_wp2", "FAMS29_wp2", "FAMS26_wp2", "FAMS24_wp2", "FAMS27_wp2", "FAMS13_wp2"
)

all_items_efa_final <- all_items_final %>%
  select(all_of(final_items))

cat("Number of items in final model:", ncol(all_items_efa_final), "\n")

cor_matrix_final <- hetcor(all_items_efa_final, use = "pairwise.complete.obs")$correlations


# STEP 3: FINAL EFA ON REFINED 18-ITEM SET

efa_result <- fa(r = cor_matrix_final, 
                  nfactors = 4, 
                  n.obs = nrow(all_items_efa_final),
                  rotate = "oblimin",
                  fm = "pa")

print(efa_result$loadings, cutoff = 0.4)
efa_result$communality
efa_result$Vaccounted

cat("RMSR:", efa_result$rms, "\n")
cat("TLI:", efa_result$TLI, "\n")
cat("RMSEA:", efa_result$RMSEA[1], "\n")
efa_result$Phi   # factor correlations


# LOADINGS PLOT BY THEMATIC DIMENSION

library(ggplot2)
library(dplyr)
library(tidyr)

loadings_df <- as.data.frame(unclass(efa_result$loadings))
loadings_df$item <- rownames(efa_result$loadings)

loadings_long <- loadings_df %>%
  pivot_longer(
    cols = starts_with("PA"),
    names_to = "Factor",
    values_to = "Loading"
  ) %>%
  filter(abs(Loading) > 0.4) %>%
  mutate(
    Factor = recode(Factor,
      "PA1" = "F1 = Gender behavior",
      "PA2" = "F2 = Family structure",
      "PA3" = "F3 = Conservationism",
      "PA4" = "F4 = Attitude towards women"
    )
  )

ggplot(loadings_long, aes(x = reorder(item, Loading), y = Loading, fill = Factor)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ Factor, scales = "free_y") +
  coord_flip() +
  scale_fill_brewer(palette = "Dark2") +
  theme_minimal(base_size = 13) +
  labs(
    title = "Factor loadings by thematic dimension",
    x = "Items",
    y = "Loadings"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 15, hjust = 0.5),
    strip.text = element_text(face = "bold", size = 12),
    panel.grid.minor = element_blank()
  )


# CRONBACH'S ALPHA PER FACTOR

alpha_by_factor <- loadings_long %>%
  group_by(Factor) %>%
  group_map(~ psych::alpha(all_items_efa_final[, .x$item])$total$raw_alpha)

alpha_by_factor
```


### detail Items


The plot's code is from the website [tidytextmining.com](https://www.tidytextmining.com/tidytext.html)


# Plan for the second part of the project

The plan of this mini project is, then, to validate and adapt this scale if needed. For that, convergent and discriminant validity will be assessed before the Confirmatory factor analysis. In addition, to improve the feasibility of this scale, it would be interesting to propose a short scale by either reducing the items or proposing to separate the dimensions depending on what you need to study. Following CFA validation, three hierarchical regression models will be compared to assess the predictive power of gender ideologies: model 1 (Baseline): ART attitude ~ Gender; model 2 (gender Ideology Factors (F1 – F4) + gender; model 3 (full): ART attitude ~ Gender Ideology + gender + socio demographics + ART control variables. Model comparison will use ΔR² and F-tests to quantify the unique contribution of gender ideologies beyond sex and socio-demographic factors. Dominance analysis (Luchman, 2021) will decompose the variance explained by each predictor.


# Contribution

This study makes two theoretical contributions to gender analysis by 1) shifting from identity-based to belief-based gender indicators. This approach overcomes limitations of binary gender measurement and offers a more inclusive framework; 2) demonstrates that gender ideologies are stronger predictors of political attitude than binary gender identity. This experiment also contributes to public service communication by improving strategy and effective messages by focusing on individuals’ gender ideologies.

This mini project can be adapted into a doctoral thesis through three avenues: 1) by deepening the analysis of the relationship among different dimensions of gender ideology, and by including the question of gender identities. This could build on reviewing existing research while incorporating an innovative large-scale exploratory approach (e.g an unstructured or open-ended analysis) to better understanding how these attitude interact; 2) in the same direction, analysis whether gender ideology mediates the relationship between socio-demographic factors or study theses gender indicators in non-familial context ; 3) Cross-national validation across countries using ESS 11 or World Values survey data to assess cultural generalizability or longitudinal tracking from the FNS data.


# bibliography
* Blondé, J., Gianettoni, L., & Gross, D. (n.d.). Measurement of Sexism, Gender Identity, and Perceived Gender Discrimination: A Brief Overview and Suggestions for Short Scales.
* Bauer, G. R., Braimoh, J., Scheim, A. I., & Dharma, C. (2017). Transgender-inclusive measures of sex/gender for population surveys: Mixed-methods evaluation and recommendations. PLOS ONE, 12(5), e0178043. https://doi.org/10.1371/journal.pone.0178043
* Bornatici C., Felder M., Gianettoni L., Mordasini R., & Steinmetz S. (2025). Measuring assigned sex, gender identity, and sexual orientation in population surveys. FORS Guides, 26, Version 1.0, 1-44. https://doi.org/10.24449/FG-2025-00026
* Büchler 2017 : Büchler, A. (2017). Reproduktive Autonomie und Selbstbestimmung. Dimensionen, Umfang und Grenzen an den Anfängen menschlichen Lebens (2. Auflage). Basel: Helbing Lichtenhahn Verlag. 
* Cameron, J. J., & Stinson, D. A. (2019). Gender (mis)measurement: Guidelines for respecting gender diversity in psychological research. Social and Personality Psychology Compass, 13(11), e12506. https://doi.org/10.1111/spc3.12506
* Chonody (2013) : Chonody J. M. (2013). Measuring sexual prejudice against gay men and lesbian women: Development of the Sexual Prejudice Scale (SPS). Journal of Homosexuality, 60(6), 895–926. doi:10.1080/00918369.2013.774863 
* Glick, P., & Fiske, S. T. (1996). The Ambivalent Sexism Inventory: Differentiating hostile and benevolent sexism. Journal of Personality and Social * Psychology, 70(3), 491-512. https://doi.org/10.1037/0022-3514.70.3.491 
* Glick, P., & Fiske, S. T. (1999). The Ambivalence toward Men Inventory: Differentiating hostile and benevolent beliefs about men. Psychology of Women Quarterly, 23(3), 519 536. https://doi.org/10.1111/j.1471-6402.1999.tb00379.x
(FSO, 2019 : FSO (Federal Statistical Office) (2019). Enquête sur les familles et les générations 2018 – Questionnaire. 
* Kostenzer, J., A. M.E. Bos, A. de Bont and J. van Exel (2021). Unveiling the Controversy on Egg Freezing in The Netherlands: A Q-Methodology Study on Women’s Viewpoints. Reproductive Biomedicine & Society Online 12: 32-43. 
* Luchman, J. N. (2021). Determining relative importance in Stata using dominance analysis: Domin and domme. The Stata Journal, 21(2), 510–538. https://doi.org/10.1177/1536867X211025837
* Nießen, D., Schmidt, I., Beierlein, C., & Lechner, C. M., 2019: Nießen, D., Schmidt, I., Beierlein, C., & Lechner, C. M. (2019). Authoritarianism Short Scale (KSA-3). Zusammenstellung sozialwissenschaftlicher Items und Skalen (ZIS). https://doi.org/10.6102/zis272 
* Salah, H. B., Wernli, B., & Henchoz, C. (2017). Les nouvelles masculinités en Suisse: Une approche par l’idéologie de genre et la répartition du travail rémunéré et non rémunéré au sein des couples. In Enfances, Familles, Générations. Enfances, Familles, Générations. https://doi.org/10.7202/1041058ar
* Tougas et al., 1995 : Tougas, F., Brown, R., Beaton, A. M., & Joly, S. (1995). 
* Walter, J. G. (2018). Measures of gender role attitudes under revision: The example of the German General Social Survey. Social Science Research, 72, 170-182. doi:10.1016/j.ssresearch.2018.02.009
* Yamamoto et al. 2018 (Q15): Yamamoto, N., T. Hirata, G. Izumi, A. Nakazawa, S. Fukuda, K. Neriishi, et al. (2018). A survey of public attitudes towards third-party reproduction in Japan in 2014. PLoS One 13(10): e0198499. 
* Zehnter et al., 2021 : Zehnter, M. K., Manzi, F., Shrout, P. E., & Heilman, M. E. (2021). Belief in sexism shift: Defining a new form of contemporary sexism and introducing the belief in sexism shift scale (BSS scale). PloS one, 16(3), e0248374. https://doi.org/10.1371/journal.pone.0248374

