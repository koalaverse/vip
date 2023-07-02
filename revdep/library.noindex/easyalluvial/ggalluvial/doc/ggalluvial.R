## ----setup, echo=FALSE, message=FALSE, results='hide'-------------------------
library(ggalluvial)
knitr::opts_chunk$set(fig.width = 6, fig.height = 4, fig.align = "center")

## ----example alluvial plot using Titanic dataset, echo=FALSE------------------
ggplot(data = to_lodes_form(as.data.frame(Titanic),
                            key = "Demographic",
                            axes = 1:3),
       aes(x = Demographic, stratum = stratum, alluvium = alluvium,
           y = Freq, label = stratum)) +
  scale_x_discrete(expand = c(.05, .05)) +
  geom_alluvium(aes(fill = Survived)) +
  geom_stratum() + geom_text(stat = "stratum") +
  ggtitle("passengers on the maiden voyage of the Titanic",
          "stratified by demographics and survival")

## ----alluvia format of Berkeley admissions dataset----------------------------
head(as.data.frame(UCBAdmissions), n = 12)
is_alluvia_form(as.data.frame(UCBAdmissions), axes = 1:3, silent = TRUE)

## ----alluvial plot of UC Berkeley admissions dataset--------------------------
ggplot(as.data.frame(UCBAdmissions),
       aes(y = Freq, axis1 = Gender, axis2 = Dept)) +
  geom_alluvium(aes(fill = Admit), width = 1/12) +
  geom_stratum(width = 1/12, fill = "black", color = "grey") +
  geom_label(stat = "stratum", aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("Gender", "Dept"), expand = c(.05, .05)) +
  scale_fill_brewer(type = "qual", palette = "Set1") +
  ggtitle("UC Berkeley admissions and rejections, by sex and department")

## ----parallel sets plot of hair and eye color dataset-------------------------
ggplot(as.data.frame(HairEyeColor),
       aes(y = Freq,
           axis1 = Hair, axis2 = Eye, axis3 = Sex)) +
  geom_alluvium(aes(fill = Eye),
                width = 1/8, knot.pos = 0, reverse = FALSE) +
  scale_fill_manual(values = c(Brown = "#70493D", Hazel = "#E2AC76",
                               Green = "#3F752B", Blue = "#81B0E4")) +
  guides(fill = "none") +
  geom_stratum(alpha = .25, width = 1/8, reverse = FALSE) +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)),
            reverse = FALSE) +
  scale_x_continuous(breaks = 1:3, labels = c("Hair", "Eye", "Sex")) +
  coord_flip() +
  ggtitle("Eye colors of 592 subjects, by sex and hair color")

## ----lodes format of Berkeley admissions dataset------------------------------
UCB_lodes <- to_lodes_form(as.data.frame(UCBAdmissions),
                           axes = 1:3,
                           id = "Cohort")
head(UCB_lodes, n = 12)
is_lodes_form(UCB_lodes, key = x, value = stratum, id = Cohort, silent = TRUE)

## ----time series alluvia plot of refugees dataset-----------------------------
data(Refugees, package = "alluvial")
country_regions <- c(
  Afghanistan = "Middle East",
  Burundi = "Central Africa",
  `Congo DRC` = "Central Africa",
  Iraq = "Middle East",
  Myanmar = "Southeast Asia",
  Palestine = "Middle East",
  Somalia = "Horn of Africa",
  Sudan = "Central Africa",
  Syria = "Middle East",
  Vietnam = "Southeast Asia"
)
Refugees$region <- country_regions[Refugees$country]
ggplot(data = Refugees,
       aes(x = year, y = refugees, alluvium = country)) +
  geom_alluvium(aes(fill = country, colour = country),
                alpha = .75, decreasing = FALSE) +
  scale_x_continuous(breaks = seq(2003, 2013, 2)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = -30, hjust = 0)) +
  scale_fill_brewer(type = "qual", palette = "Set3") +
  scale_color_brewer(type = "qual", palette = "Set3") +
  facet_wrap(~ region, scales = "fixed") +
  ggtitle("refugee volume by country and region of origin")

## ----alluvial plot of majors dataset------------------------------------------
data(majors)
majors$curriculum <- as.factor(majors$curriculum)
ggplot(majors,
       aes(x = semester, stratum = curriculum, alluvium = student,
           fill = curriculum, label = curriculum)) +
  scale_fill_brewer(type = "qual", palette = "Set2") +
  geom_flow(stat = "alluvium", lode.guidance = "frontback",
            color = "darkgray") +
  geom_stratum() +
  theme(legend.position = "bottom") +
  ggtitle("student curricula across several semesters")

## ----alluvial plot of vaccinations dataset------------------------------------
data(vaccinations)
vaccinations <- transform(vaccinations,
                          response = factor(response, rev(levels(response))))
ggplot(vaccinations,
       aes(x = survey, stratum = response, alluvium = subject,
           y = freq,
           fill = response, label = response)) +
  scale_x_discrete(expand = c(.1, .1)) +
  geom_flow() +
  geom_stratum(alpha = .5) +
  geom_text(stat = "stratum", size = 3) +
  theme(legend.position = "none") +
  ggtitle("vaccination survey responses at three points in time")

## ----session info-------------------------------------------------------------
sessioninfo::session_info()

