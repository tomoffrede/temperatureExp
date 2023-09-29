# Leftovers from WupperTalk

## Temperature

### Groups

```{r, include= FALSE}
load(paste0(here::here(), "/data/tempData-wuppertal.RData"))

# Positive slopes - All

d <- dat %>% 
  filter(grepl("FXO|TTY|ZNV", speaker),
         ROI %in% c("N", "F", "C")) %>% 
  mutate(sign = case_when(
    paste0(speaker, ROI) %in% paste0(eff$speaker_dyad, eff$ROI_feat) ~ "yes",
    TRUE ~ as.character(NA)
  ),
  sign = ifelse(paste0(speaker, ROI) == "ZNV-AC", NA, sign),
  speaker = case_when(
    speaker == "ZNV-A" ~ "A-1",
    speaker == "ZNV-B" ~ "A-2",
    speaker == "TTY-A" ~ "B-1",
    speaker == "TTY-B" ~ "B-2",
    speaker == "FXO-A" ~ "D-1",
    speaker == "FXO-B" ~ "D-2"
  ),
  ROI = case_when(
    ROI == "N" ~ "Nose",
    ROI == "F" ~ "Forehead",
    ROI == "C" ~ "Cheeks"
  ))

ggplot(d , aes(frame, temperature, color=speaker)) +
  geom_point()+
  geom_smooth(method="lm")+
  facet_wrap(~ROI)+
  scale_color_viridis_d()+
  labs(x = "Time",
       y = "Temperature",
       color = "Speaker")+
  theme(strip.text = element_text(size = 14))
ggsave(filename = paste0(paste(here::here(), "figures", "prez", "GeneralTemperaturePositive", sep="/"), ".png"),
       dpi = "retina")

# Positive slopes - only significant

ggplot(d %>% filter(sign == "yes"), aes(frame, temperature, color=speaker)) +
  geom_point()+
  geom_smooth(method="lm")+
  facet_wrap(~ROI)+
  scale_color_viridis_d()+
  labs(x = "Time",
       y = "Temperature",
       color = "Speaker")+
  theme(strip.text = element_text(size = 14))
ggsave(filename = paste0(paste(here::here(), "figures", "prez", "GeneralTemperaturePositive_Sign", sep="/"), ".png"),
       dpi = "retina")

# Negative slopes

d <- dat %>% 
  filter(grepl("SGB", speaker),
         ROI %in% c("N", "F", "C")) %>% 
  mutate(sign = case_when(
    paste0(speaker, ROI) %in% paste0(eff$speaker_dyad, eff$ROI_feat) ~ "yes",
    TRUE ~ as.character(NA)
  ),
  speaker = case_when(
    speaker == "SGB-A" ~ "C-1",
    speaker == "SGB-B" ~ "C-2"
  ),
  ROI = case_when(
    ROI == "N" ~ "Nose",
    ROI == "F" ~ "Forehead",
    ROI == "C" ~ "Cheeks"
  ))

ggplot(d, aes(frame, temperature, color=speaker)) +
  geom_point()+
  geom_smooth(method="lm")+
  facet_wrap(~ROI)+
  scale_color_viridis_d()+
  labs(x = "Time",
       y = "Temperature",
       color = "Speaker")
ggsave(filename = paste0(paste(here::here(), "figures", "prez", "GeneralTemperatureNegative", sep="/"), ".png"),
       dpi = "retina")

ggplot(d %>% filter(sign == "yes"), aes(frame, temperature, color=speaker)) +
  geom_point()+
  geom_smooth(method="lm")+
  facet_wrap(~ROI)+
  scale_color_viridis_d()+
  labs(x = "Time",
       y = "Temperature",
       color = "Speaker")
ggsave(filename = paste0(paste(here::here(), "figures", "prez", "GeneralTemperatureNegative_Sign", sep="/"), ".png"),
       dpi = "retina")
```

### Individuals

```{r}
d <- dat %>% 
  filter(ROI %in% c("N", "F", "C")) %>% 
  mutate(sign = case_when(
    paste0(speaker, ROI) %in% paste0(eff$speaker_dyad, eff$ROI_feat) ~ "yes",
    TRUE ~ as.character(NA)
  ),
  sign = ifelse(paste0(speaker, ROI) == "ZNV-AC", NA, sign),
  speaker = case_when(
    speaker == "ZNV-A" ~ "A-1",
    speaker == "ZNV-B" ~ "A-2",
    speaker == "TTY-A" ~ "B-1",
    speaker == "TTY-B" ~ "B-2",
    speaker == "SGB-A" ~ "C-1",
    speaker == "SGB-B" ~ "C-2",
    speaker == "FXO-A" ~ "D-1",
    speaker == "FXO-B" ~ "D-2"
  ),
  ROI = case_when(
    ROI == "N" ~ "Nose",
    ROI == "F" ~ "Forehead",
    ROI == "C" ~ "Cheeks"
  ),
  dyad = substr(speaker, 1, 1))

for(dy in unique(d$dyad)){
  ggplot(d %>% filter(dyad==dy, sign=="yes"), aes(frame, temperature, color=speaker)) +
    geom_point()+
    geom_smooth(method="lm")+
    facet_wrap(~ROI)+
    scale_color_viridis_d()+
    labs(x = "Time",
         y = "Temperature",
         color = "Speaker")+
    theme(strip.text = element_text(size = 14))
  ggsave(filename = paste0(paste(here::here(), "figures", "prez", "DyadTemperature_", sep="/"), dy, ".png"),
         dpi = "retina")
}
```

scales::show_col(viridis_pal(option="D")(8))