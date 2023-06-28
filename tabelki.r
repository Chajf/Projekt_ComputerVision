library(tidyverse)

#########################################################################################################################################################################################
### Gatunki
#########################################################################################################################################################################################

rounded_gatunki <- 3

nazwy_modeli_gatunki <- c("first","second","third","fourth","fifth","sixth")

metryki_gatunki_ev <- data.frame("loss"=round(c(1.53479695320129,1.64454686641693,1.69026494026184,1.59458303451538,1.24018633365631,1.2624317407608),rounded_gatunki)
                                 ,"accuracy"=round(c(0.455780208110809,0.432709008455276,0.42545273900032,0.48319274187088,0.566174626350403,0.564810216426849),rounded_gatunki)
                                 ,"top3"=round(c(0.78336638212204,0.772699058055878,0.764698565006256,0.772109925746918,0.791863083839417,0.803491711616516),rounded_gatunki)
                                 ,"top5"=round(c(0.912614762783051,0.904614210128784,0.902939736843109,0.907017469406128,0.916707992553711,0.92246550321579),rounded_gatunki)
                                 , row.names = nazwy_modeli_gatunki
                                 )
# metryki_gatunki_ev

#########################################################################################################################################################################################
### Style
#########################################################################################################################################################################################

rounded_style <- 3

nazwy_modeli_style <- c("first","second","third")

metryki_style_ev <- data.frame("loss"=round(c(2.29541754722595,2.18937468528748,1.59250617027283),rounded_style)
                               ,"accuracy"=round(c(0.274705529212952,0.370241791009903,0.49504029750824),rounded_style)
                               ,"top3"=round(c(0.769512891769409,0.755240142345428,0.757775545120239),rounded_style)
                               ,"top5"=round(c(0.892446458339691,0.879871964454651,0.880586206912994),rounded_style)
                               , row.names = nazwy_modeli_style
                               )
# metryki_style_ev

#########################################################################################################################################################################################
### FlexTable
#########################################################################################################################################################################################

library(flextable)

# Gatunki
gatunki_ft <- flextable(metryki_gatunki_ev |> rownames_to_column("model")) |> 
  add_header_row(colwidths = 5,
                 values = "Gatunki"
                 ) |> 
  theme_vanilla() |> 
  flextable::align(i = 1, part = "header", align = "center")

# Style
style_ft <- flextable(metryki_style_ev |> rownames_to_column("model")) |> 
  add_header_row(colwidths = 5,
                 values = "Style"
                 ) |> 
  theme_vanilla() |> 
  flextable::align(i = 1, part = "header", align = "center")


gatunki_ft
style_ft

#########################################################################################################################################################################################
### Wykresy
#########################################################################################################################################################################################

###### Gatunki

### Loss function

#1 lizakowy
metryki_gatunki_ev |> rownames_to_column("model") |> mutate(model=factor(model, levels=nazwy_modeli_gatunki)) |> 
  ggplot(aes(x=model,y=loss)) +
  geom_segment(aes(xend=model, yend=0)) +
  geom_point(size=4, color="blue") +
  geom_text(aes(label=loss),vjust=-0.75) +
  theme_minimal() +
  ggtitle("Wartości funkcji straty dla Gatunków") +
  xlab("Architektura modelu") +
  ylab("Wartość funkcji straty")

#2 słupkowy
metryki_gatunki_ev |> rownames_to_column("model") |> mutate(model=factor(model, levels=nazwy_modeli_gatunki)) |> 
  ggplot(aes(x=model,y=loss)) +
  geom_bar(stat="identity", fill="blue", alpha=.6, width=.4) +
  geom_text(aes(label=loss),vjust=-0.75) +
  theme_minimal() +
  ggtitle("Wartości funkcji straty dla Gatunków") +
  xlab("Architektura modelu") +
  ylab("Wartość funkcji straty")

### ACC

#1 lizakowy
metryki_gatunki_ev |> rownames_to_column("model") |> mutate(model=factor(model, levels=nazwy_modeli_gatunki)) |> 
  ggplot(aes(x=model,y=accuracy)) +
  geom_segment(aes(xend=model, yend=0)) +
  geom_point(size=4, color="orange") +
  geom_text(aes(label=accuracy),vjust=-0.75) +
  theme_minimal() +
  scale_y_continuous(limits = c(0,1)) +
  ggtitle("Wyniki miary Accuracy dla Gatunków") +
  xlab("Architektura modelu") +
  ylab("Wartość miary Accuracy")

#2 słupkowy
metryki_gatunki_ev |> rownames_to_column("model") |> mutate(model=factor(model, levels=nazwy_modeli_gatunki)) |> 
  ggplot(aes(x=model,y=accuracy)) +
  geom_bar(stat="identity", fill="orange", alpha=.6, width=.4) +
  geom_text(aes(label=accuracy),vjust=-0.75) +
  theme_minimal() +
  scale_y_continuous(limits = c(0,1)) +
  ggtitle("Wyniki miary Accuracy dla Gatunków") +
  xlab("Architektura modelu") +
  ylab("Wartość miary Accuracy")

### top3

#1 lizakowy
metryki_gatunki_ev |> rownames_to_column("model") |> mutate(model=factor(model, levels=nazwy_modeli_gatunki)) |> 
  ggplot(aes(x=model,y=top3)) +
  geom_segment(aes(xend=model, yend=0)) +
  geom_point(size=4, color="green") +
  geom_text(aes(label=top3),vjust=-0.75) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(limits = c(0,1)) +
  ggtitle("Wyniki miary TOP 3 dla Gatunków") +
  xlab("Architektura modelu") +
  ylab("Wartość miary TOP3")

#2 słupkowy
metryki_gatunki_ev |> rownames_to_column("model") |> mutate(model=factor(model, levels=nazwy_modeli_gatunki)) |> 
  ggplot(aes(x=model,y=top3)) +
  geom_bar(stat="identity", fill="green", alpha=.6, width=.4) +
  geom_text(aes(label=top3),vjust=-0.75) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(limits = c(0,1)) +
  ggtitle("Wyniki miary TOP 3 dla Gatunków") +
  xlab("Architektura modelu") +
  ylab("Wartość miary TOP3")

### top5

#1 lizakowy
metryki_gatunki_ev |> rownames_to_column("model") |> mutate(model=factor(model, levels=nazwy_modeli_gatunki)) |> 
  ggplot(aes(x=model,y=top5)) +
  geom_segment(aes(xend=model, yend=0)) +
  geom_point(size=4, color="red") +
  geom_text(aes(label=top5),vjust=-0.75) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(limits = c(0,1)) +
  ggtitle("Wyniki miary TOP 5 dla Gatunków") +
  xlab("Architektura modelu") +
  ylab("Wartość miary TOP5")

#2 słupkowy
metryki_gatunki_ev |> rownames_to_column("model") |> mutate(model=factor(model, levels=nazwy_modeli_gatunki)) |> 
  ggplot(aes(x=model,y=top5)) +
  geom_bar(stat="identity", fill="red", alpha=.6, width=.4) +
  geom_text(aes(label=top5),vjust=-0.75) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(limits = c(0,1)) +
  ggtitle("Wyniki miary TOP 5 dla Gatunków") +
  xlab("Architektura modelu") +
  ylab("Wartość miary TOP5")