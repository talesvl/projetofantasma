dados_mulheres<-filter(dados,Gender=="F")
mulheres_medalha<-filter(dados_mulheres, Medal %in% c("Bronze","Silver","Gold"))


frequencia_feminina <- mulheres_medalha %>%
  filter(!is.na(Team)) %>%
  count(Team) %>%
  mutate(
    freq = n,
    label = str_c(n) %>% str_squish()
  )


top5_feminino_medalhas<-arrange(frequencia_feminina,desc(n))
top5_feminino_medalhas<-head(top5_feminino_medalhas,5)
top5_feminino_medalhas$Team<-c("Estados Unidos","Rússia","Alemanha","China","Austrália")

top5_feminino_medalhas_tabela<-select(top5_feminino_medalhas,Team,n)
colnames(top5_feminino_medalhas_tabela)<-c("País","Frequência")



ggplot(top5_feminino_medalhas) +
  aes(x = fct_reorder(Team, n, .desc=T), y = n, label = label) +
  geom_bar(stat = "identity", fill = "#A11D21", width = 0.7) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.5, #hjust = .5,
    size = 3
  ) +
  labs(x = "País", y = "Frequência") +
  theme_estat()

top5_feminino_medalhas_tabela
ggsave("colunas-uni-freq.pdf", width = 158, height = 93, units = "mm"
)

dados$`Weight (lbs)`<-dados$`Weight (lbs)`*0.45359237
dados$`Height (cm)`<-dados$`Height (cm)`/100
dados<-mutate(dados, IMC=dados$`Weight (lbs)`/(dados$`Height (cm)`**2))
dados_ginástica_futebol_judô_atletismo_badminton<-filter(dados, Sport %in% c("Gymnastics","Football","Judo","Athletics","Badminton"))


ggplot(dados_ginástica_futebol_judô_atletismo_badminton) +
  aes(x = reorder(Sport, IMC), y = IMC) +
  geom_boxplot(fill = c("#A11D21"), width = 0.5) +
  stat_summary(
    fun = "mean", geom = "point", shape = 23, size = 3, fill = "white"
  ) +
  labs(x = "Esporte", y = "IMC") +
  ylim(15,30)+
  theme_estat()
ggsave("box_bi2.pdf", width = 158, height = 93, units = "mm")


estat_colors <- c(
  "#A11D21", "#003366", "#CC9900",
  "#663333", "#FF6600", "#CC9966",
  "#999966", "#006606", "#008091",
  "#041835", "#666666" )


theme_estat <- function(...) {
  theme <- ggplot2::theme_bw() +
    ggplot2::theme(
      axis.title.y = ggplot2::element_text(colour = "black", size = 12),
      axis.title.x = ggplot2::element_text(colour = "black", size = 12),
      axis.text = ggplot2::element_text(colour = "black", size = 9.5)
      ,
      panel.border = ggplot2::element_blank(),
      axis.line = ggplot2::element_line(colour = "black"),
      legend.position = "top",
      ...
    )
  return(
    list(
      theme,
      scale_fill_manual(values = estat_colors),
      scale_colour_manual(values = estat_colors)
    )
  )
}


print_quadro_resumo <- function(data, var_name, title="Medidas resumo
da(o) [nome da variável]", label="quad:quadro_resumo1")
{
  var_name <- substitute(var_name)
  data <- data %>%
    summarize(`Média` = round(mean(!!sym(var_name)),2),
              `Desvio Padrão` = round(sd(!!sym(var_name)),2),
              `Variância` = round(var(!!sym(var_name)),2),
              `Mínimo` = round(min(!!sym(var_name)),2),
              `1º Quartil` = round(quantile(!!sym(var_name), probs =
                                              .25),2),
              `Mediana` = round(quantile(!!sym(var_name), probs = .5)
                                ,2),
              `3º Quartil` = round(quantile(!!sym(var_name), probs =
                                              .75),2),
              `Máximo` = round(max(!!sym(var_name)),2)) %>%
    t() %>%
    as.data.frame() %>%
    rownames_to_column()
  latex <- str_c("\\begin{quadro}[H]
\t\\caption{", title, "}
\t\\centering
\t\\begin{adjustbox}{max width=\\textwidth}
\t\\begin{tabular}{", sep="")
  col_count <- ncol(data)
  row_count <- nrow(data)
  latex <- str_c(latex, "| l |\n", sep=" ")
  for (i in seq(2, col_count))
  {
    numCount <- data[i, -c(1)] %>%
      as.numeric() %>%
      {floor(log10(.)) + 1} %>%
      max()
    latex <- str_c(latex, "\t\t\tS[table-format = ", numCount ,".2]\n
      ", sep="")
  }
  latex <- str_c(latex, "\t\t\t|}\n\t\\toprule\n\t\t", sep="")
  if (col_count > 2)
  {
    for (i in seq(1,col_count))
    {
      if (i == 1)
        latex <- str_c(latex, "\\textbf{Estatística}", sep="")
      else
        latex <- str_c(latex, " \\textbf{", data[1, i], "}", sep="")
      
      if (i < col_count)
        latex <- str_c(latex, "&", sep=" ")
      else
        latex <- str_c(latex, "\\\\\n", sep=" ")
    }
  }
  else
  {
    latex <- str_c(latex, "\\textbf{Estatística} & \\textbf{Valor}
      \\\\\n", sep="")
  }
  
  latex <- str_c(latex, "\t\t\\midrule\n", sep="")
  
  if (col_count > 2)
    starting_number <- 2
  else
    starting_number <- 1
  for (i in seq(starting_number, row_count))
  {
    latex <- str_c(latex, "\t\t", str_flatten(t(data[i,]), collapse =
                                                " & "), " \\\\\n")
  }
  latex <- str_c(latex, "\t\\bottomrule
\t\\end{tabular}
\t\\label{", label, "}
\t\\end{adjustbox}
\\end{quadro}", sep="")
  
  writeLines(latex)
}
library(ggplot2)
library(readxl)
library(latexpdf)
library(tidyverse)
library(dplyr)
dados<-read_excel("Olimpiadas 2000 - 2016.xlsx")
install.packages("rvcheck")
library("rvcheck")
check_r()
