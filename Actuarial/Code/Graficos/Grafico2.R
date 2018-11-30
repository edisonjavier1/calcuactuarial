#------------------------------------------------------------
#!!!!!!!!!!!!!!!!!!!  GRAFICAS INDIVIDUALES 2  !!!!!!!!!!!!!!
#------------------------------------------------------------
# Graficos del IPC Deflactado, Regresion Simple

BDDgraf1 = BDDgraf 
deflactAux = "MM12(IPC General)"

BDDgraf1 = BDDgraf1[,c(1,3,4)]
names(BDDgraf1) = c("Fecha",
                    "Serie Original",
                    deflactAux)

BDDgraf1 = BDDgraf1 %>%
  # select(Fecha,`Serie Original` = SerieOrig, IPC_GeneralS) %>%
  gather(key = "Serie", value = "value", -Fecha)

seriegraf1 = ggplot(BDDgraf1, aes(x = Fecha, y = value)) + 
  geom_line(aes(color = Serie), size = 0.7) +
  scale_color_manual(values = c("#0174DF","#2E2E2E")) +
  theme_minimal()+
  labs(title = paste("IPC:", productos[k]) , y = "IPC") +

  theme(
    legend.title = element_text(size = 12, color = "black", face = "bold"),
    legend.justification = c(0, 1),
    legend.position = c(0.05, 0.95),
    legend.background = element_blank(),
    legend.key = element_blank()
  )

# seriegraf2 =  ggplot(data = BDDgraf, aes(x = Fecha, y = SerieStnd)) +
#   geom_line(size = 0.7) + theme_minimal() +
#   labs(title = paste("IPC Deflactado+Regresión:", productos[k]) , y = "IPC Deflactado (por IPC General Suavizado)") +
#   geom_vline(
#     xintercept = as.Date(paste0(periodos[-c(1, length(periodos))], "-01-01")),
#     linetype = "dashed",
#     color = "red",
#     size = 1
#   ) +
#   geom_line(
#     data = predicted,  #Anadir Lineas de Regresion !!!!!!!!!
#     aes(x = Fecha, y = IPCfit, colour = PeriodoCorte),
#     size = 0.7
#   ) +
#   theme(
#     legend.title = element_text(size = 12, color = "black", face = "bold"),
#     legend.justification = c(0, 1),
#     legend.position = c(0.75,0.5),
#     legend.background = element_blank(),
#     legend.key = element_blank()
#   )
Pval = as.numeric(summary(modelo1)$coefficients[,4])
rangos = cut(Pval,breaks = c(0,0.001,0.01,0.05,0.1,1),
             labels = c("***","**","*","."," "))

betaAux = round(as.numeric(coef(modelo1)[2]),digits = 5)

graf=ggplot(data = BDDgraf, aes(x = Fecha, y = SerieStnd)) + 
  geom_line(size = 0.7,colour = "black") +
  # ggtitle("Diagrama de dispersión") +
  geom_smooth(method = "lm",color="red") +theme_minimal()+
  # theme(plot.title = element_text(hjust = 0.5))+
  labs(title = paste("IPC Deflactado & Regresión:", productos[k]) , y = "IPC")+
  annotate("text", 
           label = TeX(paste0("$ \\beta = ",
                              betaAux,"$",
                              as.character(rangos[2]))
           ), 
           x = as.Date("01-01-2010", format = "%d-%m-%Y"), 
           y = max(BDDgraf$SerieStnd),
           size = 8
  ) 
  



#Grafico Multiple -----------------
grid.arrange(
  grobs = list(seriegraf1,graf),
  widths = c(2, 2),
  layout_matrix = rbind(c(1, 1),
                        c(2, 2))
)
