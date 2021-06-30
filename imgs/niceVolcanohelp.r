#' Cut log FC of DEG data frame
#'
#' @param data a deg data frame
#' @param x colname of logFC(character)
#' @param y colname of logFC(character)
#' @param cut_FC breaks for cutting FC(numeric vector)
#' @param cut_P breaks for cutting FDR(numeric vector)
#' @param label names for group in cut result
#' @param label_ns names for group not singed
#'
#' @return a DEG data frame with group
#' @export
#'
#' @examples
#' deg <- read.csv("https://cdn.jsdelivr.net/gh/xiayh17/Figs/imgs/deg_dat.csv")
#' # cut off
#' FC_up <- log2(2.1)
#' FC_down <- -log2(2.8)
#' pv_up <- 0.05
#' pv_down <- 0.01
#' cut_FC <- c(FC_down, FC_up) # cut off for FC
#' cut_P <- c(pv_down, pv_up) # cut off for FDR
#' label <- c("Down","Stable","Up") # labels for group
#' label_ns <- 'Stable' # no singed group
#' deg <- cut_much(deg,x = "logFC",y = "P.Value",
#' cut_FC = cut_FC,cut_P = cut_P,
#' label = label,label_ns = label_ns)
cut_much <- function(data,x,y,cut_FC,cut_P,label,label_ns) {
  cut_FC <- sort(cut_FC)
  label_cg <- setdiff(label,label_ns)
  names(cut_P) <- label_cg
  data$group <- cut(data[,x],
                    breaks = c(-Inf,cut_FC,Inf),
                    labels = label)
  index = list()
  for (i in label_cg) {
    index[[i]] <- setdiff(which(data$group == i), which(data$group == i & data[,y] < cut_P[i]))
    data$group[index[[i]]] <- label_ns
  }
  message(paste(label, as.character(table(data$group)),"\n"))
  return(data)
}

# a ggplot theme for nice volcano
theme_nice <- function(...) theme(...,
                                  axis.line = element_line(size = 0.2, linetype = "solid"), 
                                  axis.ticks = element_line(size = 0.2),
                                  panel.grid.major = element_line(colour = "gray50", size = 0.1), 
                                  panel.grid.minor = element_line(linetype = "blank"),
                                  axis.title = element_text(family = "Times"),
                                  axis.text = element_text(family = "Times"),
                                  axis.text.x = element_text(family = "Times"),
                                  axis.text.y = element_text(family = "Times"),
                                  legend.text = element_text(family = "Times"),
                                  legend.title = element_text(family = "Times"),
                                  panel.background = element_rect(fill = NA),
                                  legend.key = element_rect(fill = NA),
                                  legend.background = element_rect(fill = NA),
                                  legend.position = "top", legend.direction = "horizontal",
                                  plot.caption = element_text(family = "Times", size = 6, face = "italic", colour = "dodgerblue"))
