hist_df = hist



calc_state_props <- function(hist_df){

  # calc probs table for nodes without parents
  base <- c("Pn", "VTB", "Sm") #unconditional probs
  base_probs <- list()
  for (el in base){
    base_probs <- c(base_probs, table(hist_df[[el]]))
  }
  base_probs <- lapply(base_probs, function(x){x/nrow(hist_df)})
  base_probs_df <- data.frame(
    name <- base,
    zero_prop <- unlist(base_probs[seq(1,length(base_probs),2)]),
    one_prop <- unlist(base_probs[seq(2,length(base_probs),2)])
  )
  colnames(base_probs_df)[2] <- "zero_prop"
  colnames(base_probs_df)[3] <- "one_prop"
  #print(base_probs_df)
  
  #calc probs table for nodes with parents
  TB = c("TB","VTB") #list structure: node, parent1, parent2....
  LC = c("LC","Sm")
  Br = c("Br","Sm")
  Te = c("Te","Pn")
  XR = c("XR","Pn","TB","LC")
  Dy = c("Dy","LC","Br")
  all_fitting_points <- list(TB, LC, Br, Te, XR, Dy)
  
  tb_probs_df = data.frame(
    TB = c(0,1),
    vtb_zero = c(nrow(hist_df[hist_df$TB==0 & hist_df$VTB==0,]), nrow(hist_df[hist_df$TB==0 & hist_df$VTB==1,])),
    vtb_one =  c(nrow(hist_df[hist_df$TB==1 & hist_df$VTB==0,]), nrow(hist_df[hist_df$TB==1 & hist_df$VTB==1,]))
  )
  tb_probs_df[1:2,2:3] =  apply(tb_probs_df[1:2,2:3], 2, function(i)i/sum(i))

  print(tb_probs_df)
  
  browser()
}

calc_state_props(hist_df)

