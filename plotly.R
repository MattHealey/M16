library(bootStepAIC)
library(plotly)
library(mlbench)
# Fit Linear regression model
fit <- lm(crim ~ ., data = BostonHousing)

# Run bootstrapped stepwise regression
fit.boot <- boot.stepAIC(fit, data = BostonHousing, B = 100) # That's it !

# Extract data
nBoot <- summary(fit.boot)[8,1]
origModel <- paste(names(coef(fit.boot$OrigModel)), collapse = " + ")
stepModel <- paste(names(coef(fit.boot$OrigStepAIC)), collapse = " + ")

# Names of covariates
covariates <- rownames(fit.boot$Covariates)
nCovariates <- length(covariates)

# Matrix of number of times each covariate was picked
coef.pick <- fit.boot$Covariates

# Matrix for the consistency of sign on each covariate
coef.sign <- fit.boot$Sign

# Change name for "chas" since it is a factor
rownames(coef.sign)[which(rownames(coef.sign) == "chas1")] <- "chas"
coef.sign <- coef.sign[match(rownames(coef.pick), rownames(coef.sign)),]

# Matrix for statistical significance
coef.stat <- fit.boot$Significance

# Change name for "chas" since it is a factor
rownames(coef.stat)[which(rownames(coef.stat) == "chas1")] <- "chas"
coef.stat <- coef.stat[match(rownames(coef.pick), rownames(coef.stat)),]

# Make into long form for charting later
coef.stat.long <- data.frame()

for(i in 1:length(coef.stat)){
  n <- round(coef.stat[i],0)
  vec <- seq(0, n, by = 2)
  mat <- data.frame(rep(names(coef.stat)[i], length(vec)), vec, paste("% Sig", n))
  names(mat) <- c("variable", "sig", "text")
  
  # We'll use mode = "line". NA helps separate line segments
  coef.stat.long <- rbind(coef.stat.long, mat, c(NA, NA))
}

# Convert to dataframes
coef.pick <- as.data.frame(coef.pick)
coef.stat <- as.data.frame(coef.stat)
coef.sign <- as.data.frame(coef.sign)

names(coef.pick) <- "pick"
names(coef.sign) <- c("pos", "neg")
names(coef.stat) <- "stat"

# Base plot for number of times a variable was picked by stepAIC
# plot
scale <- 3

# Create hover text
pick.text <- paste("% Picked: ", round(coef.pick[,1],2))
sign.text.up <- paste("% Pos Sign: ", round(coef.sign$pos,2))
sign.text.down <- paste("% Neg Sign: ", round(coef.sign$neg,2))

# Base plot for number of times a variable was picked by stepAIC
p <- plot_ly(coef.pick, x = rownames(coef.pick), y = pick,
             type = "bar", opacity = 0.75, name = "Times picked (%)",
             hoverinfo = "text", text = pick.text,
             marker = list(color = "#00994d", line = list(width = 2))) %>% 
  
  # Layer for number of times a variable was statistically significant at 5%
  add_trace(data = coef.stat.long, x = variable, y = sig, 
            type = "scatter", mode = "markers + line", name = "Stat. Sig (%)",
            line = list(color = "#ffdb4d", width = 15),
            hoverinfo = "text", text = text) %>% 
  
  # Layer for number of times a variable's coefficient was positive
  add_trace(data = coef.sign, x = rownames(coef.pick), y = rep(-5, nCovariates), 
            type = "scatter", mode = "markers", name = "Coef Sign(% pos)",
            marker = list(symbol = "triangle-up", size = pos/scale, color = "#4da6ff",
                          line = list(color = "black", width = 2)),
            hoverinfo = "text", text = sign.text.up) %>% 
  
  # Layer for number of times a variable's coefficient was negative
  add_trace(data = coef.sign, x = rownames(coef.pick), y = rep(-10, nCovariates), 
            type = "scatter", mode = "markers", name = "Coef Sign(% neg)",
            marker = list(symbol = "triangle-down", size = neg/scale, color = "#ff704d",
                          line = list(color = "black", width = 2)),
            hoverinfo = "text", text = sign.text.down) %>% 
  
  # Layout, annotations, axis options etc
  layout(xaxis = list(title = "<b>Covariates</b>"),
         yaxis = list(title = "<b>Percentage(%)</b>",
                      tickmode = "array", 
                      tickvals = round(seq(0, 100, length.out = 10), 0),
                      domain = c(0.2, 0.9)),
         plot_bgcolor = "#e1efc3",
         paper_bgcolor = "#e1efc3",
         
         annotations = list(
           list(x = 0.2, y = 1, 
                xref = "paper", yref = "paper", 
                xanchor = "left", yanchor = "top",
                ax = 0, ay = 0,
                text = "Visualizing <em>boot.stepAIC()</em>",
                font = list(family = "serif", size = 30)),
           
           list(x = 0.3, y = 0, 
                xref = "paper", yref = "paper", 
                xanchor = "left", yanchor = "top",
                ax = 0, ay = 0,
                align = "left",
                text = paste("<em>Original Model:</em>", origModel, "<br>",
                             "<em>Stepwise Model:</em>", stepModel),
                font = list(family = "PT Sans Narrow", size = 15)),        
           
           list(x = 0.8, y = 0.90, 
                xref = "paper", yref = "paper", 
                xanchor = "left", yanchor = "top", align = "left",
                ax = 0, ay = 0,
                text = paste0("<em>No. of Covariates:</em>", nCovariates, "<br>",
                              "<em>No. of bootstrap samples:</em>", nBoot, "<br>"),
                font = list(family = "PT Sans Narrow", size = 15))
         ))

