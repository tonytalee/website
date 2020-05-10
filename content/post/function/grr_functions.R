#=== Function to store GRR data
# Standard GRR ANOVA
fun.grr.std <- function(df, tolerance= NULL, colors= color_set) {
    df <- as.data.frame(df)
    names(df) <- c("Operator", "Part", "Trial", "Value")
    
    for (i in names(df)[! names(df) %in% "Value"]){
        df[[i]] <- as.factor(df[[i]])
    }
    
    k <- nlevels(df$Operator) # numbers of operators
    n <- nlevels(df$Part) # numbers of parts
    r <- nlevels(df$Trial) # numbers of trials
    
    aov <- anova(aov(Value ~ Operator * Part, data=df))
    pValue_interact <- aov["Operator:Part", "Pr(>F)"]
    if (pValue_interact < 0.05) {
        # Interaction is significant, keep the original aov
        # Data frame for storing GRR
        tb_grr <- data.frame(
            row.names = c("Total R&R", "Repeatability", "Reproducibility",
                "Operator", "Operator:Part", "Part", "Total Variation"))
        tb_grr <- data.frame(tb_grr, Variance=NA, StDev=NA, StudyVar=NA, perSV=NA,
                             perContri=NA)
        # Calculate GRR
        variance.E <- aov["Residuals", "Mean Sq"]
        variance.OxP <- (aov["Operator:Part", "Mean Sq"] - variance.E) /  r
        if (variance.OxP < 0) variance.OxP <- 0
        variance.P <- (aov["Part", "Mean Sq"] - 
                           aov["Operator:Part", "Mean Sq"]) / (k * r)
        if (variance.P < 0) variance.P <- 0
        variance.O <- (aov["Operator", "Mean Sq"] - 
                           aov["Operator:Part", "Mean Sq"]) / (n * r)
        if (variance.O < 0) variance.O <- 0
        variance.GRR <- variance.E + variance.O + variance.OxP
        variance.T <- variance.O + variance.P + variance.OxP + variance.E # total variance
        # GRR table
        tb_grr["Total R&R", "Variance"] <- variance.GRR
        tb_grr["Repeatability", "Variance"] <- variance.E
        tb_grr["Reproducibility", "Variance"] <- variance.O + variance.OxP
        tb_grr["Operator", "Variance"] <- variance.O
        tb_grr["Operator:Part", "Variance"] <- variance.OxP
        tb_grr["Part", "Variance"] <- variance.P
        tb_grr["Total Variation", "Variance"] <- variance.T
    } else {
        # Interaction is not significant, recreate aov
        aov <- anova(aov(Value ~ Operator + Part, data=df))
        # Data frame for storing GRR
        tb_grr <- data.frame(row.names = c("Total R&R", "Repeatability", 
            "Reproducibility", "Part", "Total Variation"))
        tb_grr <- data.frame(tb_grr, Variance=NA, StDev=NA, StudyVar=NA, perSV=NA,
                             perContri=NA)
        # Calculate variance
        variance.E <- aov["Residuals", "Mean Sq"]
        variance.P <- (aov["Part", "Mean Sq"] - variance.E) / (k * r)
        if (variance.P < 0) variance.P <- 0
        variance.O <- (aov["Operator", "Mean Sq"] - variance.E)/ (n * r)
        if (variance.O < 0) variance.O <- 0
        variance.GRR <- variance.E + variance.O
        variance.T <- variance.O + variance.P + variance.E # total variance
        # GRR table
        tb_grr["Total R&R", "Variance"] <- variance.GRR
        tb_grr["Repeatability", "Variance"] <- variance.E
        tb_grr["Reproducibility", "Variance"] <- variance.O
        tb_grr["Part", "Variance"] <- variance.P
        tb_grr["Total Variation", "Variance"] <- variance.T
    }
    tb_grr$StDev <- sqrt(tb_grr$Variance)
    tb_grr$StudyVar <- 6 * tb_grr$StDev
    tb_grr$perSV <- 100 * tb_grr$StDev / sqrt(variance.T)
    tb_grr$perContri <- 100 * tb_grr$Variance / variance.T
    
    # If process tolerance is provided
    if (is.null(tolerance)) {
        # Create data frame for plotting
        df_comp <- tb_grr[-nrow(tb_grr),] %>% select(perSV, perContri) %>%
            round(., 1) %>%
            mutate(Component = row.names(.)) %>% as.data.frame(.)
        df_comp$Component <- factor(df_comp$Component, levels = df_comp$Component)
        
        # max y-axis
        ymax <- max(select(df_comp, - Component)) * 1.15
        
        # Ploting component of variation
        p_comp = plot_ly(df_comp, height = 395, width = 770) %>%
            add_trace(x = ~Component, y = ~perSV, type = "bar", text = ~perSV, 
                      name= "% Study Variation", textposition = "outside",
                      marker = list(color = colors[1], width = 1.5)) %>%
            add_trace(x = ~Component, y = ~perContri, type = "bar", text = ~perContri,
                      name= "% Contribution", textposition = "outside",
                      marker = list(color = colors[2], width= 1.5)) %>%
            plotly::layout(title = "Component of variation", margin = list(t= 100),
                           xaxis = list(title = ""), 
                           yaxis = list(title = "%", range = c(0, ymax)))
    } else {
        tb_grr$perToler <- 100 * tb_grr$StudyVar / tolerance
        
        # Create data frame for plotting
        df_comp <- tb_grr[-nrow(tb_grr),] %>% select(perSV, perContri, perToler) %>%
            round(., 1) %>%
            mutate(Component = row.names(.)) %>% as.data.frame(.)
        df_comp$Component <- factor(df_comp$Component, levels = df_comp$Component)
        
        # max y-axis
        ymax <- max(select(df_comp, - Component)) * 1.15
        
        # Ploting component of variation
        p_comp = plot_ly(df_comp, height = 395, width = 770) %>%
            add_trace(x = ~Component, y = ~perSV, type = "bar", text = ~perSV, 
                      name= "% Study Variation", textposition = "outside",
                      marker = list(color = colors[1], width = 1.5)) %>%
            add_trace(x = ~Component, y = ~perContri, type = "bar", text = ~perContri,
                      name= "% Contribution", textposition = "outside",
                      marker = list(color = colors[2], width= 1.5)) %>%
            add_trace(x = ~Component, y = ~perToler, type = "bar", text = ~perToler,
                      name= "% Tolerance", textposition = "outside",
                      marker = list(color = colors[3], width= 1.5)) %>%
            plotly::layout(title = "Component of variation", margin = list(t= 100),
                           xaxis = list(title = ""), 
                           yaxis = list(title = "%", range = c(0, ymax)))
    }
    
    #--- Output by operator
    # Variance aross operator
    operator_var_test <- leveneTest(Value ~ Operator, data = df)
    operator_varTest_pValue <- operator_var_test$`Pr(>F)`[1]
    df_operator_sd <- df %>% group_by(Operator) %>%
        summarise(Mean= mean(Value, na.rm= T),
                  StDev = sd(Value, na.rm = T)) %>%
        as.data.frame(.)
    p_oper <- Plotly_box(select(df, Operator, Value), title = "Boxplot by Operators",
                         name_vector = c("operator", ""),
                         margin = list(t= 100), height = 395, width = rep(640, 2)) %>%
        add_trace(data= df_operator_sd, x= ~Operator, y= ~Mean, type= "scatter",
                  mode= "markers+lines", color= I(colors[2]),
                  marker= list(size= 15, symbol= "circle-x-open"))
    
    # Output by part
    part_var_test <- leveneTest(Value ~ Part, data = df)
    part_varTest_pValue <- part_var_test$`Pr(>F)`[1]
    df_part_sd <- df %>% group_by(Part) %>%
        summarise(Mean= mean(Value, na.rm= T),
                  StDev = sd(Value, na.rm = T)) %>%
        as.data.frame(.)
    p_part <- Plotly_box(select(df, Part, Value), title = "Boxplot by Parts",
                         name_vector = c("part", ""),
                         margin = list(t= 100), height = 395, width = rep(640, 2)) %>%
        add_trace(data= df_part_sd, x= ~Part, y= ~Mean, type= "scatter",
                  mode= "markers+lines", color= I(colors[2]),
                  marker= list(size= 15, symbol= "circle-x-open"))
    
    # Operators * Part Interaction
    df2 <- df %>% group_by(Part, Operator) %>% summarise(Mean = mean(Value)) %>%
        as.data.frame(.)
    p_inter <- Plotly_scatter(select(df2, Part, Mean), 
        info = select(df2, Operator),
        name_vector = c("Part", "Mean", "Operator"),
        xcolor = df2$Operator, title = "Operators-Parts Interaction",
        showLegend = T, margin = list(t= 100), height = 395, width = rep(770, 2))
    
    # Xbar-R
    df <- mutate(df, Op_parts = paste(Operator, Part, sep= "-"))
    li_spc <- Xbar_R(df, x_var = "Op_parts", y_var = "Value", 
                     info_var = c("Operator", "Part"), 
                     group_var = c("Operator", "Part"), 
                     color_var = "Operator", 
                     xlab = "operator-parts",
                     color_set = colors[1:k], zoneStrip = F)
    
    #=== Number of distinct data categories 
    ndc <- floor(1.4142 * tb_grr["Part", "StudyVar"] / tb_grr["Total R&R", "StudyVar"])
    
    list(tb_grr = tb_grr, ndc = ndc, pValue_interact = pValue_interact, aov = aov,
         p_comp = p_comp, p_oper = p_oper, p_part = p_part, p_inter = p_inter, 
         li_spc = li_spc, n_operator = k, n_trial = r, n_parts = n,
         operator_varTest_pValue = operator_varTest_pValue, 
         df_operator_sd = df_operator_sd,
         part_varTest_pValue = part_varTest_pValue,
         df_part_sd = df_part_sd)
}

#--- output by ggplot
Grr_plot <- function(df, tolerance= NULL, colors= color_set) {
    
    df <- as.data.frame(df)
    names(df) <- c("Operator", "Part", "Trial", "Value")
    
    for (i in names(df)[! names(df) %in% "Value"]){
        df[[i]] <- as.factor(df[[i]])
    }
    
    k <- nlevels(df$Operator) # numbers of operators
    n <- nlevels(df$Part) # numbers of parts
    r <- nlevels(df$Trial) # numbers of trials
    
    aov <- anova(aov(Value ~ Operator * Part, data=df))
    pValue_interact <- aov["Operator:Part", "Pr(>F)"]
    if (pValue_interact < 0.05) {
        # Interaction is significant, keep the original aov
        # Data frame for storing GRR
        tb_grr <- data.frame(
            row.names = c("Total R&R", "Repeatability", "Reproducibility",
                          "Operator", "Operator:Part", "Part", "Total Variation"))
        tb_grr <- data.frame(tb_grr, Variance=NA, StDev=NA, StudyVar=NA, perSV=NA,
                             perContri=NA)
        # Calculate GRR
        variance.E <- aov["Residuals", "Mean Sq"]
        variance.OxP <- (aov["Operator:Part", "Mean Sq"] - variance.E) /  r
        if (variance.OxP < 0) variance.OxP <- 0
        variance.P <- (aov["Part", "Mean Sq"] - 
                           aov["Operator:Part", "Mean Sq"]) / (k * r)
        if (variance.P < 0) variance.P <- 0
        variance.O <- (aov["Operator", "Mean Sq"] - 
                           aov["Operator:Part", "Mean Sq"]) / (n * r)
        if (variance.O < 0) variance.O <- 0
        variance.GRR <- variance.E + variance.O + variance.OxP
        variance.T <- variance.O + variance.P + variance.OxP + variance.E # total variance
        # GRR table
        tb_grr["Total R&R", "Variance"] <- variance.GRR
        tb_grr["Repeatability", "Variance"] <- variance.E
        tb_grr["Reproducibility", "Variance"] <- variance.O + variance.OxP
        tb_grr["Operator", "Variance"] <- variance.O
        tb_grr["Operator:Part", "Variance"] <- variance.OxP
        tb_grr["Part", "Variance"] <- variance.P
        tb_grr["Total Variation", "Variance"] <- variance.T
    } else {
        # Interaction is not significant, recreate aov
        aov <- anova(aov(Value ~ Operator + Part, data=df))
        # Data frame for storing GRR
        tb_grr <- data.frame(row.names = c("Total R&R", "Repeatability", 
                                           "Reproducibility", "Part", "Total Variation"))
        tb_grr <- data.frame(tb_grr, Variance=NA, StDev=NA, StudyVar=NA, perSV=NA,
                             perContri=NA)
        # Calculate variance
        variance.E <- aov["Residuals", "Mean Sq"]
        variance.P <- (aov["Part", "Mean Sq"] - variance.E) / (k * r)
        if (variance.P < 0) variance.P <- 0
        variance.O <- (aov["Operator", "Mean Sq"] - variance.E)/ (n * r)
        if (variance.O < 0) variance.O <- 0
        variance.GRR <- variance.E + variance.O
        variance.T <- variance.O + variance.P + variance.E # total variance
        # GRR table
        tb_grr["Total R&R", "Variance"] <- variance.GRR
        tb_grr["Repeatability", "Variance"] <- variance.E
        tb_grr["Reproducibility", "Variance"] <- variance.O
        tb_grr["Part", "Variance"] <- variance.P
        tb_grr["Total Variation", "Variance"] <- variance.T
    }
    tb_grr$StDev <- sqrt(tb_grr$Variance)
    tb_grr$StudyVar <- 6 * tb_grr$StDev
    tb_grr$perSV <- 100 * tb_grr$StDev / sqrt(variance.T)
    tb_grr$perContri <- 100 * tb_grr$Variance / variance.T
    
    # If process tolerance is provided
    if (is.null(tolerance)) {
        # Create data frame for plotting
        df_comp <- tb_grr[-nrow(tb_grr),] %>% select(perSV, perContri) %>%
            round(., 1) %>%
            mutate(Component = row.names(.)) %>% 
            gather("item", "value", perContri, perSV) %>%
            as.data.frame(.)
        df_comp$Component <- factor(df_comp$Component, levels = unique(df_comp$Component))
        df_comp$item <- factor(df_comp$item, levels = c("perSV", "perContri"))
        
        # Ploting component of variation
        p_comp <- ggplot(df_comp, aes(x= Component, y= value, fill= item)) + 
            geom_bar(stat = "identity", position = "dodge") +
            geom_text(aes(label = value), vjust = -0.2,
                      position = position_dodge ( .9 ), size = 4 ) +
            scale_fill_manual(values = colors, 
                              limits = c("perSV", "perContri"),
                              labels= c("% Study Variation", "% Contribution")) +
            labs(title = "Component of variation", x= NULL, y= "%") +
            theme_min(xGrid_major = F, border_color = NA)
    } else {
        tb_grr$perToler <- 100 * tb_grr$StudyVar / tolerance
        
        # Create data frame for plotting
        df_comp <- tb_grr[-nrow(tb_grr),] %>% select(perSV, perContri, perToler) %>%
            round(., 1) %>%
            mutate(Component = row.names(.)) %>% 
            gather("item", "value", perSV, perContri, perToler) %>%
            as.data.frame(.)
        df_comp$Component <- factor(df_comp$Component, levels = unique(df_comp$Component))
        df_comp$item <- factor(df_comp$item, levels = c("perSV", "perContri", "perToler"))
        
        # Ploting component of variation
        p_comp <- ggplot(df_comp, aes(x= Component, y= value, fill= item)) + 
            geom_bar(stat = "identity", position = "dodge") +
            geom_text(aes(label = value), vjust = -0.2,
                      position = position_dodge ( .9 ), size = 4 ) +
            scale_fill_manual(values = colors, 
                              limits = c("perSV", "perContri", "perToler"),
                              labels= c("% Study Variation", "% Contribution", "% Tolerance")) +
            labs(title = "Component of variation", x= NULL, y= "%") +
            theme_min(xGrid_major = F, border_color = NA)
    }
    p_comp <- p_comp +
        guides(fill= guide_legend(title = NULL))
    
    #--- Output by operator
    # Variance aross operator
    operator_var_test <- leveneTest(Value ~ Operator, data = df)
    operator_varTest_pValue <- operator_var_test$`Pr(>F)`[1]
    df_operator_sd <- df %>% group_by(Operator) %>%
        summarise(Mean= mean(Value, na.rm= T),
                  StDev = sd(Value, na.rm = T)) %>%
        as.data.frame(.)
    p_oper <- ggplot(df, aes(x= Operator, y= Value)) +
        boxplot() +
        geom_line(data= df_operator_sd, aes(x= Operator, y= Mean, group= 1), color= colors[2]) +
        geom_point(data= df_operator_sd, aes(x= Operator, y= Mean), color= colors[2], shape= 13, size= 5) +
        labs(title = "Boxplot by Operators", x= "Operator", y= NULL) +
        theme_min(xGrid_major = F, border_color = NA)
    
    # Output by part
    part_var_test <- leveneTest(Value ~ Part, data = df)
    part_varTest_pValue <- part_var_test$`Pr(>F)`[1]
    df_part_sd <- df %>% group_by(Part) %>%
        summarise(Mean= mean(Value, na.rm= T),
                  StDev = sd(Value, na.rm = T)) %>%
        as.data.frame(.)
    p_part <- ggplot(df, aes(x= Part, y= Value)) +
        boxplot() +
        geom_line(data = df_part_sd, aes(x= Part, y= Mean, group= 1), color= colors[2]) +
        geom_point(data= df_part_sd, aes(x= Part, y= Mean), color= colors[2], shape= 13, size= 5) +
        labs(title = "Boxplot by Parts", x= "Part", y= NULL) +
        theme_min(xGrid_major = F, border_color = NA)
    
    # Operators * Part Interaction
    df2 <- df %>% group_by(Part, Operator) %>% summarise(Mean = mean(Value)) %>%
        as.data.frame(.)
    p_inter <- ggplot(df2, aes(x= Part, y= Mean, group= Operator, color= Operator)) + 
        geom_line() + 
        geom_point(color= "white", size= 2.5) + 
        geom_point(size= 1.5) +
        scale_color_manual(values = colors) +
        guides(color= guide_legend(title = NULL)) +
        labs(title = "Operators-Parts Interaction", x= "Part", y= "Mean") +
        theme_min(xGrid_major = F, border_color = NA)
    
    # Xbar-R
    df <- mutate(df, Op_parts = paste(Operator, Part, sep= "-"))
    li_spc <- Xbar_R_n(df, x_var = "Op_parts", y_var = "Value", 
                     group_var = c("Operator", "Part"), 
                     color_var = "Operator", 
                     xlab = "operator-parts",
                     color_set = colors, zoneStrip = F, chart_format = "gg")
    
    #=== Number of distinct data categories 
    ndc <- floor(1.4142 * tb_grr["Part", "StudyVar"] / tb_grr["Total R&R", "StudyVar"])
    
    list(tb_grr = tb_grr, ndc = ndc, pValue_interact = pValue_interact, aov = aov,
         p_comp = p_comp, p_oper = p_oper, p_part = p_part, p_inter = p_inter, 
         li_spc = li_spc, n_operator = k, n_trial = r, n_parts = n,
         operator_varTest_pValue = operator_varTest_pValue, 
         df_operator_sd = df_operator_sd,
         part_varTest_pValue = part_varTest_pValue,
         df_part_sd = df_part_sd)
}

    
# Plotly_cook
# cooksd: cook's distance 
Plotly_cook <- function(cooksd, threshold= 5, info= NULL, 
      xcolor= NULL, text= NULL, name_vector= c("sequence", "Cook's distance"),
      margin= NULL, xaxis_style= NULL, yaxis_style= NULL, showLegend= FALSE,
      xGrid= TRUE, yGrid= TRUE, xFactor= FALSE, x_extend = NULL, y_extend = NULL,
      title= "", title_font= NULL, textposition= "top", 
      width= NULL, height= NULL, autosize= TRUE, colors= color_set) {
    
    # Not NA
    row_notNA <- which(! is.na(cooksd))
    cooksd <- cooksd[row_notNA]
    
    # 
    bound <- threshold * mean(cooksd, na.rm = T)
    df_cook <- data.frame(cooksd= cooksd)
    
    if (! is.null(info)) {
        if (! is.data.frame(info)) return("info must be a dataframe")
        if (length(cooksd) != nrow(info)) return("Lengthes of cooksd and info must be equal.")
        
        info <- as_data_frame(info)
        info <- info[row_notNA, ]
        df_cook <- cbind(df_cook, info)
    }
    
    if (! is.null(xcolor)) {
        if (length(cooksd) != length(xcolor) & length(xcolor) != 1) {
            return("Lengthes of cooksd and xcolor must be equal. Or a character represent one
                   of the columns in info")
        } 
        
        if (length(xcolor) == 1) {
            if ( ! xcolor %in% names(info)) {
                return("xcolor must be one of the columns in info")
            } else {
                xcolor <- info[[xcolor]]
            }
        }
        
        xcolor <- xcolor[row_notNA]
        }
    
    if (! is.null(text)) {
        if (length(cooksd) != length(text) & length(text) != 1) {
            return("Lengthes of cooksd and text must be equal. Or a character represent one
                   of the columns in info")
        } 
        
        if (length(text) == 1) {
            if ( ! text %in% names(info)) {
                return("text must be one of the columns in info")
            } else {
                text <- info[[text]]
            }
        }
        
        text <- text[row_notNA]
        }
    
    #---
    df_cook <- df_cook %>%
        mutate(seq= 1:length(cooksd),
               influential= ifelse(cooksd > bound, TRUE, FALSE))
    
    df <- select(df_cook, seq, cooksd)
    
    #--- Plot
    Plotly_scatter(df, xcolor = xcolor, info= info,
            name_vector = name_vector, showLegend = showLegend,
            mode= "markers", width = width, height = height, autosize = autosize) %>%
        plotly::layout(shapes = list(hline(bound), hline(0, color= "slateblue")))
}

#=== Plotly, create list of segments
Plotly_segments <- function(x0s, x1s, y0s, y1s, colors= "#4682B4", width= 30) {
    nmax <- max(length(x0s), length(y0s))
    if (length(x0s) > length(y0s)) {
        if (length(y0s) == 1) {
            y0s <- rep(y0s, length(x0s))
            y1s <- rep(y1s, length(x0s))
        } 
    } else if (length(x0s) < length(y0s)) {
        if (length(x0s) == 1) {
            x0s <- rep(x0s, length(y0s))
            x1s <- rep(x1s, length(y0s))
        } 
    }
    
    if (nmax > length(width)) width <- rep(width, nmax)
    
    
    line <- list(type = "line", xref = "x", yref= "y")
    segments <- list()
    for (i in  1:length(x0s)) {
        line[["x0"]] <- x0s[i]
        line[["x1"]] <- x1s[i]
        line[["y0"]] <- y0s[i]
        line[["y1"]] <- y1s[i]
        line[["line"]] <- list(color = colors[i], width= width[i])
        segments <- c(segments, list(line))
    }
    
    segments
}

#=== Plotly, create indicator of %GRR
Plotly_grr_indicator <- function(tb_grr, ind_val= c(0.1, 0.3), colors= color_set) {
    tb_grr <- tb_grr %>%
        mutate(Component = row.names(.)) %>% as.data.frame(.)
    
    if (nrow(tb_grr) > 5) {
        labs <- c("Repeatability", "Operator","Operator:Part")
    } else {
        labs <- c("Repeatability", "Reproducibility")
    }
    
    rnr <- tb_grr[tb_grr$Component == "Total R&R", "Variance"]
    dfx <- tb_grr %>%
        select(Component, Variance) %>%
        filter(Component %in% labs) %>%
        mutate(x = Variance / rnr,
               y= 0)
    dfx$Component <- factor(dfx$Component, levels = labs)
    dfx <- arrange(dfx, Component)
    
    
    ax_sty <- list(title = F, zeroline = F, showticklabels = F, showgrid= F)
    grr_per <- tb_grr[tb_grr$Component == "Total R&R", "perSV"]
    
    # %grr indicator
    segments <- Plotly_segments(x0s = c(0, 0.1, 0.3, 0), 
                                x1s = c(0.1, 0.3, 1, grr_per / 100), y0s = 0, y1s = 0,
                                colors = c("green", "orange", "red", "black"),
                                width = c(30, 30, 30, 10))
    p_grr <- plot_ly(x= 0.5, y= 0, width = 500, height = 100, hoverinfo= "skip", 
                     type= "bar") %>%
        layout(title = FALSE,
               barmode= "stack", showlegend = F, margin = list(t= 30),
               xaxis = list(title = F, zeroline = F, showgrid = F, tickvals = ind_val,
                            tickformat = "%"),
               yaxis = ax_sty, shapes = segments)
    
    # contribution of grr variance
    n <- nrow(dfx)
    x0s <- c(0,  cumsum(dfx$x))[1:n]
    x1s <- c(0,  cumsum(dfx$x))[2:(n+1)]
    colors <- colors[1:n]
    ano_x <- c(0, cumsum(dfx$x))[1:n] + dfx$x / 2
    ano_labs <- paste(labs, paste0(round(dfx$x * 100, 1), "%"), sep= " = ")
    segments <- Plotly_segments(x0s, x1s, 0, 0, colors)
    p_contri <- plot_ly(x= 0.5, y= 0, width = 500, height = 150, hoverinfo= "skip", 
                        type= "bar") %>%
        layout(title = F,
               barmode= "stack", showlegend = F, margin = list(t= 70),
               xaxis = ax_sty, yaxis = ax_sty, shapes = segments) %>%
        add_annotations(x= ano_x[1], text= ano_labs[1], yshift= 5, standoff= 10, ax= 0) %>%
        add_annotations(x= ano_x[2], text = ano_labs[2], yshift= 15, ax= 0, ay= -45)
    if (n == 3) {
        p_contri <- p_contri %>% 
            add_annotations(x= ano_x[3], text= ano_labs[3], yshift= 5, standoff= 10, ax= 0) 
        
    }
    
    # nuber of disinct categories
    dfa <- tb_grr %>%
        filter(Component %in% c("Total R&R", "Part"))
    dfa$Component <- factor(dfa$Component, levels = c("Total R&R", "Part"))
    p_ndc <- plot_ly(dfa, x= ~StudyVar, y= ~Component, type = "bar", 
                     hoverinfo= "skip") %>%
        layout(title = "Study variation of Total R&R vs. Part",
               xaxis = ax_sty, yaxis = list(title = F))
    
    # return
    list(p_grr = p_grr, p_contri = p_contri, p_ndc = p_ndc)
}

#=== SPC chart
Gplot_spc <- function(x, y, xlab= NULL, ylab= NULL, df_info= NULL, info_names= NULL,
                       df_color= NULL, center= NULL, UCL= NULL, LCL= NULL, oo_limits= TRUE,
                       violate_runs= TRUE, zoneStrip= FALSE, legend= FALSE, color_set= "Set1") {
    
    #  Form dataframe and remove NA data
    if (is.null(xlab)) xlab <- "x"
    if (is.null(ylab)) ylab <- "data"
    
    df <- data.frame(x= x, y= y, stringsAsFactors = FALSE)
    if (! is.null(df_info)) {
        df <- cbind(df, df_info)
        if (is.null(info_names)) info_names <- names(df_info)
        info_names <- c(xlab, ylab, info_names)
    } else {
        info_names <- c(xlab, ylab)
    }
    
    # drop out columns of info from df
    df <- select(df, x, y)
    
    # Set color map for ooc
    color_map <- c(normal= "steelblue", above= "red", below= "red", violate= "orange")
    
    # Add variable for marking normal, oo_limits and violate_runs
    df$label <- "normal"
    if (! is.null(UCL[1])) df$label[which(df$y > UCL)] <- "above"
    if (! is.null(LCL[1])) df$label[which(df$y < LCL)] <- "below"
    df$label[violating.runs(list(statistics= df$y, center= center, cl= NULL))] <-
        "violate"
    
    # Set x, y corrdinates limits
    ymax <- max(c(df$y, UCL), na.rm = TRUE)
    ymin <- min(c(df$y, LCL), na.rm = TRUE)
    ymarg <- (ymax - ymin) * 0.1
    yul <- ymax + ymarg # for coor limits
    yll <- ymin - ymarg
    
    # breaks for y-axis
    breaks <- c()
    if (! is.null(LCL[1])) breaks <- c(breaks, "L"= min(LCL, na.rm = TRUE))
    breaks <- c(breaks, "C"= center)
    if (! is.null(UCL[1])) breaks <- c(breaks, "U"= max(UCL, na.rm = TRUE))
    breaks <- round(breaks, 4)
    
    # --- ggplot
    if (! is.null(df_color)) {
        df$colorx <- as.vector(df_color)
        p <- ggplot(df, aes(x= x, y= y, color= colorx))
    } else {
        p <- ggplot(df, aes(x= x, y= y))
    }
    
    # if zone strips required
    if (zoneStrip) {
        X_3s <- UCL - center # 3-sigma zone
        yint <- round(center + c(-2, -1, 1, 2) * X_3s / 3, 4)
        breaks <- c(breaks, yint)
        p <- p +
            geom_hline(yintercept = yint, color= "grey", size= 0.5, linetype= "dashed")
    }
    
    # add control limit
    if (! is.na(UCL[1])) {
        p <- p +
            geom_hline(yintercept = UCL, color= "red", size= 0.5, linetype= "dashed")
    }
    if (! is.na(LCL[1])) {
        p <- p +
            geom_hline(yintercept = LCL, color= "red", size= 0.5, linetype= "dashed")
    }
    
    # add center line and lines-points
    p <- p +
        geom_hline(yintercept = center, color= "darkgreen", size= 0.5)
    if (! is.null(df_color)) {
        p <- p +
            geom_line(aes(group= 1)) +
            geom_point(color= "white", size= 2.5) +
            geom_point(size= 1.5)
    } else {
        p <- p +
            geom_line(aes(group= 1), color= color_set[1]) +
            geom_point(color= "white", size= 2.5) +
            geom_point(size= 1.5, color= color_set[1])
    }
    p <- p +
        scale_y_continuous(breaks = as.vector(breaks)) +
        scale_color_manual(values = color_set) + 
        labs(title = NULL, x= xlab, y= ylab) +
        guides(color= guide_legend(title = NULL)) +
        theme_min(yGrid_major = F, xAngle = 90, border_color = NA, xText= F)
    
    list(p = p, df = df)
}
