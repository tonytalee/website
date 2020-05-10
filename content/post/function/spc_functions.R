#===
Df_cal_xbar <- function(df, x_var, y_var, group_var, info_var, color_var) {
    # Handling group_var
    group_var <- unique(c(x_var, group_var, info_var, color_var))
    for (col in group_var) df[, col] <- as.character(df[, col])
    
    # Count Xbar, R, s and mR
    df %>%
        group_by_at(group_var) %>%
        summarise(Xbar = mean(eval(parse(text = y_var)), na.rm = TRUE),
                  s = sd(eval(parse(text = y_var)), na.rm = TRUE), # standard deviation
                  R = max(eval(parse(text = y_var)), na.rm = TRUE) -
                      min(eval(parse(text = y_var)), na.rm = TRUE), # range
                  size = n()) %>%
        as.data.frame(.) %>%
        arrange(eval(parse(text = x_var))) %>%
        mutate(mR = abs(Xbar - lag(Xbar)))
}

Xbar_R_n <- function(df, x_var, y_var, info_var= NULL, group_var= NULL, color_var = NULL,
    info_names= NULL, control_limits= NULL, xlab= NULL, ylab_X= "Xbar", ylab_R= "R",
    title= "", R_chart= TRUE, legend= TRUE, color_set= "Set1", zoneStrip= TRUE, 
    chart_format= "plotly") {
    
    # Handling group_var
    if (! is.null(group_var)) {
        # Make sure group variables are character
        for (col in group_var) df[, col] <- as.character(df[, col])
    } else {
        # if group_var is null, then df will be group_by x_var
        group_var <- x_var
    }
    
    # Handling xlab
    if (is.null(xlab)) xlab <- x_var
    
    
    # Process input df for plotting
    # Get center lines and control limits
    if (is.null(control_limits)) {
        sample <- df %>% select(group_var) %>% unite("sample_id")
        cl <- cl_Xbar_R(df[[y_var]], sample)
        Xdbar <- cl$CLx
        Rbar <- cl$CLr
        UCLx <- cl$UCLx
        LCLx <- cl$LCLx
        UCLr <- cl$UCLr
        LCLr <- cl$LCLr
        #...
    } else {
        Xdbar <- control_limits$CLx
        Rbar <- control_limits$CLr
        UCLx <- control_limits$UCLx
        LCLx <- control_limits$LCLx
        UCLr <- control_limits$UCLr
        LCLr <- control_limits$LCLr
    }
    
    # Count Xbar and R
    df <- Df_cal_xbar(df, x_var, y_var, group_var, info_var, color_var)
    
    # Handling info_var and info_names
    if (! is.null(info_var)) {
        # info_var
        if (is.null(info_names)) {
            info_names <- names(info_var)
        } else {
            # check the length of info_var equal to info_names
            if (length(info_names) != length(info_var)) {
                stop("infor_var and info_names are not equal length.")
            }
        }
        
        # df_info
        df_info <- df[, info_var]
    } else {
        df_info <- NULL
    }
    
    # Handling color_var
    if (! is.null(color_var)) {
        df_color <- df[, color_var]
    } else {
        df_color <- NULL
    }
    
    #=== Plot Xbar chart
    if (chart_format == "plotly") {
        li <- Plotly_spc(df[[x_var]], df$Xbar, xlab, ylab_X, df_info = df_info,
                         info_names = info_names, df_color= df_color, center= Xdbar, 
                         UCL= UCLx, LCL= LCLx, oo_limits = TRUE, violate_runs = TRUE, 
                         zoneStrip= zoneStrip, legend = legend, color_set = color_set)
    } else {
        li <- Gplot_spc(df[[x_var]], df$Xbar, xlab, ylab_X, df_color= df_color, 
                        center= Xdbar, UCL= UCLx, LCL= LCLx, oo_limits = TRUE, 
                        violate_runs = TRUE, zoneStrip= zoneStrip,
                        legend = legend, color_set = color_set)
    }
    
    
    p <- li$p
    df_xbar <- li$df
    
    #=== Plot R chart
    if (R_chart) {
        if (chart_format == "plotly") {
            li <- Plotly_spc(df[[x_var]], df$R, xlab, ylab_R, df_info = df_info,
                             info_names = info_names, df_color= df_color, center= Rbar, 
                             UCL= UCLr, LCL= LCLr, oo_limits = TRUE, violate_runs = FALSE,
                             zoneStrip= FALSE, legend = F, color_set = color_set)
            p2 <- li$p
            
            # Merge plots
            p <- subplot(p, p2, nrows = 2,  heights = c(0.6, 0.4),
                         shareX = TRUE, titleY = TRUE) %>%
                plotly::layout(title = title, margin = list(l = 100),
                               yaxis= list(tickprefix= " "))
        } else {
            li <- Gplot_spc(df[[x_var]], df$R, xlab, ylab_R,  df_color= df_color, 
                            center= Rbar, UCL= UCLr, LCL= LCLr, oo_limits = TRUE, 
                            violate_runs = FALSE, zoneStrip= FALSE,
                            legend = F, color_set = color_set)
            p2 <- li$p
            
            # Merge plots
            library(cowplot)
            theme_set(theme_gray())
            p <- p + theme(axis.title.x = element_blank())
            p <- ggdraw() + draw_plot(p, x= 0, y= 0.4, width = 1, height = 0.6) +
                draw_plot(p2, x= 0, y= 0, width = 1, height = 0.4)
        }
        
        
        df_r <- li$df
        
        
    } else {
        df_r <- NULL
    }
    
    # Return
    list(p = p, df_xbar = df_xbar, df_r = df_r,
         dfi = df[, unique(c(group_var, info_var, color_var))])
}