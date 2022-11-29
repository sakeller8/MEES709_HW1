#Shayna Keller

#this function plots...

myplot <- function(object, main_title = "Your Title Here", 
                   x_title = "Your X-Axis Title Here", 
                   y_title = "Your Y-Title Here",
                   Mean = "green", Median = "blue", `Q1 and Q3` = "orange",
                   plotmean = TRUE) {
    check_class(object)
    object_clean <- na.omit(object)
    object_plot <- main_plotter(object_clean, main_title = main_title,
                                x_title = x_title,
                                y_title = y_title, mean_color_in = Mean,
                                median_color_in = Median, quantile_color_in = `Q1 and Q3`,
                                plotmean = plotmean)
    return(object_plot)
    
    
}

main_plotter <- function(series, main_title = "Your Title Here", x_title = "Your X-Axis Title Here", 
                         y_title = "Your Y-Axis Title Here", mean_color_in = "green", 
                         median_color_in = "blue", quantile_color_in = "orange",
                         plotmean = TRUE){
    # check_class <- function(thing){
    #     if(
    #         !any(
    #             is.data.frame(thing),
    #             is.matrix(thing),
    #             is.ts(thing))
    #     ){stop("Object must be a data frame, matrix or ts")}
    # }
    my_omit <- function(thing) {
        na.omit(thing)
    }
    #create new column for date so data frame knows its a date
    #series_2<- series %>%
    #   mutate(Date = (paste0(series[ ,1])))
    type = ""
    if (is.ts(series)){
        type = "TimeSeries"
        x <- time(series)
        series <- as.data.frame(series)
        series_2 <- series %>%
            mutate(Date = x)
        
    }else if (is.data.frame(series)){
        type = "DataFrame"
        series_2 <- series %>%
            mutate(Date = (rownames(series)))
    } else {
        type = "Unknown"
        series_2<- series %>%
            mutate(Date = (rownames(as.data.frame(series))))
    }
    
    #create data frame with just data
    series_3 <- data.frame(series[c(2:ncol(series))])
    
    #apply loop for quantiles and attach outputs to a data frame
    df_quantiles = data.frame()
    for(i in seq_len(nrow(series_3))) {
        output = quantile(series_3[i, ])
        df_quantiles = rbind(df_quantiles, output)
    }
    
    #find means of each row
    series_mean <- rowMeans(series[ ,c(2:ncol(series))])
    means <- as.data.frame(series_mean)
    
    #find medians of each row
    series_median <- apply(series,1,median)
    medians <- as.data.frame(series_median)
    
    #combine everything into a data frame
    df1 <- as.data.frame(c(series_2, means, medians, df_quantiles))
    
    #make columns for date, mean, median, and quantiles for plotting
    
    # df2 <- data.frame(df1$Date, df1$series_mean, df1$series_median, df_quantiles)
    # print(nrow(df1))
    # print(nrow(df2))
    #plot
    
    if (plotmean == TRUE) {
        plot1 <- ggplot() +
            geom_line(data=df1, aes(x=Date, y=series_mean, group=1, color = "Mean"),
                      size = 1, shape = 21) +
            geom_line(data=df1, aes(x=Date, y=series_median, group=1, color = "Median"), 
                      size = 1, shape = 21) +
            geom_point(data=df1, aes(x=Date, y=series_mean, group=1, color = "Mean"),
                       size = 2, shape = 21)
    } else {
        plot1 <- ggplot() +
            geom_line(data=df1, aes(x=Date, y=series_median, group=1, color = "Median"),
                      size = 1, shape = 21)
    }
    
    #plot1
    
    if (type == "TimeSeries"){
        plot2 <- plot1 + geom_ribbon(data = df1, aes(x = Date, ymin = X25.,
                                                     ymax = X75., color = "Q1 and Q3"),
                                     alpha = 0.3)
    }else if (type == "DataFrame"){
        plot2 <- plot1 + geom_ribbon(data = df1, aes(x = 1:nrow(df1), ymin = X25., 
                                                     ymax = X75., color = "Q1 and Q3"), 
                                     alpha = 0.3)
    }else{
        plot2 <- plot1 + geom_ribbon(data = df1, aes(x = 1:nrow(df1), ymin = X25., 
                                                     ymax = X75., color = "Q1 and Q3"), 
                                     alpha = 0.3)
    }
    
    #plot2
    
    if (plotmean == TRUE) {
        plot3 <- plot2 + ggtitle(main_title) +
            labs(x = x_title, y = y_title) +
            scale_color_manual(values = c(Mean = mean_color_in, Median = median_color_in, 
                                          `Q1 and Q3` = quantile_color_in))
    } else {
        plot3 <- plot2 + ggtitle(main_title) +
            labs(x = x_title, y = y_title) +
            scale_color_manual(values = c(Median = median_color_in, 
                                          `Q1 and Q3` = quantile_color_in))
    }
    
    return(plot3)
    
}

# check_class tests whether a thing is a usable data type
check_class <- function(thing) {
    if(
        !any(
            is.data.frame(thing),
            is.matrix(thing),
            is.ts(thing))) {
        stop("Object must be a data frame, matrix or ts")
    }
    if(is.null(ncol(thing))) {
        stop("object has no columns")
    }
    if (ncol(thing) <= 1) {
        stop("object must have more than one column")
    }
    
}

#the last line of the file