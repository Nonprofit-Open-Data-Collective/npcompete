
library( haven )        # importing data files
library( tidyr )        # data wrangling
library( dplyr )        # data wrangling
library( ggplot2 )      # fancy plots
library( ggthemes )     # fancy plots
library( scales )       # re-scaling numbers
library( stargazer )    # nice tables
library( pander )       # format tables for HTML
library( DT )           # embed datasets in HTML docs
library(plotly)



get_distribution <- function(df, msa, metric, subsector, year){

  print(paste("metric get_dist:", {{metric}}))
  
  msa.metric <- df[df$geo == {{msa}} & df$subsector == {{subsector}} & df$year =={{year}}, {{metric}}]
  
  dat.fig <-
    df %>% 
      filter(tolower(subsector) == tolower({{subsector}}), year == {{year}}) %>%
      rename(metric = {{metric}})
  
  dat.fig <- select(dat.fig, geo, subsector,n, metric, year)
  

    occ <- length(unique(dat.fig$metric))
    
    if(occ > 1){
      dens <- density(dat.fig$metric)
      
      fig.metric <- 
        plot_ly(
          dat.fig,
          x = dat.fig$metric,
          type = "histogram",
          name = {{subsector}}) %>% 
        add_lines(x =dens$x , y = dens$y, yaxis = "y2", name = "Density") %>% 
        layout(yaxis2 = list(overlaying = "y", #Adds the dual y-axis
                             side = "right", #Adds the density axis on the right side
                             rangemode = "tozero"),
               title = fig.title,
               shapes = list(vline({{msa.metric}}, msa={{msa}} ))
        ) 
      
      
      return (fig.metric)
    } else{
      
      fig.metric <- 
        plot_ly(
          dat.fig,
          x = dat.fig$metric,
          type = "histogram",
          name = {{subsector}}) %>% 
        layout(yaxis2 = list(overlaying = "y", #Adds the dual y-axis
                             side = "right", #Adds the density axis on the right side
                             rangemode = "tozero"),
               title = fig.title,
               shapes = list(vline({{msa.metric}}, msa={{msa}} ))
        ) 
      
      
      return (fig.metric)
    }
}

get_scatter <- function(df, msa, metric, second_metric,  subsector, year){
  
  
  msa.metric <- df[df$geo == {{msa}} & df$subsector == {{subsector}} & df$year =={{year}}, {{metric}}]
  msa.secondmetric <- df[df$geo == {{msa}} & df$subsector == {{subsector}} & df$year =={{year}}, {{second_metric}}]
 
  dat.fig <-
    df %>% 
    filter(tolower(subsector) == tolower({{subsector}}), year == {{year}}) %>%
    rename(metric = {{metric}})
  
  if({{metric}} == {{second_metric}}){
    dat.fig$second_metric <- dat.fig$metric
  } else{
    
    dat.fig <-
      dat.fig %>% 
     rename(second_metric = {{second_metric}})
  }
  
  
  dat.fig <- select(dat.fig, geo, subsector, metric, second_metric, year)
  
  
  fig.title <- paste0("Scatter plot of ", {{metric}}, " and " , {{second_metric}})
  
  fig.metric <- 
    plot_ly(
      dat.fig,
      x = dat.fig$metric,
      y = dat.fig$second_metric,
      type = "scatter",
      name ="Scatter") 
  
  fig.metric <- fig.metric %>% layout(yaxis = list(zeroline = FALSE, title={{second_metric}}),
                        xaxis = list(zeroline = FALSE, title={{metric}})
                        )
  return (fig.metric)
}

get_table <- function(df, msa, metric, subsector, year){
  
  msa.metric <- df[df$geo == {{msa}} & df$subsector == {{subsector}} & df$year =={{year}}, {{metric}}]

  dat.fig <-
    df %>% 
    filter(tolower(subsector) == tolower({{subsector}}), year == {{year}})
  
  
  dat.fig <- select(dat.fig, geo, subsector, metric, year)

  dat.tab <- merge( dat.fig, msa.name, by.x="geo", by.y="MSA_NECHS", all.x=T )
  
  
  return (dat.tab)
  
}


vline <- function(x = 0, color = "red", msa) {
  list(
    type = "line", 
    y0 = 0, 
    y1 = 1, 
    yref = "paper",
    x0 = x, 
    x1 = x, 
    name = {{msa}},
    line = list(color = color)
  )
}


get_gridplot_hist <-  function(df, msa, metric, year){
  
  subsectors.list <- c("Arts", "Universities","Education","Hospital","Environmental",
                       "Health","Human Services","International","Mutual Benefit","Public Benefit",
                       "Religion","Unknown")
  
  figure.list<- vector(mode = "list", length = 12)
  annotations <- vector(mode = "list", length = 12)
  i <- 1
  
  for(ss in subsectors.list){
    print(paste("metric ss:", {{ss}}))
    temp.fig <- get_subplot_hist(df, msa, metric, {{ss}}, year)
    
    figure.list[[i]] <- temp.fig
   
    figure.list[[i]] <-  figure.list[[i]] %>%
      add_annotations( x = 0.2,  
                       y = 1.0,  
                       text = {{ss}},
                       font = list(size = 24),
                       xref = "paper",  
                       yref = "paper",  
                       xanchor = "center",  
                       yanchor = "bottom",  
                       showarrow = FALSE )
    
    i <- i + 1
  }
  
  fig <- subplot(figure.list,  nrows = 4, margin=0.06) %>%
    layout(plot_bgcolor='#e5ecf6',
           xaxis = list(
             zerolinecolor = '#ffff',
             zerolinewidth = 2,
             gridcolor = 'ffff'),
           yaxis = list(
             zerolinecolor = '#ffff',
             zerolinewidth = 2,
             gridcolor = 'ffff'),
           font = list(size = 12),
           margin = list(t = 60))
 
  return (fig)
}

get_subplot_hist <- function(df, msa, metric, subsector, year){
  

  msa.metric <- df[df$geo == {{msa}} & df$subsector == {{subsector}} & df$year =={{year}}, {{metric}}]
  
  dat.fig <-
    df %>% 
    filter(tolower(subsector) == tolower({{subsector}}), year == {{year}}) %>%
    rename(metric = {{metric}})
  
  dat.fig <- select(dat.fig, geo, subsector,n, metric, year)
  
  occ <- length(unique(dat.fig$metric))
  
   fig.metric <- 
      plot_ly(
        dat.fig,
        x = dat.fig$metric,
        type = "histogram",
        name = {{subsector}}) %>% 
      layout(yaxis2 = list(overlaying = "y", #Adds the dual y-axis
                           side = "right", #Adds the density axis on the right side
                           rangemode = "tozero"),
             title = list(
               text = paste0("Histogram of ", {{metric}}, " for ", {{year}}),
               font = list(size = 16),
               x = 0.5,
               y = 1,
               xanchor =  'center',
               yanchor = 'top'),
             shapes = list(vline({{msa.metric}}, msa={{msa}} ))
      ) 
    
    
    return (fig.metric)
}

get_boxplot <- function(df, msa, subsector, year){
  
  metric.list <- c("hhi", "nhhi","CR4","kindex","gini",
                       "densityper100000","densitysmallper100000","densitybigper100000","density_commercial")
  
  figure.list<- vector(mode = "list", length = 9)
  annotations <- vector(mode = "list", length = 9)
  i <- 1
  
  for(ss in metric.list){
    print(paste("metric ss:", {{ss}}))
    temp.fig <- get_subplot_box(df, msa, {{ss}}, subsector, year)
    
    figure.list[[i]] <- temp.fig
    
    figure.list[[i]] <-  figure.list[[i]] %>%
      add_annotations( x = 0.2,  
                       y = 1.0,  
                       text = {{ss}},
                       font = list(size = 24),
                       xref = "paper",  
                       yref = "paper",  
                       xanchor = "center",  
                       yanchor = "bottom",  
                       showarrow = FALSE )
    
    i <- i + 1
  }
  
  fig <- subplot(figure.list,  nrows = 3, margin=0.06) %>%
    layout(plot_bgcolor='#e5ecf6',
           xaxis = list(
             zerolinecolor = '#ffff',
             zerolinewidth = 2,
             gridcolor = 'ffff'),
           yaxis = list(
             yaxis = list(showticklabels = F),
             zerolinecolor = '#ffff',
             zerolinewidth = 2,
             gridcolor = 'ffff'),
           showlegend = F)
  
  return (fig)
  
}

get_subplot_box <- function(df, msa, metric, subsector, year){
  
  msa.metric <- df[df$geo == {{msa}} & df$subsector == {{subsector}} & df$year =={{year}}, {{metric}}]
  
  dat.fig <-
    df %>% 
    filter(tolower(subsector) == tolower({{subsector}}), year == {{year}}) %>%
    rename(metric = {{metric}})
  
  dat.fig <- select(dat.fig, geo, subsector,n, metric, year)
  

  fig.metric <- 
    plot_ly(x = dat.fig$metric, type = "box", hoverinfo = 'x') %>%
      layout(showlegend = F,
             yaxis = list(
               showticklabels = F)
      )
  
  
  return (fig.metric)
  
}
################msa to msa name##############

get_msanech <- function(df){
 
  msa.name <- df[is.na(df$FIPS),]
  msa.name <- select(msa.name, MSA_NECHS, NAME)
  
  return (msa.name)
}

get_msacode <- function(df, msa.name ){
  
  msa.code <- df$MSA_NECHS[df$NAME == {{msa.name}}]
  
  return (msa.code)
}

get_msaname <- function(df = msa.name, msa.code){
  
  msa.code <- df$NAME[df$MSA_NECHS == {{msa.code}}]
  return (msa.code)
}
#################################################

check_if_exists <- function(df, msa, metric, subsector,year){
  
  msa.metric <- df$hhi[df$geo == {{msa}} & df$subsector == {{subsector}} & df$year =={{year}}]
  
  
  if(length(msa.metric)==0){
    return (FALSE)
  }
  
  return (TRUE)
}

############################################################

############################################################
