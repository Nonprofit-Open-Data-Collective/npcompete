
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
#Description for the dashboard
############################################################
get_desc <- function(metric){
  
  if({{metric}}=='hhi'){
    return ("HHI is a commonly used measure of market structure for research in nonprofit management and economics. 
  Specifically, it is a measure of market concentration. The index is calculated as the sum of the squared market 
  share of each firm competing in a market (Thornton & Belski, 2010). In the nonprofit sector, market share could 
  be determined by the ratio of a nonprofit’s total revenue (or expenditures) to the aggregate revenue (or expenditures) 
  for the organization's market. The calculated values range from 1/N to 1, where N is the number of organizations in a 
  market. A market with an HHI of 1 is considered as a monopoly. In contrast, a market with an HHI close to 0 would be 
  viewed as a near competition one.   In other words, the more concentrated a market is, the less competitive the market is.
  (Source: Lecy, Jesse and Hung, Chiako. Measuring the Intensity of Nonprofit Competition: Introduction of New Research Metrics)")
  }
  
  else if({{metric}} == 'nhhi'){
    return ("HHI is a commonly used measure of market structure for research in nonprofit management and economics. 
  Specifically, it is a measure of market concentration. Normalized HHi is the HHI normalized with a variable. The 
  index is calculated as the sum of the squared market share of each firm competing in a market (Thornton & Belski, 2010). In the nonprofit sector, market share could 
  be determined by the ratio of a nonprofit’s total revenue (or expenditures) to the aggregate revenue (or expenditures) 
  for the organization's market. The calculated values range from 1/N to 1, where N is the number of organizations in a 
  market. A market with an HHI of 1 is considered as a monopoly. In contrast, a market with an HHI close to 0 would be 
  viewed as a near competition one.   In other words, the more concentrated a market is, the less competitive the market is.
  (Source: Lecy, Jesse and Hung, Chiako. Measuring the Intensity of Nonprofit Competition: Introduction of New Research Metrics)")
  }
  else if(grepl( 'CR',{{metric}}, fixed = TRUE)){
    return ("Concentration Ratio is often indicated with its abbreviation CR-n where n is the concentration ratio for top n organization
            in a market. Unlike HHI requires the information of the market shares of all organizations in a particular market, 
            CRN focuses on the largest N organizations. CR has been used by the U.S. Census Bureau as a measure of the level of competition for a 
            number of industries. CRN is calculated as the summation of the market shares of the largest N nonprofit organizations. 
            In general, a low value of the index indicates a high level of competition; a high value represents an oligopoly market. 
            Although there is no consensus on the criteria required to evaluate the level of market competition, Gwin (2001) proposed that a market 
            is considered as effective competition if CR4 is less than 40; a market is considered as loose oligopoly if CR4 is between 40 and 60; 
            a market is considered as tight oligopoly if CR4 is greater than 60. CR4 has been applied to the nonprofit sector as well. Seaman et al., 
            (2014) mentioned that CR4 is a simplest measure of market competition. Castaneda et al., (2008) used CR4 as one of the competition measures 
            to examine how competition affects expenditure patterns of nonprofit organizations. 
            (Source: Lecy, Jesse and Hung, Chiako. Measuring the Intensity of Nonprofit Competition: Introduction of New Research Metrics)"  )
  }
  else if({{metric}} == 'gini'){
    return ("The Gini coefficient has been widely used in economics as a measure of income inequality. Specifically, it measures 
            income distribution among a population. In economics, there are multiple formulas used to calculate the coefficient 
            (Dorfman, 1979). However, no matter which formula is selected, a low value of the coefficient indicates a high level 
            of competition (less inequality); a high value represents a low level of competition (greater inequality). 
            In nonprofit study, Seaman et al., (2014) supplemented HHI with the Gini coefficient to capture the concept of 
            market competition and suggested that a market is considered as a reasonable level of equality if the coefficient 
            is less than 0.5; a market is considered as a modest level of inequality if the coefficient is between 0.5 and 0.75; 
            a market is considered as a high level of inequality if the coefficient is greater than .75.
            (Source: Lecy, Jesse and Hung, Chiako. Measuring the Intensity of Nonprofit Competition: Introduction of New Research Metrics)")
  }
  else if({{metric}} == 'kindex'){
    return ("On the basis of his study, Kwoka (1979) argued that CR4 is an arbitrary statistic of market competition index. 
            Instead, his suggests that the measure of the market share of largest three leading organizations in a market is 
            a better index and a market becomes competitive when the three organizations have equal-sized market share. 
            Built on Kwoka’s (1979) research findings, we generate an index aimed to measure market competition. We label 
            it the “Kwoka Index.” Basically, it is the sum of the squared market share of the largest three organizations competing 
            in a market. So, it is calculation logic is the same as that of HHI and CR4. Seaman et al., (2014) introduced the concept of the Kwoka index.
            (Source: Lecy, Jesse and Hung, Chiako. Measuring the Intensity of Nonprofit Competition: Introduction of New Research Metrics)")
    
  }
  else if({{metric}} == 'densityper100000'){
    return ("Density is the other popular measure of market competition in the literature. Market competition measured by density has 
            been used to examine its influence on nonprofit performance or survival (Abzug & Turnheim, 1998; Sorenson, 2003; Stretesky, 
            Huss, Lynch, Zahran, & Childs, 2011). There are two competing forces generated by density: legitimacy force that attracts 
            financial support for nonprofits and competition force that creates entry barrier for new organizations (Hannan & Freeman, 1987). 
            The most straightforward approach to measure density is to count the number of nonprofits in a market (Harrison & Thornton, 2014; 
            Saxton & Benson, 2005; LeRoux & Wright, 2010; Lecy & Van Slyke, 2013; Castaneda, et al., 2008). The other approach considers 
            population in the markets, dividing the number of nonprofits by population. It counts the number of nonprofits per capita in 
            a market (Harrison & Thornton, 2014; Paarlberg & Hwang, 2017). It is also called demand density (Harrison & Thornton, 2014). 
            However, simply using density as the measure of market structure cannot capture the intensity of nonprofit competition as well (Walsh, 2013). 
            (Source: Lecy, Jesse and Hung, Chiako. Measuring the Intensity of Nonprofit Competition: Introduction of New Research Metrics)")
  }
  else if({{metric}} == 'densitysmallper100000'){
    return ("Unlike CR4, Kwoka Index, and the density of super big nonprofits look into big organizations in a market to measure competition, 
            the index of small nonprofits density inquires into the percentage of nonprofits with annual revenue less than $50K in a market. 
            The index can be calculated as the number of nonprofits with annual revenue below $50K in a market divided by the number of nonprofits 
            in a market. However, a big shortcoming of calculating the number of less-than-50K-revenue nonprofits in a market to measure competition 
            is that the calculation cannot reflect real competition intensity when there are not too many nonprofits in a market. For example, 
            let’s say there is a market where only two public benefits nonprofits exist, and both are less-than-50K-revenue nonprofits. Using the 
            calculation to evaluate market competition is misleading since the ratio would be 100%, which indicates a high level of competition in 
            the market. However, the reality might be the opposite: the market is not competitive at all. Therefore, a better way to calculate the 
            index of small nonprofits density is to measure the percentage of nonprofits with annual revenue below the average in a market. 
            In other words, we look at a relative value rather than an absolute value. The relative value better reflects the density of small nonprofits 
            in a market. A high ratio indicates greater competition in a market.
            (Source: Lecy, Jesse and Hung, Chiako. Measuring the Intensity of Nonprofit Competition: Introduction of New Research Metrics)")
  }
  else if({{metric}} == 'densitybigper100000'){
    return ("Another way to measure the degree to which leading nonprofits shape market competition is to measure the percentage of big nonprofits 
            in a market. A big nonprofit is defined as an organization with annual revenue greater than 1M. 1M-revenue-nonprofit is a big enough organization 
            to be able to employ professional fundraisers to raise fund on its behalf and to hire competent staff to provide public or social services. 
            Therefore, we assume the more 1M-revenue-nonprofit a market has, the less competitive the market is. The metric can be calculated 
            as the percentage of big nonprofits in a market divided by the number of nonprofits in a market. We label it the density of super big nonprofits. 
            A high ratio indicates less competition in a market.
            (Source: Lecy, Jesse and Hung, Chiako. Measuring the Intensity of Nonprofit Competition: Introduction of New Research Metrics)")
    
  }
  else if({{metric}} == 'density_commercial'){
    
    return ("In general, nonprofits rely on diverse revenue streams such as government funding, private donations, and commercial revenues. 
            Although the requests of any revenue streams come into inevitable competition, nonprofits that are dependent on commercial revenues 
            might experience relatively higher competition since they not only compete with their nonprofit counterparts, but also for-profit enterprises 
            (Dees, 1998; Frumkin & Andre-Clark, 2000). We extend this argument to the market level and argue that the more commercial nonprofits exist in a 
            market, the more competitive the market is. Therefore, we generate an index that measures the percentage of commercial nonprofits in a market 
            as a market competition metric. A commercial nonprofit is defined as an organization that has 50% of its revenues from commercial activities. 
            The index is calculated as the number of commercial nonprofits in a market divided by the number of nonprofits in the market. We label the 
            index the density of commercial nonprofits. A high ratio indicates greater competition in a market.
            (Source: Lecy, Jesse and Hung, Chiako. Measuring the Intensity of Nonprofit Competition: Introduction of New Research Metrics)")
  }
  
  
}
