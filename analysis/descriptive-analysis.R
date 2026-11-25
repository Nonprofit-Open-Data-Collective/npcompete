jplot <- function( x1, x2, draw.line=T, ... )
{

	plot( x1, x2,
	      pch=19, 
	      col=gray(0.6, alpha = 0.4), 
	      cex=0.8,  
	      bty = "n",
	      cex.lab=1.5,
        ... )

	if( draw.line==T ){ 
		ok <- is.finite(x1) & is.finite(x2)
		lines( lowess(x2[ok]~x1[ok]), col="steelblue", lwd=3 ) }

}




panel.cor <- function(x, y, digits=2, prefix="", cex.cor)
{
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(0, 1, 0, 1))
    r <- cor(x, y, use="pairwise.complete.obs")
    txt <- format(c(r, 0.123456789), digits=digits)[1]
    txt <- paste(prefix, txt, sep="")
    if(missing(cex.cor)) cex <- 0.8/strwidth(txt)
    
    test <- cor.test(x,y)
    # borrowed from printCoefmat
    Signif <- symnum(test$p.value, corr = FALSE, na = FALSE,
                  cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                  symbols = c("***", "**", "*", ".", " "))
    
    text(0.5, 0.5, txt, cex = 2 )
    text(.7, .8, Signif, cex=3, col=2)
}


panel.smooth <- function (x, y, col = par("col"), bg = NA, pch = par("pch"), 
  cex = 1, col.smooth = "red", span = 2/3, iter = 3, ...) 
{
  points(x, y, pch = 19, col = gray(0.5,0.5), 
         bg = bg, cex = 0.7)
  ok <- is.finite(x) & is.finite(y)
  if (any(ok)) 
    lines(stats::lowess(x[ok], y[ok], f = span, iter = iter), 
      col = col.smooth, lwd=2, ...)
}


pairs( df_2021, lower.panel=panel.smooth, upper.panel=panel.cor )



# library( corrplot )
corrplot( cor(df_2021, use="complete.obs"), 
          order = "hclust", tl.col='black', tl.cex=.75 ) 


jplot2 <- function( x1, x2, draw.line=T, ... ){
  plot( x1, x2,
        pch=19, 
        col=gray(0.6, alpha = 0.4), 
        cex=1.3,  
        bty = "n",
        cex.lab=1.5,
          ... )
  if( draw.line==T ){ 
  ok <- is.finite(x1) & is.finite(x2)
  lines( lowess(x2[ok]~x1[ok]), col="steelblue", lwd=3 ) }
}



df_2021 <- df_wide %>%
  dplyr::select(ends_with("_2021"))

# Select columns ending with _2021
df_2021x <- df_wide %>%
  dplyr::select(ends_with("_2021"))

jplot( df_2021x$HMS_2021, df_2021$HMS_2021, xlab="Residuals from Total Expense Models", ylab="Residuals from NPO Count Models" )
abline( h=c(-1,0,1), col="darkorange", lty=3 )
abline( v=c(-1,0,1), col="darkorange", lty=3 )
pairs( df_2021x, lower.panel=panel.smooth, upper.panel=panel.cor )




log_npo <- sf$LOG_NPO[ sf$YEAR == 2022 & sf$SUB == "ALL" ]
log_pop <- sf$LOG_POP[ sf$YEAR == 2022 & sf$SUB == "ALL" ]
log_rev <- sf$LOG_TOTREV[ sf$YEAR == 2022 & sf$SUB == "ALL" ]
pctl_d <- sf$DENSITY_PCTL_N[ sf$YEAR == 2022 & sf$SUB == "ALL" ]
pctl_r <- sf$DENSITY_PCTL_R[ sf$YEAR == 2022 & sf$SUB == "ALL" ]

summary( lm( log_npo ~ log_pop ) )
summary( lm( log_rev ~ log_pop ) )

par( mfrow=c(1,2) )
jplot2( log_pop, log_npo, ylim=c(-1,13), xlab="Population (logged)", yaxt="n", main="Number of Nonprofits (logged)", ylab="" )
abline( a=-6.5189, b=1, lty=3, col="darkorange", lwd=3 ) 
text( 12, 1, "B1 = 0.98", col="steelblue", pos=4, cex=2 )
jplot2( log_pop, log_rev, ylim=c(12,26), xlab="Population (logged)", yaxt="n", main="Size of Nonprofits (logged revenues)", ylab="" )
abline( a=(3.77923+3), b=1, lty=3, col="darkorange", lwd=3 ) 
text( 12, 14, "B1 = 1.36", col="steelblue", pos=4, cex=2 )

plot( sort(pctl_d,decr=T), type="b", pch=19, cex=0.5, col="steelblue", bty="n", 
      xlab="rank", ylab="Residuals", xaxt="n", main="Nonprofit Density (Count)", cex.lab=1.5  )
abline( h=0, lwd=1 )
segments( x0=1:length(pctl_d), y0=sort(pctl_d,decr=T), y1=0, col=gray(0.6,0.4), lwd=0.2 )

plot( sort(pctl_r,decr=T), type="b", pch=19, cex=0.5, col="steelblue", bty="n", 
      xlab="rank", ylab="Residuals", xaxt="n", main="Nonprofit Density (Size)", cex.lab=1.5  )
abline( h=0, lwd=1 )
segments( x0=1:length(pctl_r), y0=sort(pctl_r,decr=T), y1=0, col=gray(0.6,0.4), lwd=0.2 )

# Compute correlation matrix
cor(df_2021, use = "pairwise.complete.obs")


jplot( df_2021$ART_2021, df_2021$EDU_2021 )

npo.art <- dd$NPO_N[ dd$YEAR == 2011 & dd$SUB == "ART" ]
pop.art <- dd$POP2[ dd$YEAR == 2011 & dd$SUB == "ART" ]
res.art <- dd$RES2[ dd$YEAR == 2011 & dd$SUB == "ART" ]
geo.art <- dd$GEO[ dd$YEAR == 2011 & dd$SUB == "ART" ]


jplot( log( pop.art + 1 ), log( npo.art + 1 )  )
jplot( log( pop.art + 1 ), log( res.art + 1 )  )
jplot( log( pop.art + 1 ), df_2021$ART_2021 )

mm <- lm( log(npo.art+1) ~ log(pop.art) )
summary( mm )
abline( mm, col="firebrick" )

mm <- lm( log(res.art+1) ~ log(pop.art) )
summary( mm )
abline( mm, col="firebrick" )


npo.all <- dd$NPO_N[ dd$YEAR == 2021 & dd$SUB == "ALL" ]
pop.all <- dd$POP2[ dd$YEAR == 2021 & dd$SUB == "ALL" ]
res.all <- dd$RES2[ dd$YEAR == 2021 & dd$SUB == "ALL" ]

jplot( log( pop.all + 1 ), log( npo.all + 1 ), 
       xlab="Population (logged)", ylab="Number of NPOs (logged)", main="Log-Log Model of NP Density"  )

jplot( log( pop.all + 1 ), res.all/pop.all, ylim=c(0,20000), xlab="City Size (log pop)", ylab="Per Capita NPO Spending" )

summary( res.all/pop.all )



jplot( log( pop.all + 1 ), log( res.all + 1 ), 
       xlab="Population (logged)", ylab="Total NPO Expenses (logged)", main="Log-Log Model of NP Scale"  )


mm <- lm( log(npo.all+1) ~ log(pop.all) )
summary( mm )
abline( mm, col="firebrick" )


pcap <- dd$RES2 / dd$POP2
jplot( dd$LOG_POP, pcap ) , 
       xlab="Population (logged)", ylab="Number of NPOs (logged)", main="Log-Log Model of NP Density"  )



dft1 <- data.frame( GEO=df_wide$GEO, ART_2021=df_wide$ART_2021 )
dft2 <- data.frame( GEO=geo.art, POP=pop.art, NPO=npo.art )
dft <- merge( dft1, dft2, by="GEO" )

jplot( log( dft$POP + 1 ), dft$ART_2021 )


jplot( df_wide$HMS_2021, df_wide$HMS_2022 )


df_all <- df_wide %>%
  select(starts_with("ALL_"))
pairs( df_all, lower.panel=panel.smooth, upper.panel=panel.cor )


