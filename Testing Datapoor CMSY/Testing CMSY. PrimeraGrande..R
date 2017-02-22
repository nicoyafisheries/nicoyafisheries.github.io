#CMSY() with INCOPESCA Data for primera grande which included corvina reina.

Citation

#To cite the package, cite the authors of the original assessment method (shown below), Rosenberg et al. (2014), and:

#citation("datalimited").

#> To cite package 'datalimited' in publications use:
#> 
#>   Sean C. Anderson, Jamie Afflerbach, Andrew B. Cooper, Mark
#>   Dickey-Collas, Olaf P. Jensen, Kristin M. Kleisner, Catherine
#>   Longo, Giacomo Chato Osio, Daniel Ovando, Carolina Minte-Vera,
#>   Coilin Minto, Iago Mosqueira, Andrew A. Rosenberg, Elizabeth R.
#>   Selig, James T. Thorson and Jessica C. Walsh (2016).
#>   datalimited: Stock Assessment Methods for Data-limited
#>   Fisheries. R package version 0.1.0.
#>   https://github.com/datalimited/datalimited
#> 
#> A BibTeX entry for LaTeX users is
#> 
#>   @Manual{,
#>     title = {datalimited: Stock Assessment Methods for Data-limited Fisheries},
#>     author = {Sean C. Anderson and Jamie Afflerbach and Andrew B. Cooper and Mark Dickey-Collas and Olaf P. Jensen and Kristin M. Kleisner and Catherine Longo and Giacomo Chato Osio and Daniel Ovando and Carolina Minte-Vera and Coilin Minto and Iago Mosqueira and Andrew A. Rosenberg and Elizabeth R. Selig and James T. Thorson and Jessica C. Walsh},
#>     year = {2016},
#>     note = {R package version 0.1.0},
#>     url = {https://github.com/datalimited/datalimited},
#>   }

################################################################################################################################################################################################

#Database
corvina_reina <- structure(list(
  yr = 1960:2014,
  ct = c(152, 291, 235, 198, 282, 324, 290, 209, 197, 111, 125, 175, 164, 374, 447, 348, 371, 308, 334, 190, 228, 250, 295, 515, 390, 358, 353, 291, 252, 204, 156, 188, 263, 198, 172, 165, 159, 203, 260, 247, 185, 209, 218, 159, 137, 171, 203, 254, 187, 220, 188, 194, 176, 179, 194)),
  .Names = c("yr", "ct"),
  class = "data.frame",
  row.names = 1:55)

################################################################################################################################################################################################
#Part 1.
# install.packages("devtools")
devtools::install_github("datalimited/datalimited")

library("datalimited")
set.seed(1)
x <- cmsy(corvina_reina$yr, ct = corvina_reina$ct, reps = 2e4)
names(x)
#> [1] "theta"       "biomass"     "bmsy"        "msy"         "mean_ln_msy"
#> [6] "bbmsy"
par(mfrow = c(2, 2))
plot(corvina_reina$yr, corvina_reina$ct, type = "o", xlab = "Year", 
     ylab = "Catch (t)")
plot(corvina_reina$yr,  apply(x$biomass, 2, median)[-1], type = "o",
     ylab = "Estimated biomass", xlab = "Year")
hist(x$bmsy)
plot(x$theta$r,x$theta$k , col = "#00000030")

#plot(x$theta$r, x$theta$k, col = "#00000030")

################################################################################################
#Part 2
library("ggplot2")
ggplot(x$bbmsy, aes(year, bbmsy_q50)) + geom_line()  +
  geom_ribbon(aes(ymin = bbmsy_q25, ymax = bbmsy_q75), alpha = 0.2) +
  geom_ribbon(aes(ymin = bbmsy_q2.5, ymax = bbmsy_q97.5), alpha = 0.1) +
  geom_hline(yintercept = 1, lty = 2) + theme_light()

################################################################################################

