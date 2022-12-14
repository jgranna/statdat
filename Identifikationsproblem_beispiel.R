# ------------------------------------------------------------------------------
# Matrix per Hand ausrechnen und Identifikationsproblem 

# dummykodierung
mietspiegel$k.normal <- ifelse(mietspiegel$kueche == "normal", 1, 0)
mietspiegel$k.gehoben <- ifelse(mietspiegel$kueche == "gehoben", 1, 0)
# designmatrix mit covariablen kueche und flaeche, 
# hier: beide Kategorien werden eingebaut (falsch!)
X <- matrix(c(rep(1, nrow(mietspiegel)), mietspiegel$k.normal, 
              mietspiegel$k.gehoben, mietspiegel$flaeche), ncol = 4)
y <- mietspiegel$mieteqm
# kq schaetzer (fuehrt zu error, weil X nicht vollen Rang hat!)
solve(t(X)%*%X)%*%t(X)%*%y  

# designmatrix mit einer referenzkategorie
X <- matrix(c(rep(1, nrow(mietspiegel)), mietspiegel$k.gehoben), ncol = 2)
# Schaetzungen der Koeffizienten fuer Kueche und Flaeche
solve(t(X)%*%X)%*%t(X)%*%y  


