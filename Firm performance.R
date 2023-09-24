

install.packages("goftest")
install.packages("seminr")
library(seminr)
library(goftest)
require(readxl)
require(seminr)
attach(Performance_final)
View(Performance_final)
#===========================================

measurements <- constructs(
  composite("Technical Capability",multi_items("Tech",1:3)),
  composite("Managerial competency",multi_items("Manag",1:3)),
  composite("Human resource expertise",multi_items("Human",1:3)),
  composite("Analytics driven Culture",multi_items("Analy",1:3)),
  higher_composite("Analytics Capability",c("Technical Capability","Managerial competency","Human resource expertise","Analytics driven Culture"), method = "two stage"),
  composite("Firm performance",multi_items("Perform",1:5)),
  composite("Visibility",multi_items("Visi",2:4)),
  composite("Agility",multi_items("Agil",1:5)),
  composite("Adaptability",multi_items("Adap",1:4)),
  higher_composite("Dynamic capability",c("Visibility","Agility","Adaptability"), method = "two stage"))

#------------------------------------------
# Quickly create multiple paths "from" and "to" sets of constructs

plot(measurements)

structure_pls = relationships(
  paths(from = "Analytics Capability", to = "Firm performance"),
  paths(from = "Analytics Capability", to = "Dynamic capability"),
  paths(from = "Dynamic capability", to = "Firm performance"))

plot(structure_pls)



#===========================================

pls_model = estimate_pls(Performance_final,
                         measurements,
                         structure_pls)





plot_scores(pls_model)
plot(pls_model, cex=3)
s=summary(pls_model)

s$loadings
s$descriptives
#Validity
s$validity
s$total_effects
s$it_criteria
s$fSquare
s$reliability
s$validity$fl_criteria
s$weights
s$vif_antecedents$Ambidexterity
s$descriptives$statistics$items
s$paths
#Reliability
sink("mock_test.doc")
s$reliability
sink()
#Loadings
s$loadings
s$iterations
s$loadings
#Structural Model
s$paths
s$total_effects
s$total_indirect_effects
s$vif_antecedents
s$fSquare
s$it_criteria
s$composite_scores
s$validity$htmt
# Bootstrap
p5=bootstrap_model(pls_model,nboot = 5000)
s2=summary(p5)
s2
s2$bootstrapped_HTMT
#===========================================