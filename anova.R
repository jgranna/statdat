anova(m_glueck)
m0 <- lm(Happy ~ 1, data = m4$model)
m1 <- lm(Happy ~ logGDP, data = m4$model)
m2 <- lm(Happy ~ logGDP + LifeExp, data = m4$model)
m3 <- lm(Happy ~ logGDP + LifeExp + Freedom2, data = m4$model)
m4 <- lm(Happy ~ logGDP + LifeExp + Freedom2 + Corruption2, data = WHR2019)
anova(m0, m1, m2, m3, m4)
anova(m_glueck)

# Sollte Corruption2 zusätzlich zu Freedom2, LifeExp und logGDP aufgenommen werden?
anova(m3, m4)
anova(m4, m3)
