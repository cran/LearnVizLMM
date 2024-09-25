## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = ""
)

## ----pkgInfo, echo=FALSE, fig.width=6, fig.align='right'----------------------
library(LearnVizLMM)
diagram_text <- "\n  digraph {\n    graph [layout = dot, rankdir = LR]\n    edge [color=black]\n    node[shape=box]\n    label1[label = \"Input\", color=white, fontname = \"times-bold\", height = 0.25]\n    label1b[label = \"model \n cat_vars \n cat_vars_nlevels\", fontsize=10, style = dashed, width = 1.15]\n    label1a[label = \"model \n--- OR ---\n n_gf \n gf_description \n gf_names\", fontsize=10, style = dashed, width = 1.15]\n    label2[label = \"Function\", color=white, fontname = \"times-bold\", height = 0.25]\n    H[label = \"extract_equation()\", style = filled, width = 1.5]\n    F[label = \"extract_variables()\", style = filled, width = 1.5]\n    B[label = \"extract_structure()\", style = filled, width = 1.5]\n    label3[label = \"Output\", color=white, fontname = \"times-bold\", height = 0.25]\n    I[label = \"LaTeX model equation\", color=white, fontsize=10, width = 1.5]\n    G[label = \"Data frame of the model \nvariables and descriptions\", color=white, fontsize=10, width = 1.5]\n    C[label = \"Image of the multilevel \ndata structure\", color=white, fontsize=10, width = 1.5]\n    edge [color=white, arrowsize = 1]\n    rank=same {label1 -> label1b -> label1a}\n    label1 -> label2\n    label2 -> label3\n    rank=same {label2 -> H -> F -> B}\n    rank=same {label3 -> I -> G -> C}\n    edge [color=black, arrowsize = 1]\n    label1a -> B\n    label1b -> {F H}\n    edge [minlen=1]\n    B -> C\n    F -> G\n    H -> I\n  }\n   "
DiagrammeR::grViz(diagram_text)

## ----setup, eval=FALSE--------------------------------------------------------
#  library(LearnVizLMM)

## ----lmmPkgs1, eval=FALSE-----------------------------------------------------
#  library(lme4)
#  lme4::lmer(formula = outcome ~ fixed_effects + random_effects)
#  
#  library(nlme)
#  nlme::lme(fixed = outcome ~ fixed_effects, random = random_effects)

## ----lmmPkgs2, eval=FALSE-----------------------------------------------------
#  lmer(formula = Y ~ 1 + X1 + X2 + random_effects)
#  lme(fixed = Y ~ 1 + X1 + X2, random = random_effects)

## ----lmmPkgs3, eval=FALSE-----------------------------------------------------
#  # Random Intercept
#  lmer(formula = outcome ~ fixed_effects + (1|GF))
#  lme(fixed = outcome ~ fixed_effects, random = ~1|GF)
#  lme(fixed = outcome ~ fixed_effects, random = list(GF=~1))
#  # Random Intercept & Slope
#  lmer(formula = outcome ~ fixed_effects + (1 + X1|GF))
#  lme(fixed = outcome ~ fixed_effects, random = ~1 + X1|GF)
#  lme(fixed = outcome ~ fixed_effects, random = list(GF=~1 + X1))

## ----equation1a, eval=FALSE---------------------------------------------------
#  extract_equation(model = "lmer(score ~ 1 + age + (1|subject))")

## ----equation1b, eval=TRUE, echo=FALSE----------------------------------------
extract_equation(model = "lmer(score ~ 1 + age + (1|subject))")

## ----equation2a, eval=FALSE---------------------------------------------------
#  extract_equation(model = "lmer(score ~ 1 + age + (1|subject))",
#                   output_type = "string")

## ----equation2b, eval=TRUE, echo=FALSE----------------------------------------
extract_equation(model = "lmer(score ~ 1 + age + (1|subject))",
                 output_type = "string")

## ----equation3a, eval=FALSE---------------------------------------------------
#  extract_equation(model = "lmer(score ~ 1 + age + (1|subject))",
#                   output_type = "none")

## ----equation3b, eval=TRUE, echo=FALSE----------------------------------------
extract_equation(model = "lmer(score ~ 1 + age + (1|subject))",
                 output_type = "none")

## ----table1a, eval=FALSE------------------------------------------------------
#  extract_variables(model = "lme(weight~1+sex+time+I(time^2), random=~1+time+I(time^2)|ID)")

## ----table1b, eval=TRUE, echo=FALSE, warning=FALSE----------------------------
library(kableExtra)
extract_variables(model = "lme(weight~1+sex+time+I(time^2), random=~1+time+I(time^2)|ID)") %>%
  kbl() %>%
  kable_styling()

## ----table2a, eval=FALSE------------------------------------------------------
#  extract_variables(model = "lme(Score~type+age*sex,random=list(School=pdDiag(~type),Class=~1))",
#                    cat_vars = c("type", "sex"),
#                    cat_vars_nlevels = c(3, 2))

## ----table2b, eval=TRUE, echo=FALSE, warning=FALSE----------------------------
extract_variables(model = "lme(Score~type+age*sex,random=list(School=pdDiag(~type),Class=~1))",
                  cat_vars = c("type", "sex"),
                  cat_vars_nlevels = c(3, 2)) %>%
  kbl() %>%
  kable_styling()

## ----structure1, fig.width=7--------------------------------------------------
extract_structure(n_gf = 1, 
                  gf_names = "Subject")

## ----structure1b, fig.width=7-------------------------------------------------
extract_structure(model = "lme(Weight ~ Time, random=~Time|Subject, data)",
                  gf_nlevels = 47)

## ----structure1c, fig.width=7-------------------------------------------------
extract_structure(model = "lme(Weight ~ Time, random=~Time|Subject, data)",
                  gf_nlevels = "m", 
                  label_levels = "no")

## ----structure2cross, fig.width=7---------------------------------------------
extract_structure(model = "lmer(Strength ~ 1 + (1|Machine) + (1|Worker))",
                  gf_nlevels = c(10, 20))

## ----structure2nest, fig.width=7----------------------------------------------
extract_structure(model = "lme(score~type, random=list(school=pdDiag(~1+type),class=~1))")

## ----structure2alt, eval=FALSE------------------------------------------------
#  extract_structure(n_gf = 2,
#                    gf_description = "crossed",
#                    gf_names = c("Machine", "Worker"),
#                    gf_nlevels = c(10, 20))
#  
#  extract_structure(n_gf = 2,
#                    gf_description = "nested",
#                    gf_names = c("school", "class"))

## ----structure3, fig.width=7--------------------------------------------------
extract_structure(n_gf = 3,
                  gf_description = "crossed",
                  gf_names = c("District", "School", "Class"),
                  gf_nlevels = c(8, 15, 5))

## ----structure3b, fig.width=7-------------------------------------------------
extract_structure(n_gf = 3,
                  gf_description = "nested",
                  gf_names = c("District", "School", "Class"),
                  gf_nlevels = c(8, 15, 5),
                  gf3_index = "q")

## ----structure3c, fig.width=7-------------------------------------------------
extract_structure(n_gf = 3,
                  gf_description = "crossed with nested",
                  gf_names = c("GF1", "GF2", "GF3"))

## ----structure3d, fig.width=7-------------------------------------------------
extract_structure(n_gf = 3,
                  gf_description = "crossed within nested",
                  gf_names = c("GF1", "GF2", "GF3"))

## ----structureExport, fig.width=7---------------------------------------------
diagram_text <- extract_structure(n_gf = 1, 
                                  gf_names = "Subject",
                                  export_type = "text")

DiagrammeR::grViz(diagram_text)

## ----scope1, eval=FALSE-------------------------------------------------------
#  # One
#  (1|GF)
#  random=~1|GF
#  random=list(GF=~1)
#  # Two
#  (1|GF1)+(1|GF2)
#  (1|GF1/GF2)
#  random=~1|GF1/GF2
#  # Three
#  (1|GF1/GF2/GF3)
#  (1|GF1)+(1|GF2/GF3)
#  random=list(GF1=~1,GF2=~1,GF3=~1)

## ----scope2, eval=FALSE-------------------------------------------------------
#  # One
#  (1|GF)
#  # Two
#  (1+X1|GF)
#  (X1|GF)
#  # Three
#  (1+X1+X2|GF)
#  (X1+X2|GF)
#  # Four
#  (1+X1+X2+X3|GF)
#  (X1+X2+X3|GF)

## ----scope3, eval=FALSE-------------------------------------------------------
#  # Two-way
#  X1 + X2 + X1:X2
#  X1*X2
#  # Three-way
#  X1 + X2 + X3 + X1:X2 + X1:X3 + X2:X3 + X1:X2:X3
#  X1*X2*X3
#  # Four-way
#  X1 + X2 + X3 + X3 + X1:X2 + X1:X3 + X1:X4 + X2:X3 + X2:X4 +
#    X1:X2:X3 + X1:X2:X4 + X1:X4:X3 + X4:X2:X3 + X1:X2:X3:X4
#  X1*X2*X3*X4 # does not work

## ----tips1, eval=FALSE--------------------------------------------------------
#  # works
#  model = "lmer(formula = outcome ~ fixed_effects + random_effects)"
#  model = "lmer(outcome ~ fixed_effects + random_effects)"
#  model = "lmer(outcome ~ fixed_effects + random_effects, data, ...)"
#  # does not work
#  model = "lmer(data, formula = outcome ~ fixed_effects + random_effects)"
#  model = "lmer(data, outcome ~ fixed_effects + random_effects)"

## ----tips2, eval=FALSE--------------------------------------------------------
#  # works
#  model = "lme(fixed = outcome ~ fixed_effects, random = random_effects, data, ...)"
#  model = "lme(outcome ~ fixed_effects, random = random_effects)"
#  # does not work
#  model = "lme(outcome ~ fixed_effects, random_effects, data, ...)"
#  model = "lme(random = random_effects, fixed = outcome ~ fixed_effects)"

