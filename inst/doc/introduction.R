## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment  = "#>"
)

## ----setup--------------------------------------------------------------------
library(roundRobinR)

## ----examine-data-------------------------------------------------------------
head(sampleDyadData)
nrow(sampleDyadData)
length(unique(sampleDyadData$groupId))

## ----create-dummies-----------------------------------------------------------
d <- createDummies(
  group.id       = "groupId",
  act.id         = "actId",
  part.id        = "partId",
  d              = sampleDyadData[sampleDyadData$timeId == 1, ],
  merge.original = TRUE
)
head(d[, c("groupId", "actId", "partId", "pdSRM_dyad_id",
           "a1", "a2", "a3", "a4", "p1", "p2", "p3", "p4")])

## ----null-model---------------------------------------------------------------
null_mod <- srmRun(
  dv      = "liking",
  groupId = "groupId",
  actId   = "actId",
  partId  = "partId",
  data    = sampleDyadData[sampleDyadData$timeId == 1, ]
)
null_mod$srm.output

## ----full-model---------------------------------------------------------------
full_mod <- srmRun(
  dv      = "liking",
  groupId = "groupId",
  actId   = "actId",
  partId  = "partId",
  feVars  = c("actEx", "partEx", "contact"),
  data    = sampleDyadData[sampleDyadData$timeId == 1, ]
)
full_mod$srm.output

## ----fixed-effects------------------------------------------------------------
summary(full_mod$lme.output)$tTable

## ----pseudo-r2----------------------------------------------------------------
srmPseudoRSq(
  null.model    = null_mod$lme.output,
  predict.model = full_mod$lme.output
)

