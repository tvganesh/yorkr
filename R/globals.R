
##########################################################################################
##########################################################################################
# Designed and developed by Tinniam V Ganesh
# Date : 25 Dec 2022
# Function: winProbabiltyRF
# This function computes  the ball by ball win probability using Random Forest  model
#
###########################################################################################
#' @title
#' globals.R
#'
#' @description
#' quiets concerns of R CMD check re: the .'s that appear in pipelines
#'
#' @name globals
if(getRversion() >= "2.15.1")  utils::globalVariables(c("final_lr_model","final_model","dl_model","gan_model"))
