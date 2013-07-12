## environments for parallel computing
.DcloneEnvModel <- new.env(parent=emptyenv())
.DcloneEnvResults <- new.env(parent=emptyenv())
if(getRversion() >= "2.15.1")
    utils::globalVariables(c(".DcloneEnvModel", ".DcloneEnvResults"))

pullDcloneEnv <- 
function (x, type = c("model", "results"))
{
    switch(match.arg(type),
        "model" = get(x, envir = .DcloneEnvModel),
        "results" = get(x, envir = .DcloneEnvResults))
}

pushDcloneEnv <- 
function (x, value, type = c("model", "results"))
{
    switch(match.arg(type),
        "model" = assign(x, value, envir = .DcloneEnvModel),
        "results" = assign(x, value, envir = .DcloneEnvResults))
}

clearDcloneEnv <- 
function(..., list = character(), 
    type = c("model", "results"))
{
    switch(match.arg(type),
        "model" = rm(..., list = list, envir = .DcloneEnvModel),
        "results" = rm(..., list = list, envir = .DcloneEnvResults))
}

listDcloneEnv <- 
function (type = c("model", "results"))
{
    switch(match.arg(type),
        "model" = ls(envir = .DcloneEnvModel),
        "results" = ls(envir = .DcloneEnvResults))
}

existsDcloneEnv <- 
function (x, type = c("model", "results"),
    mode = "any", inherits = TRUE)
{
    switch(match.arg(type),
        "model" = exists(x, envir = .DcloneEnvModel, 
            mode = mode, inherits = inherits),
        "results" = exists(x, envir = .DcloneEnvResults,
            mode = mode, inherits = inherits))
}

