###############################################################################
# chatLLM helpers + call_llm()                                                #
# Requires: httr (>= 1.4.6), jsonlite                                         #
###############################################################################
# ------------------------------------------------------------------------------
#' @importFrom httr POST add_headers http_error content
#' @importFrom jsonlite toJSON fromJSON
#' @importFrom httr GET timeout status_code
#' @importFrom stats setNames

library(httr)
library(jsonlite)

`%||%` <- function(x, y) if (is.null(x)) y else x

###############################################################################
# 1. Provider defaults                                                        #
###############################################################################
get_default_model <- function(provider) {
  switch(
    tolower(provider),
    "openai"    = "gpt-3.5-turbo",
    "groq"      = "meta-llama/llama-4-scout-17b-16e-instruct",
    "anthropic" = "claude-3-7-sonnet-20250219",
    "deepseek"  = "deepseek-chat",
    "dashscope" = "qwen-plus-latest",
    "github"    = "openai/gpt-4.1",
    "gemini"    = "gemini-2.0-flash",
    "grok"      = "grok-3-latest",
    "azure_openai" = {
      deployment <- Sys.getenv("AZURE_OPENAI_DEPLOYMENT")
      if (!nzchar(deployment)) {
        stop(
          "No default deployment for provider: azure_openai. ",
          "Set AZURE_OPENAI_DEPLOYMENT or pass `model`.",
          call. = FALSE
        )
      }
      deployment
    },
    "azure_foundry" = {
      model <- Sys.getenv("AZURE_FOUNDRY_MODEL")
      if (!nzchar(model)) {
        stop(
          "No default model for provider: azure_foundry. ",
          "Set AZURE_FOUNDRY_MODEL or pass `model`.",
          call. = FALSE
        )
      }
      model
    },
    "bedrock"   = {
      model <- Sys.getenv("AWS_BEDROCK_MODEL")
      if (!nzchar(model)) {
        stop(
          "No default model for provider: bedrock. Set AWS_BEDROCK_MODEL or pass `model`.",
          call. = FALSE
        )
      }
      model
    },
    stop("No default model for provider: ", provider)
  )
}

###############################################################################
# 2. API-key helper                                                           #
###############################################################################
get_api_key <- function(provider, api_key = NULL) {
  env_var <- switch(
    tolower(provider),
    "openai"    = "OPENAI_API_KEY",
    "groq"      = "GROQ_API_KEY",
    "anthropic" = "ANTHROPIC_API_KEY",
    "deepseek"  = "DEEPSEEK_API_KEY",
    "dashscope" = "DASHSCOPE_API_KEY",
    "github"    = "GH_MODELS_TOKEN",
    "gemini"    = "GEMINI_API_KEY",
    "grok"      = "XAI_API_KEY",
    "azure_openai" = "AZURE_OPENAI_API_KEY",
    "azure_foundry" = "AZURE_FOUNDRY_API_KEY",
    "bedrock"   = "",
    stop("Unknown provider: ", provider)
  )
  if (!nzchar(env_var)) return("")
  if (is.null(api_key)) api_key <- Sys.getenv(env_var)
  if (!nzchar(api_key))
    stop(sprintf("API key not found for %s.  Set %s or pass `api_key`.",
                 provider, env_var))
  api_key
}

###############################################################################
# 3. Parse chat-completion responses                                          #
###############################################################################
parse_response <- function(provider, parsed) {
  switch(
    tolower(provider),
    "openai"    = parsed$choices[[1]]$message$content,
    "groq"      = parsed$choices[[1]]$message$content,
    "anthropic" = parsed$content[[1]]$text,
    "deepseek"  = parsed$choices[[1]]$message$content,
    "dashscope" = parsed$choices[[1]]$message$content,
    "github"    = parsed$choices[[1]]$message$content,
    "gemini"    = parsed$choices[[1]]$message$content,
    "grok"      = parsed$choices[[1]]$message$content,
    "azure_openai" = parsed$choices[[1]]$message$content,
    "azure_foundry" = parsed$choices[[1]]$message$content,
    "bedrock"   = {
      blocks <- parsed$output$message$content %||% list()
      if (!length(blocks)) return("")
      paste(
        vapply(blocks, function(x) as.character(x$text %||% ""), character(1)),
        collapse = "\n"
      )
    },
    stop("Parsing not implemented for provider: ", provider)
  )
}

build_aws_sigv4_headers <- function(
    service,
    region,
    verb,
    action,
    request_body = "",
    content_type = "application/json",
    accept = "application/json",
    aws_access_key_id = NULL,
    aws_secret_access_key = NULL,
    aws_session_token = NULL
) {
  if (!requireNamespace("aws.signature", quietly = TRUE)) {
    stop(
      "Package 'aws.signature' is required for provider='bedrock'. ",
      "Install with install.packages('aws.signature').",
      call. = FALSE
    )
  }

  aws_access_key_id <- if (!is.null(aws_access_key_id) && nzchar(aws_access_key_id)) aws_access_key_id else NULL
  aws_secret_access_key <- if (!is.null(aws_secret_access_key) && nzchar(aws_secret_access_key)) aws_secret_access_key else NULL
  aws_session_token <- if (!is.null(aws_session_token) && nzchar(aws_session_token)) aws_session_token else NULL

  host <- switch(
    service,
    "bedrock" = sprintf("bedrock.%s.amazonaws.com", region),
    "bedrock-runtime" = sprintf("bedrock-runtime.%s.amazonaws.com", region),
    stop("Unsupported AWS service for signing: ", service, call. = FALSE)
  )

  amz_date <- format(Sys.time(), "%Y%m%dT%H%M%SZ", tz = "UTC")
  canonical_headers <- list(
    host = host,
    `x-amz-date` = amz_date,
    `content-type` = content_type
  )
  if (!is.null(accept) && nzchar(accept)) canonical_headers$accept <- accept
  if (!is.null(aws_session_token)) {
    canonical_headers$`x-amz-security-token` <- aws_session_token
  }

  force_creds <- any(vapply(
    list(aws_access_key_id, aws_secret_access_key, aws_session_token),
    function(x) !is.null(x) && nzchar(x),
    logical(1)
  ))

  sig <- tryCatch(
    aws.signature::signature_v4_auth(
      datetime = amz_date,
      region = region,
      service = service,
      verb = verb,
      action = action,
      query_args = list(),
      canonical_headers = canonical_headers,
      request_body = request_body,
      signed_body = TRUE,
      key = aws_access_key_id,
      secret = aws_secret_access_key,
      session_token = aws_session_token,
      force_credentials = force_creds
    ),
    error = function(e) {
      stop(
        "Unable to sign AWS request for provider='bedrock': ", e$message, "\n",
        "Set AWS credentials (AWS_ACCESS_KEY_ID, AWS_SECRET_ACCESS_KEY, ",
        "optional AWS_SESSION_TOKEN) and AWS region (AWS_REGION or AWS_DEFAULT_REGION).",
        call. = FALSE
      )
    }
  )

  headers <- list(
    Authorization = sig$SignatureHeader,
    `X-Amz-Date` = amz_date,
    Host = host,
    `Content-Type` = content_type
  )
  if (!is.null(accept) && nzchar(accept)) headers$Accept <- accept
  if (!is.null(sig$SessionToken) && nzchar(sig$SessionToken)) {
    headers$`X-Amz-Security-Token` <- sig$SessionToken
  }

  list(headers = headers, host = host)
}

to_bedrock_message <- function(msg) {
  role <- tolower(as.character(msg$role %||% "user"))
  if (!(role %in% c("user", "assistant"))) role <- "user"

  content <- msg$content
  blocks <- list()

  if (is.character(content)) {
    blocks <- lapply(as.character(content), function(x) list(text = x))
  } else if (is.list(content) && length(content)) {
    blocks <- lapply(content, function(block) {
      if (is.list(block) && !is.null(block$text)) {
        list(text = as.character(block$text))
      } else {
        list(text = as.character(block))
      }
    })
  } else {
    blocks <- list(list(text = ""))
  }

  list(role = role, content = blocks)
}

build_bedrock_request_body <- function(messages, temperature, max_tokens, extra_args = list()) {
  roles <- vapply(messages, function(m) tolower(as.character(m$role %||% "user")), character(1))
  system_idx <- which(roles == "system")

  system_blocks <- if (length(system_idx)) {
    lapply(messages[system_idx], function(m) list(text = as.character(m$content %||% "")))
  } else {
    list()
  }

  chat_messages <- if (length(system_idx)) messages[-system_idx] else messages
  chat_messages <- lapply(chat_messages, to_bedrock_message)
  if (!length(chat_messages)) {
    chat_messages <- list(list(role = "user", content = list(list(text = ""))))
  }

  inference_config <- list(
    maxTokens = as.integer(max_tokens),
    temperature = as.numeric(temperature)
  )

  if (!is.null(extra_args$top_p)) {
    inference_config$topP <- as.numeric(extra_args$top_p)
    extra_args$top_p <- NULL
  }
  if (!is.null(extra_args$topP)) {
    inference_config$topP <- as.numeric(extra_args$topP)
    extra_args$topP <- NULL
  }
  if (!is.null(extra_args$stop)) {
    inference_config$stopSequences <- as.character(unlist(extra_args$stop, use.names = FALSE))
    extra_args$stop <- NULL
  }
  if (!is.null(extra_args$stopSequences)) {
    inference_config$stopSequences <- as.character(unlist(extra_args$stopSequences, use.names = FALSE))
    extra_args$stopSequences <- NULL
  }
  if (!is.null(extra_args$inferenceConfig) && is.list(extra_args$inferenceConfig)) {
    inference_config <- utils::modifyList(inference_config, extra_args$inferenceConfig)
    extra_args$inferenceConfig <- NULL
  }

  body <- list(messages = chat_messages, inferenceConfig = inference_config)
  if (length(system_blocks)) body$system <- system_blocks
  if (length(extra_args)) body <- c(body, extra_args)
  body
}

###############################################################################
# 4. Model-catalog helpers (one per provider)                                 #
###############################################################################
get_openai_models <- function(token = Sys.getenv("OPENAI_API_KEY")) {
  if (!nzchar(token)) return(character())
  r <- GET("https://api.openai.com/v1/models",
           add_headers(Authorization = paste("Bearer", token)),
           timeout(60))
  if (http_error(r)) return(character())
  vapply(content(r, "parsed")$data, `[[`, character(1), "id")
}

get_grok_models <- function(token = Sys.getenv("XAI_API_KEY")) {
  if (!nzchar(token)) return(character())
  r <- GET("https://api.x.ai/v1/models",
           add_headers(Authorization = paste("Bearer", token)),
           timeout(60))
  if (http_error(r)) return(character())
  vapply(content(r, "parsed")$data, `[[`, character(1), "id")
}

get_gemini_models <- function(token = Sys.getenv("GEMINI_API_KEY")) {
  if (!nzchar(token)) return(character())
  r <- GET("https://generativelanguage.googleapis.com/v1beta/openai/models",
           add_headers(Authorization = paste("Bearer", token)),
           timeout(60))
  if (http_error(r)) return(character())
  vapply(content(r, "parsed")$data, `[[`, character(1), "id")
}

get_groq_models <- function(token = Sys.getenv("GROQ_API_KEY")) {
  if (!nzchar(token)) return(character())
  r <- GET("https://api.groq.com/openai/v1/models",
           add_headers(Authorization = paste("Bearer", token)),
           timeout(60))
  if (http_error(r)) return(character())
  vapply(content(r, "parsed")$data, `[[`, character(1), "id")
}

get_anthropic_models <- function(token = Sys.getenv("ANTHROPIC_API_KEY"),
                                 anthropic_api_version = "2023-06-01",
                                 limit = 1000) {
  if (!nzchar(token)) return(character())
  r <- tryCatch(
    GET("https://api.anthropic.com/v1/models",
        add_headers(`x-api-key` = token,
                    `anthropic-version` = anthropic_api_version),
        query = list(limit = limit),
        timeout(60)),
    error = function(e) NULL)
  if (is.null(r) || http_error(r)) return(character())
  p <- content(r, "parsed")
  if (!is.null(p$data))
    return(vapply(p$data, `[[`, character(1), "id"))
  if (!is.null(p$models))
    return(vapply(p$models, `[[`, character(1), "name"))
  character()
}

get_deepseek_models <- function(token = Sys.getenv("DEEPSEEK_API_KEY")) {
  if (!nzchar(token)) return(character())
  r <- GET("https://api.deepseek.com/v1/models",
           add_headers(Authorization = paste("Bearer", token)),
           timeout(60))
  if (http_error(r)) return(character())
  vapply(content(r, "parsed")$data, `[[`, character(1), "id")
}

get_dashscope_models <- function(token = Sys.getenv("DASHSCOPE_API_KEY")) {
  if (!nzchar(token)) return(character())
  r <- GET("https://dashscope-intl.aliyuncs.com/compatible-mode/v1/models",
           add_headers(Authorization = paste("Bearer", token)),
           timeout(60))
  if (http_error(r)) return(character())
  vapply(content(r, "parsed")$data, `[[`, character(1), "id")
}

get_all_github_models <- function(token       = Sys.getenv("GH_MODELS_TOKEN"),
                                  api_version = "2022-11-28") {
  if (!nzchar(token)) return(character())
  r <- GET("https://models.github.ai/catalog/models",
           add_headers(Accept = "application/vnd.github+json",
                       Authorization = paste("Bearer", token),
                       `X-GitHub-Api-Version` = api_version),
           timeout(60))
  if (http_error(r)) return(character())
  vapply(content(r, "parsed", simplifyVector = FALSE),
         `[[`, character(1), "id")
}

get_azure_openai_models <- function(
    token = Sys.getenv("AZURE_OPENAI_API_KEY"),
    endpoint = Sys.getenv("AZURE_OPENAI_ENDPOINT"),
    azure_api_version = Sys.getenv("AZURE_OPENAI_API_VERSION", unset = "2024-02-15-preview"),
    deployment = Sys.getenv("AZURE_OPENAI_DEPLOYMENT"),
    .get_func = httr::GET
) {
  fallback <- if (nzchar(deployment)) deployment else character()
  if (!nzchar(token) || !nzchar(endpoint)) return(fallback)

  base <- sub("/+$", "", endpoint)
  encoded_ver <- utils::URLencode(azure_api_version, reserved = TRUE)

  # Prefer deployment listing since call_llm(model=...) expects deployment names.
  deploy_url <- sprintf("%s/openai/deployments?api-version=%s", base, encoded_ver)
  rd <- tryCatch(
    .get_func(
      deploy_url,
      add_headers(`api-key` = token),
      timeout(60)
    ),
    error = function(e) NULL
  )

  deployment_ids <- character()
  if (!is.null(rd) && !http_error(rd)) {
    pd <- content(rd, "parsed")
    deployment_ids <- vapply(pd$data %||% list(), function(x) as.character(x$id %||% ""), character(1))
    deployment_ids <- deployment_ids[nzchar(deployment_ids)]
  }

  if (length(deployment_ids)) return(unique(c(deployment_ids, fallback)))

  # Fallback endpoint for model catalog where deployments endpoint is unavailable.
  models_url <- sprintf("%s/openai/models?api-version=%s", base, encoded_ver)
  rm <- tryCatch(
    .get_func(
      models_url,
      add_headers(`api-key` = token),
      timeout(60)
    ),
    error = function(e) NULL
  )

  model_ids <- character()
  if (!is.null(rm) && !http_error(rm)) {
    pm <- content(rm, "parsed")
    model_ids <- vapply(pm$data %||% list(), function(x) as.character(x$id %||% ""), character(1))
    model_ids <- model_ids[nzchar(model_ids)]
  }

  unique(c(model_ids, fallback))
}

get_azure_foundry_models <- function(
    token = Sys.getenv("AZURE_FOUNDRY_API_KEY"),
    endpoint = Sys.getenv("AZURE_FOUNDRY_ENDPOINT"),
    azure_foundry_api_version = Sys.getenv("AZURE_FOUNDRY_API_VERSION", unset = "2024-05-01-preview"),
    azure_foundry_token = Sys.getenv("AZURE_FOUNDRY_TOKEN"),
    model = Sys.getenv("AZURE_FOUNDRY_MODEL"),
    .get_func = httr::GET
) {
  fallback <- if (nzchar(model)) model else character()
  if (!nzchar(endpoint)) return(fallback)

  has_token <- is.character(azure_foundry_token) && nzchar(azure_foundry_token)
  has_key <- is.character(token) && nzchar(token)
  if (!has_token && !has_key) return(fallback)

  base <- sub("/+$", "", endpoint)
  encoded_ver <- utils::URLencode(azure_foundry_api_version, reserved = TRUE)

  models_base <- if (grepl("/models($|\\?)", base, ignore.case = TRUE)) {
    base
  } else {
    paste0(base, "/models")
  }

  url <- if (grepl("api-version=", models_base, ignore.case = TRUE)) {
    models_base
  } else {
    paste0(
      models_base,
      if (grepl("\\?", models_base)) "&" else "?",
      "api-version=", encoded_ver
    )
  }

  headers <- if (has_token) {
    add_headers(
      Authorization = paste("Bearer", azure_foundry_token),
      "Content-Type" = "application/json"
    )
  } else {
    add_headers(
      `api-key` = token,
      "Content-Type" = "application/json"
    )
  }

  r <- tryCatch(
    .get_func(url, headers, timeout(60)),
    error = function(e) NULL
  )
  if (is.null(r) || http_error(r)) return(fallback)

  parsed <- content(r, "parsed")
  items <- parsed$data %||% parsed$models %||% list()
  ids <- vapply(items, function(x) {
    id <- as.character(
      x$id %||% x$model %||% x$modelId %||% x$name %||% ""
    )
    if (length(id) == 0) "" else id[[1]]
  }, character(1))
  ids <- ids[nzchar(ids)]

  unique(c(ids, fallback))
}

get_bedrock_models <- function(
    region = Sys.getenv("AWS_REGION", unset = Sys.getenv("AWS_DEFAULT_REGION", unset = "us-east-1")),
    aws_access_key_id = Sys.getenv("AWS_ACCESS_KEY_ID"),
    aws_secret_access_key = Sys.getenv("AWS_SECRET_ACCESS_KEY"),
    aws_session_token = Sys.getenv("AWS_SESSION_TOKEN"),
    .get_func = httr::GET
) {
  if (!nzchar(region)) return(character())

  action <- "/foundation-models"
  signed <- build_aws_sigv4_headers(
    service = "bedrock",
    region = region,
    verb = "GET",
    action = action,
    request_body = "",
    content_type = "application/json",
    accept = "application/json",
    aws_access_key_id = aws_access_key_id,
    aws_secret_access_key = aws_secret_access_key,
    aws_session_token = aws_session_token
  )

  r <- tryCatch(
    .get_func(
      paste0("https://", signed$host, action),
      do.call(add_headers, signed$headers),
      timeout(60)
    ),
    error = function(e) NULL
  )

  if (is.null(r) || http_error(r)) return(character())
  parsed <- content(r, "parsed")
  models <- parsed$modelSummaries %||% list()
  if (!length(models)) return(character())
  vapply(models, function(x) as.character(x$modelId %||% ""), character(1))
}

###############################################################################
# 5. list_models()                                                            #
###############################################################################
#' List Available Models for Supported Providers
#'
#' @name list_models
#'
#' @description
#' Retrieve the catalog of available model IDs for one or all supported
#' chat - completion providers. Useful for discovering active models and
#' avoiding typos or deprecated defaults.
#'
#' Supported providers:
#' \itemize{
#'   \item \code{"openai"}     -  OpenAI Chat Completions API
#'   \item \code{"groq"}       -  Groq OpenAI - compatible endpoint
#'   \item \code{"anthropic"}  -  Anthropic Claude API
#'   \item \code{"deepseek"}   -  DeepSeek chat API
#'   \item \code{"dashscope"}  -  Alibaba DashScope compatible API
#'   \item \code{"github"}     -  GitHub Models OpenAI - compatible API
#'   \item \code{"azure_openai"} - Azure OpenAI deployments/models
#'   \item \code{"azure_foundry"} - Azure AI Foundry chat/models endpoints
#'   \item \code{"bedrock"}    -  AWS Bedrock (Converse API)
#'   \item \code{"all"}        -  Fetch catalogs for all of the above
#' }
#'
#' @param provider Character. One of \code{"github"}, \code{"openai"},
#'   \code{"groq"}, \code{"anthropic"}, \code{"deepseek"},
#'   \code{"dashscope"}, \code{"azure_openai"}, \code{"azure_foundry"},
#'   \code{"bedrock"} or \code{"all"}.
#'   Case - insensitive.
#' @param ... Additional arguments passed to the per - provider helper
#'   (e.g. \code{limit} for Anthropic, or \code{api_version} for GitHub).
#' @param github_api_version Character. Header value for
#'   \code{X - GitHub - Api - Version} (GitHub Models). Default \code{"2022 - 11 - 28"}.
#' @param anthropic_api_version Character. Header value for
#'   \code{anthropic - version} (Anthropic). Default \code{"2023 - 06 - 01"}.
#' @param azure_api_version Character. Query version for Azure OpenAI listing.
#'   Default \code{"2024-02-15-preview"}.
#' @param azure_foundry_api_version Character. Query version for Azure AI Foundry
#'   model listing. Default \code{"2024-05-01-preview"}.
#'
#' @return
#' If \code{provider != "all"}, a character vector of model IDs for that
#' single provider. If \code{provider == "all"}, a named list of character
#' vectors, one per provider.
#'
#' @examples
#' \dontrun{
#' Sys.setenv(OPENAI_API_KEY = "sk-...")
#' openai_models <- list_models("openai")
#' head(openai_models)
#'
#' Sys.setenv(ANTHROPIC_API_KEY = "sk-...")
#' anthro_models <- list_models("anthropic", anthropic_api_version = "2023-06-01")
#'
#' Sys.setenv(GH_MODELS_TOKEN = "ghp-...")
#' github_models <- list_models("github", github_api_version = "2022-11-28")
#' }
#'
#' @seealso
#'   \code{\link{call_llm}}
#' @export
NULL

list_models <- function(provider = c("github","openai","groq",
                                     "anthropic","deepseek","dashscope",
                                     "gemini","grok","azure_openai","azure_foundry","bedrock","all"),
                        ...) {
  provider <- match.arg(tolower(provider),
                        c("github","openai","groq",
                          "anthropic","deepseek","dashscope",
                          "gemini","grok","azure_openai","azure_foundry","bedrock","all"))

  fetch <- switch(
    provider,
    "openai"    = get_openai_models,
    "groq"      = get_groq_models,
    "anthropic" = get_anthropic_models,
    "deepseek"  = get_deepseek_models,
    "dashscope" = get_dashscope_models,
    "gemini"    = get_gemini_models,   # new
    "grok"      = get_grok_models,     # new
    "azure_openai" = get_azure_openai_models,
    "azure_foundry" = get_azure_foundry_models,
    "bedrock"   = get_bedrock_models,
    "github"    = function(...) get_all_github_models(...),
    "all"       = NULL
  )

  if (!is.null(fetch)) {
    mods <- tryCatch(fetch(...), error = function(e) character())
    if (length(mods) == 0)
      message(sprintf("No model catalog returned for '%s'.", provider))
    return(mods)
  }

  provs <- c("openai","groq","anthropic","deepseek",
             "dashscope","github","gemini","grok","azure_openai","azure_foundry","bedrock")
  setNames(lapply(provs, function(p) {
    tryCatch(list_models(p, ...), error = function(e) character())
  }), provs)
}


###############################################################################
# 6. Core chat-completion wrapper                                             #
###############################################################################
#' Core chat - completion wrapper for multiple providers
#'
#' @title Unified chat - completion interface
#' @name call_llm
#' @description
#' A unified wrapper for several "OpenAI - compatible" chat - completion APIs
#' (OpenAI, Groq, Anthropic, DeepSeek, Alibaba DashScope, GitHub Models, Grok, Gemini)
#' plus Azure OpenAI, Azure AI Foundry, and AWS Bedrock via the Converse API.
#' Accepts either a single `prompt` **or** a full `messages` list, adds the
#' correct authentication headers, retries on transient failures, and returns
#' the assistant's text response. You can toggle informational console
#' output with `verbose = TRUE/FALSE`. If the chosen `model` is no longer
#' available, the function stops early and suggests running
#' `list_models("<provider>")`.
#'
#' @section Messages:
#' * `prompt`    -  character scalar treated as a single *user* message.
#' * `messages`  -  list of lists; each element must contain `role` and `content`.
#'                If both arguments are supplied, the `prompt` is appended
#'                as an extra user message.
#'
#' @param prompt   Character. Single user prompt (optional if `messages`).
#' @param messages List. Full chat history; see *Messages*.
#' @param provider Character. One of `"openai"`, `"groq"`, `"anthropic"`,
#'                 `"deepseek"`, `"dashscope"`,`"grok"`, `"gemini"`, `"github"`,
#'                 `"azure_openai"`, `"azure_foundry"`, or `"bedrock"`.
#' @param model    Character. Model ID. If `NULL`, uses the provider default.
#' @param temperature Numeric. Sampling temperature (0 - 2). Default `0.7`.
#' @param max_tokens  Integer. Max tokens to generate. Default `1000`.
#' @param api_key     Character. Override API key; if `NULL`, uses the
#'                    environment variable for that provider.
#' @param n_tries     Integer. Retry attempts on failure. Default `3`.
#' @param backoff     Numeric. Seconds between retries. Default `2`.
#' @param verbose     Logical. Whether to display informational messages
#'                    (`TRUE`) or suppress them (`FALSE`). Default `TRUE`.
#' @param endpoint_url Character. Custom endpoint; if `NULL`, a sensible
#'                    provider - specific default is used.
#' @param github_api_version Character. Header `X - GitHub - Api - Version`.
#'                           Default `"2022 - 11 - 28"`.
#' @param anthropic_api_version Character. Header `anthropic - version`.
#'                             Default `"2023 - 06 - 01"`.
#' @param azure_api_version Character. Query param used by Azure OpenAI.
#'                          Default comes from `AZURE_OPENAI_API_VERSION`
#'                          (fallback `"2024-02-15-preview"`).
#' @param azure_endpoint Character. Azure OpenAI resource endpoint,
#'                       e.g. `https://my-resource.openai.azure.com`.
#' @param azure_foundry_api_version Character. Query param used by Azure AI
#'                                  Foundry chat/model endpoints. Default comes
#'                                  from `AZURE_FOUNDRY_API_VERSION`
#'                                  (fallback `"2024-05-01-preview"`).
#' @param azure_foundry_endpoint Character. Azure AI Foundry endpoint base,
#'                               e.g. `https://my-foundry.models.ai.azure.com`.
#' @param azure_foundry_token Character. Optional bearer token for Azure AI
#'                            Foundry (`Authorization: Bearer ...`). If set,
#'                            this is used instead of `AZURE_FOUNDRY_API_KEY`.
#' @param aws_region Character. AWS region for `provider = "bedrock"`.
#'                   Defaults to `AWS_REGION`, then `AWS_DEFAULT_REGION`,
#'                   then `"us-east-1"`.
#' @param aws_access_key_id Character. Optional AWS access key ID override.
#' @param aws_secret_access_key Character. Optional AWS secret access key override.
#' @param aws_session_token Character. Optional AWS session token override.
#' @param ...         Extra JSON - body fields (e.g. `top_p`, `stop`,
#'                    `presence_penalty`).
#' @param .post_func  Internal. HTTP POST function (default `httr::POST`).
#'
#' @return Character scalar: assistant reply text.
#'
#' @examples
#' \dontrun{
#'
#' ## 1. Listing available models
#' # List all providers at once
#' all_mods <- list_models("all")
#' str(all_mods)
#'
#' # List OpenAI-only, Groq-only, Anthropic-only
#' openai_mods   <- list_models("openai")
#' groq_mods     <- list_models("groq")
#' anthropic_mods<- list_models("anthropic", anthropic_api_version = "2023-06-01")
#'
#' ## 2. Single-prompt interface
#'
#' # 2a. Basic usage
#' Sys.setenv(OPENAI_API_KEY = "sk-...")
#' res_basic <- call_llm(
#'   prompt   = "Hello, how are you?",
#'   provider = "openai"
#' )
#' cat(res_basic)
#'
#' # 2b. Adjust sampling and penalties
#' res_sampling <- call_llm(
#'   prompt      = "Write a haiku about winter",
#'   provider    = "openai",
#'   temperature = 1.2,
#'   top_p       = 0.5,
#'   presence_penalty  = 0.6,
#'   frequency_penalty = 0.4
#' )
#' cat(res_sampling)
#'
#' # 2c. Control length and retries
#' res_len <- call_llm(
#'   prompt      = "List 5 uses for R",
#'   provider    = "openai",
#'   max_tokens  = 50,
#'   n_tries     = 5,
#'   backoff     = 0.5
#' )
#' cat(res_len)
#'
#' # 2d. Using stop sequences
#' res_stop <- call_llm(
#'   prompt   = "Count from 1 to 10:",
#'   provider = "openai",
#'   stop     = c("6")
#' )
#' cat(res_stop)
#'
#' # 2e. Override API key for one call
#' res_override <- call_llm(
#'   prompt   = "Override test",
#'   provider = "openai",
#'   api_key  = "sk-override",
#'   max_tokens = 20
#' )
#' cat(res_override)
#'
#' # 2f. Factory interface for repeated prompts
#' GitHubLLM <- call_llm(
#'   provider   = "github",
#'   max_tokens = 60,
#'   verbose    = FALSE
#' )
#' # direct invocation
#' story1 <- GitHubLLM("Tell me a short story")
#' cat(story1)
#'
#' ## 3. Multi-message conversation
#'
#' # 3a. Simple system + user
#' convo1 <- list(
#'   list(role = "system",    content = "You are a helpful assistant."),
#'   list(role = "user",      content = "Explain recursion.")
#' )
#' res1 <- call_llm(
#'   messages   = convo1,
#'   provider   = "openai",
#'   max_tokens = 100
#' )
#' cat(res1)
#'
#' # 3b. Continue an existing chat by appending a prompt
#' prev <- list(
#'   list(role = "system", content = "You are concise."),
#'   list(role = "user",   content = "Summarize the plot of Hamlet.")
#' )
#' res2 <- call_llm(
#'   messages = prev,
#'   prompt   = "Now give me three bullet points."
#' )
#' cat(res2)
#'
#' # 3c. Use stop sequence in multi-message
#' convo2 <- list(
#'   list(role = "system", content = "You list items."),
#'   list(role = "user",   content = "Name three colors.")
#' )
#' res3 <- call_llm(
#'   messages = convo2,
#'   stop     = c(".")
#' )
#' cat(res3)
#'
#' # 3d. Multi-message via factory interface
#' ScopedLLM <- call_llm(provider = "openai", temperature = 0.3)
#' chat_ctx <- list(
#'   list(role = "system", content = "You are a math tutor.")
#' )
#' ans1 <- ScopedLLM(messages = chat_ctx, prompt = "Solve 2+2.")
#' cat(ans1)
#' ans2 <- ScopedLLM("What about 10*10?")
#' cat(ans2)
#' }
#'
#' @export
NULL

call_llm <- function(
    prompt        = NULL,
    messages      = NULL,
    provider      = c("openai","groq","anthropic",
                      "deepseek","dashscope","github",
                      "gemini","grok","azure_openai","azure_foundry","bedrock"),
    model         = NULL,
    temperature   = 0.7,
    max_tokens    = 1000,
    api_key       = NULL,
    n_tries       = 3,
    backoff       = 2,
    verbose       = TRUE,
    endpoint_url  = NULL,
    github_api_version     = "2022-11-28",
    anthropic_api_version  = "2023-06-01",
    azure_api_version = Sys.getenv("AZURE_OPENAI_API_VERSION", unset = "2024-02-15-preview"),
    azure_endpoint = Sys.getenv("AZURE_OPENAI_ENDPOINT"),
    azure_foundry_api_version = Sys.getenv("AZURE_FOUNDRY_API_VERSION", unset = "2024-05-01-preview"),
    azure_foundry_endpoint = Sys.getenv("AZURE_FOUNDRY_ENDPOINT"),
    azure_foundry_token = Sys.getenv("AZURE_FOUNDRY_TOKEN"),
    aws_region = Sys.getenv("AWS_REGION", unset = Sys.getenv("AWS_DEFAULT_REGION", unset = "us-east-1")),
    aws_access_key_id = Sys.getenv("AWS_ACCESS_KEY_ID"),
    aws_secret_access_key = Sys.getenv("AWS_SECRET_ACCESS_KEY"),
    aws_session_token = Sys.getenv("AWS_SESSION_TOKEN"),
    ...,
    .post_func    = httr::POST
) {

  ## ------------------------------------------------------------------------- ##
  ## Factory mode: if neither prompt nor messages supplied, return an LLM object
  if (missing(prompt) && missing(messages)) {
    return(
      function(prompt   = NULL,
               messages = NULL,
               ...) {
        # Re - invoke call_llm() with stored defaults + whatever the user
        # passes now
        args_main <- list(prompt = prompt, messages = messages)
        opts_main <- list(
          provider              = provider,
          model                 = model,
          temperature           = temperature,
          max_tokens            = max_tokens,
          api_key               = api_key,
          n_tries               = n_tries,
          backoff               = backoff,
          verbose               = verbose,
          endpoint_url          = endpoint_url,
          github_api_version    = github_api_version,
          anthropic_api_version = anthropic_api_version,
          azure_api_version     = azure_api_version,
          azure_endpoint        = azure_endpoint,
          azure_foundry_api_version = azure_foundry_api_version,
          azure_foundry_endpoint    = azure_foundry_endpoint,
          azure_foundry_token       = azure_foundry_token,
          aws_region            = aws_region,
          aws_access_key_id     = aws_access_key_id,
          aws_secret_access_key = aws_secret_access_key,
          aws_session_token     = aws_session_token,
          .post_func            = .post_func
        )
        extra_args <- list(...)
        all_args   <- c(args_main, opts_main, extra_args)
        do.call(call_llm, all_args)
      }
    )
  }

  ##
  ## ------------------------------------------------------------------------- ##

  provider <- match.arg(tolower(provider),
                        c("openai","groq","anthropic",
                          "deepseek","dashscope","github",
                          "gemini","grok","azure_openai","azure_foundry","bedrock"))
  if (is.null(model)) model <- get_default_model(provider)

  ## ---------------- assemble messages ----------------------------------- ##
  if (!is.null(messages)) {
    if (!is.null(prompt))
      messages <- c(messages, list(list(role = "user", content = prompt)))
  } else {
    if (is.null(prompt))
      stop("Provide either `prompt` or `messages`.")
    messages <- list(list(role = "user", content = prompt))
  }

  ## ---------------- common request pieces ------------------------------- ##
  req_body <- NULL
  req_headers <- NULL
  extra_args <- list(...)

  if (provider == "bedrock") {
    req_body <- build_bedrock_request_body(
      messages = messages,
      temperature = temperature,
      max_tokens = max_tokens,
      extra_args = extra_args
    )
  } else {
    api_key <- if (provider == "azure_foundry" &&
                     is.character(azure_foundry_token) &&
                     nzchar(azure_foundry_token)) {
      ""
    } else {
      get_api_key(provider, api_key)
    }

    req_body <- if (provider == "azure_openai") {
      c(
        list(
          messages    = messages,
          temperature = temperature,
          max_tokens  = max_tokens
        ),
        extra_args
      )
    } else {
      c(
        list(
          model       = model,
          messages    = messages,
          temperature = temperature,
          max_tokens  = max_tokens
        ),
        extra_args
      )
    }

    req_headers <- switch(
      provider,
      "openai" = add_headers(Authorization = paste("Bearer", api_key)),
      "groq"   = add_headers(Authorization = paste("Bearer", api_key),
                             "Content-Type" = "application/json"),
      "anthropic" = add_headers(`x-api-key` = api_key,
                                `anthropic-version` = anthropic_api_version,
                                "Content-Type" = "application/json"),
      "deepseek"  = add_headers(Authorization = paste("Bearer", api_key)),
      "dashscope" = add_headers(Authorization = paste("Bearer", api_key)),
      "azure_openai" = add_headers(`api-key` = api_key,
                                   "Content-Type" = "application/json"),
      "azure_foundry" = if (is.character(azure_foundry_token) &&
                              nzchar(azure_foundry_token)) {
        add_headers(
          Authorization = paste("Bearer", azure_foundry_token),
          "Content-Type" = "application/json"
        )
      } else {
        add_headers(
          `api-key` = api_key,
          "Content-Type" = "application/json"
        )
      },
      "github"    = add_headers(Accept = "application/vnd.github+json",
                                Authorization = paste("Bearer", api_key),
                                `X-GitHub-Api-Version` = github_api_version,
                                "Content-Type" = "application/json"),
      "gemini"    = add_headers(Authorization = paste("Bearer", api_key),
                                "Content-Type" = "application/json"),
      "grok"      = add_headers(Authorization = paste("Bearer", api_key),
                                "Content-Type" = "application/json")
    )
  }

  if (is.null(endpoint_url)) {
    endpoint_url <- switch(
      provider,
      "openai"    = "https://api.openai.com/v1/chat/completions",
      "groq"      = "https://api.groq.com/openai/v1/chat/completions",
      "anthropic" = "https://api.anthropic.com/v1/messages",
      "deepseek"  = "https://api.deepseek.com/v1/chat/completions",
      "dashscope" = "https://dashscope-intl.aliyuncs.com/compatible-mode/v1/chat/completions",
      "azure_openai" = {
        if (!nzchar(azure_endpoint)) {
          stop(
            "For provider='azure_openai', set AZURE_OPENAI_ENDPOINT or pass azure_endpoint.",
            call. = FALSE
          )
        }
        if (!nzchar(model)) {
          stop(
            "For provider='azure_openai', provide deployment name via `model` ",
            "or set AZURE_OPENAI_DEPLOYMENT.",
            call. = FALSE
          )
        }
        base <- sub("/+$", "", azure_endpoint)
        encoded_deployment <- utils::URLencode(as.character(model), reserved = TRUE)
        encoded_version <- utils::URLencode(as.character(azure_api_version), reserved = TRUE)
        sprintf(
          "%s/openai/deployments/%s/chat/completions?api-version=%s",
          base, encoded_deployment, encoded_version
        )
      },
      "azure_foundry" = {
        if (!nzchar(azure_foundry_endpoint)) {
          stop(
            "For provider='azure_foundry', set AZURE_FOUNDRY_ENDPOINT or pass azure_foundry_endpoint.",
            call. = FALSE
          )
        }
        base <- sub("/+$", "", azure_foundry_endpoint)
        encoded_version <- utils::URLencode(as.character(azure_foundry_api_version), reserved = TRUE)
        endpoint_base <- if (grepl("/chat/completions($|\\?)", base, ignore.case = TRUE)) {
          base
        } else if (grepl("/models($|/)", base, ignore.case = TRUE)) {
          paste0(base, "/chat/completions")
        } else {
          paste0(base, "/chat/completions")
        }
        if (grepl("api-version=", endpoint_base, ignore.case = TRUE)) {
          endpoint_base
        } else {
          paste0(
            endpoint_base,
            if (grepl("\\?", endpoint_base)) "&" else "?",
            "api-version=", encoded_version
          )
        }
      },
      "github"    = {
        org <- Sys.getenv("GH_MODELS_ORG")
        if (nzchar(org))
          sprintf("https://models.github.ai/orgs/%s/inference/chat/completions", org)
        else
          "https://models.github.ai/inference/chat/completions"
      },
      "gemini"    = "https://generativelanguage.googleapis.com/v1beta/openai/chat/completions",
      "grok"      = "https://api.x.ai/v1/chat/completions",
      "bedrock"   = {
        if (!nzchar(aws_region)) {
          stop(
            "For provider='bedrock', set AWS_REGION/AWS_DEFAULT_REGION or pass aws_region.",
            call. = FALSE
          )
        }
        encoded_model <- utils::URLencode(as.character(model), reserved = TRUE)
        sprintf(
          "https://bedrock-runtime.%s.amazonaws.com/model/%s/converse",
          aws_region, encoded_model
        )
      }
    )
  }

  if (verbose) {
    message(sprintf("Calling %s [%s] ... attempts=%d", provider, model, n_tries))
  }
  credential_hint <- if (provider == "bedrock") {
    "AWS credentials/region"
  } else if (provider == "azure_openai") {
    "AZURE_OPENAI_API_KEY (plus endpoint/deployment)"
  } else if (provider == "azure_foundry") {
    "AZURE_FOUNDRY_API_KEY or AZURE_FOUNDRY_TOKEN (plus endpoint)"
  } else {
    "*_API_KEY / MODELS_TOKEN"
  }

  ## ---------------- retry loop ------------------------------------------ ##
  res <- NULL
  for (i in seq_len(n_tries)) {

    req_headers <- if (provider == "bedrock") {
      body_json <- toJSON(req_body, auto_unbox = TRUE, null = "null")
      action <- paste0("/", sub("^https?://[^/]+/?", "", endpoint_url))
      signed <- build_aws_sigv4_headers(
        service = "bedrock-runtime",
        region = aws_region,
        verb = "POST",
        action = action,
        request_body = body_json,
        content_type = "application/json",
        accept = "application/json",
        aws_access_key_id = aws_access_key_id,
        aws_secret_access_key = aws_secret_access_key,
        aws_session_token = aws_session_token
      )
      do.call(add_headers, signed$headers)
    } else {
      req_headers
    }

    res <- tryCatch(
      .post_func(
        url    = endpoint_url,
        encode = "json",
        body   = req_body,
        req_headers
      ),
      error = function(e) {
        # detect network time-outs explicitly
        if (grepl("timeout", e$message, ignore.case = TRUE)) {
          msg <- sprintf("Attempt %d/%d failed (%s: timeout). Retrying in %ds...",
                         i, n_tries, provider, backoff)
        } else {
          msg <- sprintf("Attempt %d/%d failed (%s). Retrying in %ds...",
                         i, n_tries, provider, backoff)
        }
        message(msg)

        if (i == n_tries) {
          stop(sprintf(
            paste0(
              'The request to provider "%s" timed out after %d attempt(s).\n\n',
              'Tip: the model may be retired or misspelled.\n',
              'Run list_models("%s") (after setting the proper %s) to see ',
              'current models, e.g.\n',
              '    openai_models <- list_models("openai")\n',
              'Then rerun call_llm(..., model = "<new-model>").\n\n',
              'If the issue is network-related you can also:\n',
              ' . increase `n_tries` or `backoff`,\n',
              ' . provide a longer `timeout()` via `.post_func`, or\n',
              ' . check your network / VPN.\n\n',
              'Internal message: %s'
            ),
            provider, n_tries, provider, credential_hint, e$message
          ), call. = FALSE)
        }

        Sys.sleep(backoff)
        NULL
      }
    )

    if (is.null(res)) next
    if (!http_error(res)) break

    ## ---- HTTP error branch --------------------------------------------- ##
    err_txt    <- content(res, "text", encoding = "UTF-8")
    err_parsed <- tryCatch(fromJSON(err_txt), error = function(e) NULL)

    not_found <- FALSE
    if (!is.null(err_parsed$error$code))
      not_found <- grepl("model_not_found|invalid_model|404",
                         err_parsed$error$code, ignore.case = TRUE)
    if (!not_found && !is.null(err_parsed$message))
      not_found <- grepl("model.*not.*found|no such model|de.?commiss|deprecated",
                         err_parsed$message, ignore.case = TRUE)

    if (not_found) {
      stop(sprintf(
        paste0(
          'The model "%s" is unavailable or de-commissioned for provider "%s".\n',
          'Tip: run list_models("%s") after setting the proper %s to see ',
          'current models, e.g.\n',
          '    openai_models <- list_models("openai")\n',
          'Then rerun call_llm(..., model = "<new-model>").'
        ),
        model, provider, provider, credential_hint
      ), call. = FALSE)
    }

    ## --- generic HTTP error after final retry --------------------------- ##
    if (i == n_tries) {
      stop(sprintf(
        paste0(
          'Provider "%s" still returned an error after %d attempt(s).\n\n',
          'Raw response from server:\n%s\n\n',
          'Tip: the model may be retired, renamed, or misspelled.\n',
          '. Run list_models("%s") (after setting the proper %s) to view ',
          'currently available models, e.g.\n',
          '    openai_models <- list_models("openai")\n',
          '. Or visit the provider\'s dashboard / documentation for the ',
          'latest list.\n\n',
          'Then rerun call_llm(..., model = "<new-model>").'
        ),
        provider, n_tries, err_txt, provider, credential_hint
      ), call. = FALSE)
    }

    if (verbose) {
      message(sprintf("HTTP %d on attempt %d/%d. Retrying in %ds...",
                      status_code(res), i, n_tries, backoff))
    }

    Sys.sleep(backoff)
  }


  txt <- parse_response(provider, content(res, "parsed"))

  if (verbose) {
    message(sprintf("Response (truncated):\n%s",
                    substr(txt, 1, min(200, nchar(txt)))))
  }
  txt
}

###############################################################################
# End of block                                                                #
###############################################################################

