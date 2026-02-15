# tests/testthat/test_chatLLM.R

context("Testing call_llm() functionality")

test_that("call_llm() returns a factory function when no input is given", {
  factory_fn <- call_llm(provider = "openai")
  expect_type(factory_fn, "closure")  # should return a function
})

test_that("call_llm() returns expected response using a fake POST", {
  # Set a dummy API key so get_api_key doesn't fail
  Sys.setenv(OPENAI_API_KEY = "dummy_key")

  # Fake POST function that mimics a real httr::response
  fake_post_func <- function(url, encode, body, req_headers, ...) {
    structure(
      list(
        status_code = 200L,
        url         = url,
        headers     = c("Content-Type" = "application/json"),
        all_headers = list(list(
          status  = 200L,
          version = "HTTP/1.1",
          headers = c("Content-Type" = "application/json")
        )),
        content     = charToRaw('{"choices": [{"message": {"content": "Hello from fake response"}}]}'),
        date        = Sys.time()
      ),
      class = "response"
    )
  }

  result <- call_llm(
    prompt     = "Test",
    provider   = "openai",
    n_tries    = 1,
    .post_func = fake_post_func
  )

  expect_equal(result, "Hello from fake response")
})

test_that("call_llm() supports provider='bedrock' with signed request path", {
  skip_if_not_installed("aws.signature")

  fake_bedrock_post <- function(url, encode, body, req_headers, ...) {
    expect_true(grepl("/converse$", url))
    expect_equal(encode, "json")
    expect_true(is.list(body))
    expect_true(!is.null(body$messages))
    expect_true(!is.null(body$inferenceConfig))

    structure(
      list(
        status_code = 200L,
        url         = url,
        headers     = c("Content-Type" = "application/json"),
        all_headers = list(list(
          status  = 200L,
          version = "HTTP/1.1",
          headers = c("Content-Type" = "application/json")
        )),
        content     = charToRaw('{"output":{"message":{"role":"assistant","content":[{"text":"Hello from Bedrock"}]}}}'),
        date        = Sys.time()
      ),
      class = "response"
    )
  }

  result <- call_llm(
    prompt = "Test bedrock",
    provider = "bedrock",
    model = "amazon.nova-lite-v1:0",
    n_tries = 1,
    verbose = FALSE,
    aws_region = "us-east-1",
    aws_access_key_id = "AKIDEXAMPLE",
    aws_secret_access_key = "SECRETEXAMPLE",
    .post_func = fake_bedrock_post
  )

  expect_equal(result, "Hello from Bedrock")
})

test_that("call_llm() supports provider='azure_openai' deployment endpoint", {
  Sys.setenv(AZURE_OPENAI_API_KEY = "dummy_azure_key")

  fake_azure_post <- function(url, encode, body, req_headers, ...) {
    expect_true(grepl(
      "https://my-resource\\.openai\\.azure\\.com/openai/deployments/my-deployment/chat/completions\\?api-version=2024-02-15-preview$",
      url
    ))
    expect_equal(encode, "json")
    expect_true(is.null(body$model))
    expect_true(!is.null(body$messages))

    structure(
      list(
        status_code = 200L,
        url         = url,
        headers     = c("Content-Type" = "application/json"),
        all_headers = list(list(
          status  = 200L,
          version = "HTTP/1.1",
          headers = c("Content-Type" = "application/json")
        )),
        content     = charToRaw('{"choices":[{"message":{"content":"Hello from Azure OpenAI"}}]}'),
        date        = Sys.time()
      ),
      class = "response"
    )
  }

  result <- call_llm(
    prompt = "Test azure",
    provider = "azure_openai",
    model = "my-deployment",
    azure_endpoint = "https://my-resource.openai.azure.com/",
    azure_api_version = "2024-02-15-preview",
    n_tries = 1,
    .post_func = fake_azure_post
  )

  expect_equal(result, "Hello from Azure OpenAI")
})

test_that("list_models('azure_openai') falls back to AZURE_OPENAI_DEPLOYMENT", {
  Sys.setenv(AZURE_OPENAI_DEPLOYMENT = "dep_from_env")
  Sys.unsetenv("AZURE_OPENAI_ENDPOINT")
  Sys.unsetenv("AZURE_OPENAI_API_KEY")

  models <- list_models("azure_openai")
  expect_true("dep_from_env" %in% models)
})

test_that("call_llm() supports provider='azure_foundry' endpoint + api-key auth", {
  Sys.setenv(AZURE_FOUNDRY_API_KEY = "dummy_foundry_key")

  fake_foundry_post <- function(url, encode, body, req_headers, ...) {
    expect_true(grepl(
      "https://my-foundry\\.models\\.ai\\.azure\\.com/chat/completions\\?api-version=2024-05-01-preview$",
      url
    ))
    expect_equal(encode, "json")
    expect_equal(body$model, "my-foundry-model")
    expect_true(!is.null(body$messages))

    structure(
      list(
        status_code = 200L,
        url         = url,
        headers     = c("Content-Type" = "application/json"),
        all_headers = list(list(
          status  = 200L,
          version = "HTTP/1.1",
          headers = c("Content-Type" = "application/json")
        )),
        content     = charToRaw('{"choices":[{"message":{"content":"Hello from Foundry"}}]}'),
        date        = Sys.time()
      ),
      class = "response"
    )
  }

  result <- call_llm(
    prompt = "Test foundry",
    provider = "azure_foundry",
    model = "my-foundry-model",
    azure_foundry_endpoint = "https://my-foundry.models.ai.azure.com",
    azure_foundry_api_version = "2024-05-01-preview",
    n_tries = 1,
    .post_func = fake_foundry_post
  )

  expect_equal(result, "Hello from Foundry")
})

test_that("call_llm() supports provider='azure_foundry' bearer token auth", {
  Sys.unsetenv("AZURE_FOUNDRY_API_KEY")

  fake_foundry_post <- function(url, encode, body, req_headers, ...) {
    structure(
      list(
        status_code = 200L,
        url         = url,
        headers     = c("Content-Type" = "application/json"),
        all_headers = list(list(
          status  = 200L,
          version = "HTTP/1.1",
          headers = c("Content-Type" = "application/json")
        )),
        content     = charToRaw('{"choices":[{"message":{"content":"Hello from Foundry Bearer"}}]}'),
        date        = Sys.time()
      ),
      class = "response"
    )
  }

  result <- call_llm(
    prompt = "Test foundry bearer",
    provider = "azure_foundry",
    model = "my-foundry-model",
    azure_foundry_endpoint = "https://my-foundry.models.ai.azure.com/models",
    azure_foundry_token = "dummy-bearer-token",
    n_tries = 1,
    .post_func = fake_foundry_post
  )

  expect_equal(result, "Hello from Foundry Bearer")
})

test_that("list_models('azure_foundry') falls back to AZURE_FOUNDRY_MODEL", {
  Sys.setenv(AZURE_FOUNDRY_MODEL = "foundry_model_env")
  Sys.unsetenv("AZURE_FOUNDRY_ENDPOINT")
  Sys.unsetenv("AZURE_FOUNDRY_API_KEY")
  Sys.unsetenv("AZURE_FOUNDRY_TOKEN")

  models <- list_models("azure_foundry")
  expect_true("foundry_model_env" %in% models)
})
