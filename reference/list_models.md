# List Available Models for Supported Providers

Retrieve the catalog of available model IDs for one or all supported
chat - completion providers. Useful for discovering active models and
avoiding typos or deprecated defaults.

Supported providers:

- `"openai"` - OpenAI Chat Completions API

- `"groq"` - Groq OpenAI - compatible endpoint

- `"anthropic"` - Anthropic Claude API

- `"deepseek"` - DeepSeek chat API

- `"dashscope"` - Alibaba DashScope compatible API

- `"github"` - GitHub Models OpenAI - compatible API

- `"azure_openai"` - Azure OpenAI deployments/models

- `"azure_foundry"` - Azure AI Foundry chat/models endpoints

- `"bedrock"` - AWS Bedrock (Converse API)

- `"all"` - Fetch catalogs for all of the above

## Arguments

- provider:

  Character. One of `"github"`, `"openai"`, `"groq"`, `"anthropic"`,
  `"deepseek"`, `"dashscope"`, `"azure_openai"`, `"azure_foundry"`,
  `"bedrock"` or `"all"`. Case - insensitive.

- ...:

  Additional arguments passed to the per - provider helper (e.g. `limit`
  for Anthropic, or `api_version` for GitHub).

- github_api_version:

  Character. Header value for `X - GitHub - Api - Version` (GitHub
  Models). Default `"2022 - 11 - 28"`.

- anthropic_api_version:

  Character. Header value for `anthropic - version` (Anthropic). Default
  `"2023 - 06 - 01"`.

- azure_api_version:

  Character. Query version for Azure OpenAI listing. Default
  `"2024-02-15-preview"`.

- azure_foundry_api_version:

  Character. Query version for Azure AI Foundry model listing. Default
  `"2024-05-01-preview"`.

## Value

If `provider != "all"`, a character vector of model IDs for that single
provider. If `provider == "all"`, a named list of character vectors, one
per provider.

## See also

[`call_llm`](https://knowusuboaky.github.io/chatLLM/reference/call_llm.md)

## Examples

``` r
if (FALSE) { # \dontrun{
Sys.setenv(OPENAI_API_KEY = "sk-...")
openai_models <- list_models("openai")
head(openai_models)

Sys.setenv(ANTHROPIC_API_KEY = "sk-...")
anthro_models <- list_models("anthropic", anthropic_api_version = "2023-06-01")

Sys.setenv(GH_MODELS_TOKEN = "ghp-...")
github_models <- list_models("github", github_api_version = "2022-11-28")
} # }
```
