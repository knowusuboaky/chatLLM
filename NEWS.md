# chatLLM 0.1.4 (Upcoming Release - February 2026)

## New Features

* **AWS Bedrock Integration**
  Added first-class support for `provider = "bedrock"` using AWS SigV4 request signing and the Bedrock Converse API. New AWS parameters in `call_llm()` include `aws_region`, `aws_access_key_id`, `aws_secret_access_key`, and `aws_session_token`.

* **Azure OpenAI Integration**
  Added `provider = "azure_openai"` with deployment-based endpoint routing and `api-key` authentication. New Azure OpenAI parameters in `call_llm()` include `azure_endpoint` and `azure_api_version`.

* **Azure AI Foundry Integration**
  Added `provider = "azure_foundry"` with support for either `api-key` or bearer token authentication. New Foundry parameters in `call_llm()` include `azure_foundry_endpoint`, `azure_foundry_api_version`, and `azure_foundry_token`.

* **Model Catalog Expansion**
  `list_models()` now supports both Azure providers and Bedrock. `list_models("all")` includes the expanded provider set.

## Improvements

* **Provider-specific guidance**
  Error messages now provide more precise credential hints for AWS Bedrock, Azure OpenAI, and Azure AI Foundry.

* **Tests and documentation**
  Added test coverage for Bedrock, Azure OpenAI, and Azure AI Foundry provider paths, and updated generated Rd documentation accordingly.

# chatLLM 0.1.3 (Upcoming Release - August 2025)

## New Features

* **Google Gemini Integration**
  `chat_llm()` now speaks to **Gemini** via Googleâ€™s **OpenAI-compatible Chat Completions API** (`â€¦/v1beta/openai/chat/completions`). The default model is `gemini-2.0-flash`, and new helpers (`get_gemini_models()`, `"gemini"` option in `list_models()`) make catalog discovery a one-liner. ([Google AI for Developers][1], [Google Developers Blog][2])

* **xAI Grok Integration**
  Added first-class support for **Grok** through the endpoint `https://api.x.ai/v1/chat/completions`. The default model is `grok-3-latest`. You also get `get_grok_models()` plus a `"grok"` flag in `list_models()` for painless switching. ([xAI Docs][3], [docs.typingmind.com][4])

* **Model Catalog Expansion**
  `list_models("all")` now aggregates catalogs from **eight providers**â€”OpenAI, Groq, Anthropic, DeepSeek, DashScope, GitHub, **Gemini**, and **Grok**â€”so you can inspect every available model in a single call.

[1]: https://ai.google.dev/gemini-api/docs/openai "OpenAI compatibility | Gemini API | Google AI for Developers"
[2]: https://developers.googleblog.com/en/gemini-is-now-accessible-from-the-openai-library/ "Gemini is now accessible from the OpenAI Library"
[3]: https://docs.x.ai/docs/api-reference "REST API Reference"
[4]: https://docs.typingmind.com/manage-and-connect-ai-models/xai-%28grok-ai%29 "xAI (Grok AI)"


# chatLLM 0.1.2 (Upcoming Release â€“ May 2025)

## New Features

- **DeepSeek Integration**  
  `chat_llm()` now supports **DeepSeek** as a backend provider. This expands the range of available language models and increases flexibility for users selecting different inference engines.

- **Alibaba DashScope Integration**  
  You can now use models from **Alibaba Cloudâ€™s Model Studio (DashScope)** via OpenAI-compatible endpoints. This allows users in mainland China and beyond to easily integrate powerful **Qwen-series** models (like `qwen-plus`, `qwen-turbo`, and others) using the same `chat_llm()` interface.

- **GitHub Copilot-Compatible Model Integration**  
  You can now use models hosted through **GitHub Copilot-compatible endpoints**. This allows seamless integration with custom-hosted or proxy-accessible models, making it easier to experiment with private or specialized deployments.

- **Model Catalog Access**  
  `chat_llm()` now supports listing **all available models across all supported providers**. This makes it easier to discover and compare model options before selecting one for your workflow.

