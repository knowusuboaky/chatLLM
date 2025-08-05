# chatLLM 0.1.3 (Upcoming Release – August 2025)

## New Features

* **Google Gemini Integration**
  `chat_llm()` now speaks to **Gemini** via Google’s **OpenAI-compatible Chat Completions API** (`…/v1beta/openai/chat/completions`). The default model is `gemini-2.0-flash`, and new helpers (`get_gemini_models()`, `"gemini"` option in `list_models()`) make catalog discovery a one-liner. ([Google AI for Developers][1], [Google Developers Blog][2])

* **xAI Grok Integration**
  Added first-class support for **Grok** through the endpoint `https://api.x.ai/v1/chat/completions`. The default model is `grok-3-latest`. You also get `get_grok_models()` plus a `"grok"` flag in `list_models()` for painless switching. ([xAI Docs][3], [docs.typingmind.com][4])

* **Model Catalog Expansion**
  `list_models("all")` now aggregates catalogs from **eight providers**—OpenAI, Groq, Anthropic, DeepSeek, DashScope, GitHub, **Gemini**, and **Grok**—so you can inspect every available model in a single call.

[1]: https://ai.google.dev/gemini-api/docs/openai?utm_source=chatgpt.com "OpenAI compatibility | Gemini API | Google AI for Developers"
[2]: https://developers.googleblog.com/en/gemini-is-now-accessible-from-the-openai-library/?utm_source=chatgpt.com "Gemini is now accessible from the OpenAI Library"
[3]: https://docs.x.ai/docs/api-reference?utm_source=chatgpt.com "REST API Reference"
[4]: https://docs.typingmind.com/manage-and-connect-ai-models/xai-%28grok-ai%29?utm_source=chatgpt.com "xAI (Grok AI)"


# chatLLM 0.1.2 (Upcoming Release – May 2025)

## New Features

- **DeepSeek Integration**  
  `chat_llm()` now supports **DeepSeek** as a backend provider. This expands the range of available language models and increases flexibility for users selecting different inference engines.

- **Alibaba DashScope Integration**  
  You can now use models from **Alibaba Cloud’s Model Studio (DashScope)** via OpenAI-compatible endpoints. This allows users in mainland China and beyond to easily integrate powerful **Qwen-series** models (like `qwen-plus`, `qwen-turbo`, and others) using the same `chat_llm()` interface.

- **GitHub Copilot-Compatible Model Integration**  
  You can now use models hosted through **GitHub Copilot-compatible endpoints**. This allows seamless integration with custom-hosted or proxy-accessible models, making it easier to experiment with private or specialized deployments.

- **Model Catalog Access**  
  `chat_llm()` now supports listing **all available models across all supported providers**. This makes it easier to discover and compare model options before selecting one for your workflow.
