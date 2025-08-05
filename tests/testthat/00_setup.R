# tests/testthat/00_setup.R

# You can set environment variables to mock or hold real API keys:
Sys.setenv(OPENAI_API_KEY    = "FAKE_OPENAI_KEY_FOR_TESTS")
Sys.setenv(GROQ_API_KEY      = "FAKE_GROQ_KEY_FOR_TESTS")
Sys.setenv(ANTHROPIC_API_KEY = "FAKE_ANTHROPIC_KEY_FOR_TESTS")
Sys.setenv(GH_MODELS_TOKEN = "FAKE_GH_MODELS_TOKEN_FOR_TESTS")
Sys.setenv(DEEPSEEK_API_KEY = "FAKE_DEEPSEEK_API_KEY_FOR_TESTS")
Sys.setenv(DASHSCOPE_API_KEY = "FAKE_DASHSCOPE_API_KEY_FOR_TESTS")
Sys.setenv(GEMINI_API_KEY = "FAKE_GEMINI_API_KEY_FOR_TESTS")
Sys.setenv(XAI_API_KEY = "FAKE_XAI_API_KEY_FOR_TESTS")

# If you have additional setup steps for testthat, place them here.
# This file is automatically sourced before test_*.R files.
