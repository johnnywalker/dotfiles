{pkgs, ...}: let
  models = pkgs.linkFarm "llm-models" {
    "DeepSeek-R1-Distill-Qwen-32B-Q4_K_M.gguf" = pkgs.fetchurl {
      name = "DeepSeek-R1-Distill-Qwen-32B-Q4_K_M.gguf";
      url = "https://huggingface.co/bartowski/DeepSeek-R1-Distill-Qwen-32B-GGUF/resolve/main/DeepSeek-R1-Distill-Qwen-32B-Q4_K_M.gguf?download=true";
      sha256 = "0qpwp704mxbmsz5w8695nzb7mhw71y7s922qvbwmpfgma7sv1ndy";
    };
  };
in {
  environment.systemPackages = with pkgs; [
    rocmPackages.rocminfo
    llama-cpp
  ];
  environment.variables = {
    # doesn't seem to work
    HSA_OVERRIDE_GFX_VERSION = "11.0.0";
  };
  services.llama-cpp.enable = true;
  services.llama-cpp.model = "${models}/DeepSeek-R1-Distill-Qwen-32B-Q4_K_M.gguf";
  services.llama-cpp.extraFlags = [
    "--temp"
    "0.6" # recommended for R1
  ];
  services.llama-cpp.port = 9001;

  # open-webui doesn't seem necessary, and the stop action doesn't seem to work
  # services.open-webui.enable = true;
  # services.open-webui.environment = {
  #   ANONYMIZED_TELEMETRY = "False";
  #   DO_NOT_TRACK = "True";
  #   SCARF_NO_ANALYTICS = "True";
  #   WEBUI_AUTH = "False";
  #   OPENAI_API_BASE_URL = "http://${config.services.llama-cpp.host}:${builtins.toString config.services.llama-cpp.port}";
  # };
  # services.open-webui.port = 9002;
}
