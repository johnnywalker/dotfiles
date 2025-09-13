{pkgs, ...}: {
  home.packages = with pkgs; [
    (aspellWithDicts (dicts: with dicts; [en en-computers en-science]))
    basedpyright # language server for python
    copilot-language-server
    # nil # language server for nix
    nixd # language server for nix
    ruff # formatter, language server for python
    rust-analyzer
    tailwindcss-language-server
    terraform-ls # language server for terraform
    vscode-langservers-extracted
    vtsls
    yaml-language-server
  ];
}
