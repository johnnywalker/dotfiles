{...}: {
  services = {
    openssh = {
      enable = true;
      settings = {
        PasswordAuthentication = false;
        PermitRootLogin = "no";
      };
      ports = [22];
      # disable rsa keys
      hostKeys = [
        {
          path = "/etc/ssh/ssh_host_ed25519_key";
          type = "ed25519";
        }
      ];
      # Automatically remove stale sockets
      extraConfig = ''
        StreamLocalBindUnlink yes
      '';
      # Allow forwarding ports to everywhere
      # gatewayPorts = "clientspecified";
    };
  };

  # Passwordless sudo when SSH'ing with keys
  security = {
    pam = {
      sshAgentAuth.enable = true;
    };
  };

  # NOTE: port not opened by default
  # networking = {
  #   firewall = {
  #     allowedTCPPorts = config.services.openssh.ports;
  #   };
  # };
}
