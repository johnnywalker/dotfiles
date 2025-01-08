{config, ...}: {
  sops.secrets."wireguard/bastion-a".sopsFile = ./secrets.yaml;
  networking.wg-quick.interfaces.wg0 = {
    address = ["172.29.1.4/24" "fd02:8016:6d16::4/64"];
    autostart = false;
    privateKeyFile = config.sops.secrets."wireguard/bastion-a".path;
    peers = [
      {
        publicKey = "lNb7SHicnoB1GWKRV5688beRMr6YF0cK9anRDXkhcyw=";
        allowedIPs = [
          "10.205.0.0/16"
          "10.206.0.0/20"
          "172.29.1.0/24"
          "2600:1f16:111a:db00::/56"
          # exclude bastion subnet 2600:1f16:1c0a:5ff8::/64 from 2600:1f16:1c0a:5f00::/56
          # ref: https://www.procustodibus.com/blog/2021/03/wireguard-allowedips-calculator/
          "2600:1f16:1c0a:5f00::/57"
          "2600:1f16:1c0a:5f80::/58"
          "2600:1f16:1c0a:5fc0::/59"
          "2600:1f16:1c0a:5fe0::/60"
          "2600:1f16:1c0a:5ff0::/61"
          "2600:1f16:1c0a:5ff9::/64"
          "2600:1f16:1c0a:5ffa::/63"
          "2600:1f16:1c0a:5ffc::/62"
        ];
        endpoint = "bastion-a-use2.dfh.dev:51820";
      }
    ];
  };
}
