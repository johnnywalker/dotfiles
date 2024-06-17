{...}: {
  networking.nameservers = [
    "45.90.28.0#m3800-bbf4c2.dns.nextdns.io"
    "2a07:a8c0::#m3800-bbf4c2.dns.nextdns.io"
    "45.90.30.0#m3800-bbf4c2.dns.nextdns.io"
    "2a07:a8c1::#m3800-bbf4c2.dns.nextdns.io"
  ];

  services.resolved = {
    enable = true;
    dnssec = "true";
    domains = ["~."];
    fallbackDns = ["1.1.1.1#one.one.one.one" "1.0.0.1#one.one.one.one"];
    dnsovertls = "true";
  };
}
