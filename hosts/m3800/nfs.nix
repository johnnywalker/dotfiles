{...}: {
  fileSystems."/mnt/drive" = {
    device = "192.168.86.11:/mnt/primary/drive";
    fsType = "nfs";
    options = ["nfsvers=4.2" "x-systemd.automount" "noauto"];
  };
}

