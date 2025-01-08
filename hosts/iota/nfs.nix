{...}: {
  fileSystems."/mnt/music" = {
    device = "192.168.86.11:/mnt/primary/drive/music";
    fsType = "nfs";
    options = ["nfsvers=4.2" "x-systemd.automount" "noauto"];
  };
}
