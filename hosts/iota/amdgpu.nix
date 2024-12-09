{ pkgs, ... }:
{
  boot.initrd.kernelModules = [ "amdgpu" ];
  boot.kernelParams = [ "radeon.cik_support=0" "amdgpu.cik_support=1" ];

  hardware.opengl.extraPackages = with pkgs; [ amdvlk ];

  services.xserver.videoDrivers = [ "amdgpu" ];
}
