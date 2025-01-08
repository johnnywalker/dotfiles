{...}: {
  # swap alt and win/meta/super
  services.udev.extraHwdb = ''
    evdev:name:USB-HID Keyboard:*
      KEYBOARD_KEY_700e2=leftmeta
      KEYBOARD_KEY_700e3=leftalt
  '';
}
