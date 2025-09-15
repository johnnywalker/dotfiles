{
  services.swaync.enable = true;
  services.swaync.settings = {
    positionX = "right";
    positionY = "top";
    layer = "overlay";
    control-center-layer = "top";
    layer-shell = true;
    control-center-margin-top = 10;
    control-center-margin-bottom = 0;
    control-center-margin-right = 10;
    control-center-margin-left = 0;
    fit-to-screen = false;
    notification-2fa-action = true;
    notification-inline-replies = false;
    notification-body-image-height = 100;
    notification-body-image-width = 200;
    control-center-width = 500;
    control-center-height = 600;
  };
  services.swaync.style = ''
    /* use nordic colors */
    @define-color noti-border-color rgba(255, 255, 255, 0.15);
    @define-color noti-bg #2E3440;
    @define-color noti-bg-alt #383E4A;
    @define-color noti-fg #E5E9F0;
    @define-color noti-bg-hover #81A1C1;
    @define-color noti-bg-focus #A3BE8C;
    @define-color noti-close-bg rgba(255, 255, 255, 0.1);
    @define-color noti-close-bg-hover rgba(255, 255, 255, 0.15);
    @define-color noti-urgent #BF616A;

    @define-color bg-selected rgb(0, 128, 255);

    *{
      color: @noti-fg;
    }

    .notification-row {
      outline: none;
    }

    .notification-row:focus,
    .notification-row:hover {
      background: @noti-bg-focus;
    }

    .notification {
      border-radius: 12px;
      margin: 6px 12px;
      box-shadow: 0 0 0 1px rgba(0, 0, 0, 0.3), 0 1px 3px 1px rgba(0, 0, 0, 0.7),
        0 2px 6px 2px rgba(0, 0, 0, 0.3);
      padding: 0;
    }

    .critical {
      background: @noti-urgent;
      padding: 2px;
      border-radius: 12px;
    }


    .notification-content {
      background: transparent;
      padding: 6px;
      border-radius: 12px;
    }

    .close-button {
      background: @noti-close-bg;
      color: white;
      text-shadow: none;
      padding: 0;
      border-radius: 100%;
      margin-top: 10px;
      margin-right: 16px;
      box-shadow: none;
      border: none;
      min-width: 24px;
      min-height: 24px;
    }

    .close-button:hover {
      box-shadow: none;
      background: @noti-close-bg-hover;
      transition: all 0.15s ease-in-out;
      border: none;
    }

    .notification-default-action,
    .notification-action {
      padding: 4px;
      margin: 0;
      box-shadow: none;
      background: @noti-bg;
      border: 1px solid @noti-border-color;
      color: white;
    }

    .notification-default-action:hover,
    .notification-action:hover {
      -gtk-icon-effect: none;
      background: @noti-bg-hover;
    }

    .notification-default-action {
      border-radius: 12px;
    }

    /* When alternative actions are visible */
    .notification-default-action:not(:only-child) {
      border-bottom-left-radius: 0px;
      border-bottom-right-radius: 0px;
    }

    .notification-action {
      border-radius: 0px;
      border-top: none;
      border-right: none;
    }

    /* add bottom border radius to eliminate clipping */
    .notification-action:first-child {
      border-bottom-left-radius: 10px;
    }

    .notification-action:last-child {
      border-bottom-right-radius: 10px;
      border-right: 1px solid @noti-border-color;
    }

    .image {}

    .body-image {
      margin-top: 6px;
      background-color: white;
      border-radius: 12px;
    }

    .summary {
      font-size: 16px;
      font-weight: bold;
      background: transparent;
      color: white;
      text-shadow: none;
    }

    .time {
      font-size: 16px;
      font-weight: bold;
      background: transparent;
      color: white;
      text-shadow: none;
      margin-right: 18px;
    }

    .body {
      font-size: 15px;
      font-weight: normal;
      background: transparent;
      color: white;
      text-shadow: none;
    }
  '';
}
