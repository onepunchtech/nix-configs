{
  mobile = {
    outputs = [
      {
        criteria = "eDP-1";
        status = "enable";
      }
    ];
  };
  office1 = {
    outputs = [
      {
        criteria = "Ltd LG ULTRAGEAR 108MXBP68917";
        mode = "2560x1440";
        position = "0,0";
        scale = 1.0;
        status = "enable";
      }
      {
        criteria = "eDP-1";
        status = "disable";
      }

    ];
  };
  office2 = {
    outputs = [
      {
        criteria = "LG Electronics LG ULTRAGEAR 108MXBP68917";
        mode = "2560x1440";
        position = "3840,0";
        scale = 1.0;
        status = "enable";
      }
      {
        criteria = "LG Electronics LG Ultra HD 0x000074D3";
        mode = "3840x2160";
        position = "0,0";
        scale = 2.0;
        status = "enable";
      }

    ];
  };
  downstairs = {
    outputs = [
      {
        criteria = "Goldstar Company Ltd LG Ultra HD 0x000074D3";
        mode = "3840x2160";
        position = "0,0";
        scale = 2.0;
        status = "enable";
      }
      {
        criteria = "eDP-1";
        status = "disable";
      }

    ];
  };
}
