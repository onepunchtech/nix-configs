let gruvbox-dark =
      { primaryColors = { background = "#282828", foreground = "#fbf1c7" }
      , normalColors =
        { black = "#282828"
        , red = "#cc241d"
        , green = "#98971a"
        , yellow = "#d79921"
        , blue = "#458588"
        , magenta = "#b16286"
        , cyan = "#689d6a"
        , white = "#a89984"
        }
      , brightColors =
        { black = "#928374"
        , red = "#fb4934"
        , green = "#b8bb26"
        , yellow = "#fabd2f"
        , blue = "#83a598"
        , magenta = "#d3869b"
        , cyan = "#8ec07c"
        , white = "#ebdbb2"
        }
      , editorColors =
        { dark0Soft = "#32302f"
        , dark1 = "#3c3836"
        , dark2 = "#504945"
        , dark3 = "#665c54"
        , dark4 = "#7c6f64"
        , gray = "#928374"
        , light0Hard = "#ffffc8"
        , light0 = "#fdf4c1"
        , light1 = "#ebdbb2"
        , light2 = "#d5c4a1"
        , light3 = "#bdae93"
        , light4 = "#a89984"
        , brightOrange = "#fe8019"
        , normalOrange = "#d65d0e"
        , darkRed = "#421E1E"
        , darkBlue = "#2B3C44"
        , darkAqua = "#36473A"
        , sienna = "#DD6F48"
        , lightblue4 = "#66999D"
        , burlywood4 = "#BBAA97"
        , turquoise4 = "#61ACBB"
        , currentDiffA = "#4f2121"
        , currentDiffB = "#243c24"
        , currentDiffC = "#4f214f"
        , currentDiffAncestor = "#21214f"
        , fineDiffA = "#761919"
        , fineDiffB = "#1c691c"
        , fineDiffC = "#761976"
        , fineDiffAncestor = "#12129d"
        }
      }

let gruvbox-light =
      { primaryColors = { background = "#fbf1c7", foreground = "#3c3836" }
      , normalColors =
        { black = "#fbf1c1"
        , red = "#cc241d"
        , green = "#98971a"
        , yellow = "#d79921"
        , blue = "#458588"
        , magenta = "#b16286"
        , cyan = "#689d6a"
        , white = "#7c6f64"
        }
      , brightColors =
        { black = "#928374"
        , red = "#9d0006"
        , green = "#79740e"
        , yellow = "#b57614"
        , blue = "#076678"
        , magenta = "#8f3f71"
        , cyan = "#427b58"
        , white = "#3c3836"
        }
      , editorColors =
        { dark0Soft = "#f2e5bc"
        , dark1 = "#ebdbb2"
        , dark2 = "#d5c4a1"
        , dark3 = "#bdae93"
        , dark4 = "#a89984"
        , gray = "#928374"
        , light0Hard = "#1d2021"
        , light0 = "#282828"
        , light1 = "#3c3836"
        , light2 = "#504945"
        , light3 = "#665c54"
        , light4 = "#7c6f64"
        , brightOrange = "#af3a03"
        , normalOrange = "#d65d0e"
        , darkRed = "#421E1E"
        , darkBlue = "#2B3C44"
        , darkAqua = "#36473A"
        , sienna = "#DD6F48"
        , lightblue4 = "#66999D"
        , burlywood4 = "#BBAA97"
        , turquoise4 = "#61ACBB"
        , currentDiffA = "#fbc6a3"
        , currentDiffB = "#e3f3b5"
        , currentDiffC = "#fadccc"
        , currentDiffAncestor = "#ccc6d1"
        , fineDiffA = "#fbb091"
        , fineDiffB = "#b6f691"
        , fineDiffC = "#fbb0d6"
        , fineDiffAncestor = "#b6b0d6"
        }
      }

in  { themes = toMap { gruvbox-dark, gruvbox-light } }
