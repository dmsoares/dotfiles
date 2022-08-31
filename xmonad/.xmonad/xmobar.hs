Config { overrideRedirect = False
       , font     = "xft:Ubuntu:weight=bold:pixelsize=11:antialias=true:hinting=true"
       , bgColor  = "#5f5f5f"
       , fgColor  = "#f8f8f2"
       , position = TopW L 100
       , commands = [
                    -- Run Weather "EGPF"
                    --     [ "--template", "<weather> <tempC>°C"
                    --     , "-L", "0"
                    --     , "-H", "25"
                    --     , "--low"   , "lightblue"
                    --     , "--normal", "#f8f8f2"
                    --     , "--high"  , "red"
                    --     ] 36000
                    Run Cpu
                        [ "-L", "3"
                        , "-H", "50"
                        , "--high"  , "red"
                        , "--normal", "green"
                        ] 10
                    , Run DiskU [("/", "Disk: <used>/<size>")] [] 10
                    , Run Uptime ["-t", "Uptime: <days>d <hours>h"] 360
                    , Run Alsa "default" "Master"
                        [ "--template", "<volumestatus>"
                        , "--suffix"  , "True"
                        , "--"
                        , "--on", ""
                        ]
                    , Run Memory ["--template", "Mem: <usedratio>%"] 10
                    , Run Swap [] 10
                    , Run Date "%a %Y-%m-%d <fc=#8be9fd>%H:%M</fc>" "date" 10
                    , Run Com "/bin/sh" ["-c","~/.xmonad/spotify.sh"] "music" 1000
                    , Run Com "/bin/sh" ["-c","~/.xmonad/bluetooth.sh"] "bluetooth" 1000
                    , Run StdinReader
                    ]
       , sepChar  = "%"
       , alignSep = "}{"
       , template = "%StdinReader% } %music% { %alsa:default:Master% | %bluetooth% | %cpu% | %memory% • %swap% | %disku% | %uptime% | %date% "
       }