controlbar <- dashboardControlbar(skin = "light", collapsed = TRUE, width = 250,
      controlbarMenu(
       id = "menu",
       controlbarItem(
        NULL,
          chooseSliderSkin("Flat", color = "#e00007"),
    bsTooltip(id='menu', 'Save file', placement = "bottom", trigger = "hover", options = NULL),
)))  # 138 pixel per cm
