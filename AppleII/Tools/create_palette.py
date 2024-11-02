#!/usr/bin/env python3

colors = [  0,   0,   0   #  COL_BLACK
         , 221,   0,  5   #  COL_MAGENTA
         ,   0,   0, 15   #  COL_DARK_BLUE
         , 221,  34, 22   #  COL_PURPLE
         ,   0, 119,  3   #  COL_GREEN
         ,  85,  85,  8   #  COL_GREY_1
         ,  34,  34, 25   #  COL_MEDIUM_BLUE
         , 102, 170, 25   #  COL_LIGHT_BLUE
         , 136,  85,      #  COL_BROWN
         , 255, 102,      #  COL_ORANGE
         , 170, 170, 17   #  COL_GREY_2
         , 255, 153, 13   #  COL_PINK
         ,  17, 221,      #  COL_LIGHT_GREEN
         , 255, 255,      #  COL_YELLOW
         ,  68, 255, 15   #  COL_AQUAMARINE
         , 255, 255, 25]  #  COL_WHITE         




with open("palette.bin", "wb") as f:
    byte_data = bytes (colors)
    f.write(byte_data)

