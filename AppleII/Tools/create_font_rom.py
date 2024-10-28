#!/usr/bin/env python3
from PIL import Image, ImageDraw, ImageFont

font_path = "./Font/PrintChar21.ttf"

def get_pattern(char):
    # Load the font at a size that will fit in a 7x8 grid
    font = ImageFont.truetype(font_path, 8)
    
    # Create a 7x8 image with a white background
    image = Image.new("1", (7, 8), color=1)  # "1" mode is black and white
    draw = ImageDraw.Draw(image)
    
    # Draw the character 'A' centered in the 7x8 grid
    draw.text((0, 0), char, font=font, fill=0)  # Fill text in black
    
    # Extract the pixel data as a 7x8 grid
    pixel_grid = [[image.getpixel((x, y)) for x in range(7)] for y in range(8)]
    if char != '_':
        pixel_grid = [pixel_grid[y] for y in [7, 0, 1, 2, 3, 4, 5, 6]]
    
    return pixel_grid



chars =  ['@', 'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z', '[', '\\', ']', '^', '_', ' ', '!', '"', '#', '$', '%', '&', '\'', '(', ')', '*', '=', ',', '-', '.', '/', '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', ':', ';', '<', '=', '>', '?']
current_char = 0

with open("font.bin", "wb") as f:
    for char in chars:
        char_pattern = get_pattern(char)
        int_data = [char_pattern[y][x] for x in range(7) for y in range(8)]
        byte_data = bytes (int_data)
        f.write(byte_data)


# import pygame
# from datetime import datetime
# from pygame.locals import *
# pygame.init()
#
# screen_width, screen_height = 800, 600  # Define the screen dimensions
# screen = pygame.display.set_mode((screen_width, screen_height))
# pygame.display.set_caption('Stretched Character Display')
#
# text_color = (0, 0, 0)             # Black for character pixels
# background_color = (255, 255, 255) # White for background
#
#
# texture = pygame.Surface((7, 8))
#
# def update_char():
#     global current_char
#     current_char = (current_char + 1) % len(chars)
#
# def update_texture():
#     global texture
#     char = chars[current_char]
#     char_pattern = get_pattern(char)
#     for y in range(8):
#         for x in range(7):
#             color = text_color if char_pattern[y][x] == 1 else background_color
#             texture.set_at((x, y), color)
#     stretched_texture = pygame.transform.scale(texture, (screen_width, screen_height))
#     return(stretched_texture)
#
# running = True
# frame_time = datetime.now()
# while running:
#     for event in pygame.event.get():
#         if event.type == QUIT:
#             running = False
#
#     # Draw the stretched texture to the screen
#     now = datetime.now()
#     delta_time = (now - frame_time).total_seconds()
#     if delta_time > 1:
#         frame_time = now
#         update_char()
#     stretched_texture = update_texture()
#     screen.blit(stretched_texture, (0, 0))
#     pygame.display.flip()  # Update the display
#
# pygame.quit()

