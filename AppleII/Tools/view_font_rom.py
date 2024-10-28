#!/usr/bin/env python3
import pygame
from datetime import datetime
from pygame.locals import *

with open("./font.bin", 'rb') as file:
    byte_data = file.read()
    
char_rom = byte_data
current_char = 0

pygame.init()

screen_width, screen_height = 800, 600  # Define the screen dimensions
screen = pygame.display.set_mode((screen_width, screen_height))
pygame.display.set_caption('Character Display')

text_color = (0, 0, 0)             # Black for character pixels
background_color = (255, 255, 255) # White for background

texture = pygame.Surface((7, 8))

def update_char():
    global current_char
    current_char = (current_char + 1) % 64

def update_texture():
    global texture
    global current_char
    start = current_char * 7 * 8
    end = start + 7 * 8
    char_pattern = char_rom[start:end]
    for y in range(8):
        for x in range(7):
            color = text_color if char_pattern[x * 8 + y] == 1 else background_color
            texture.set_at((x, y), color)
    stretched_texture = pygame.transform.scale(texture, (screen_width, screen_height))
    return(stretched_texture)

running = True
frame_time = datetime.now()
while running:
    for event in pygame.event.get():
        if event.type == QUIT:
            running = False

    # Draw the stretched texture to the screen
    now = datetime.now()
    delta_time = (now - frame_time).total_seconds()
    if delta_time > 1:
        frame_time = now
        update_char()
    stretched_texture = update_texture()
    screen.blit(stretched_texture, (0, 0))
    pygame.display.flip()  # Update the display

pygame.quit()

