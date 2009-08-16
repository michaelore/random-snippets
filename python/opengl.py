import pygame
from OpenGL.GL import *
from OpenGL.GLU import *

def initializeDisplay(w, h):
    pygame.display.set_mode((w,h), pygame.OPENGL|pygame.DOUBLEBUF)
    glClearColor(0.0, 0.0, 0.0, 1.0)
    glClear(GL_COLOR_BUFFER_BIT|GL_DEPTH_BUFFER_BIT)


def inputLoop():
    def display():
        pygame.display.flip()
    done = False
    while not done:
        for event in pygame.event.get():
            if event.type == QUIT \
                    or event.type == KEYDOWN and event.key == K_ESCAPE:
                        done = True
            elif event.type == K_LEFT:
                pass
            elif event.type == K_RIGHT:
                pass
        display()

def run():
    pygame.init()
    initializeDisplay(800, 600)
    inputLoop()

run()
