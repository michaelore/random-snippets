# Make sure body.png and wheel.png are in the working directory.

import pygame, sys
from math import *
import numpy as np
from pygame.locals import *
screen = pygame.display.set_mode((1024, 768))
clock = pygame.time.Clock()

def midpoint(point1, point2):
	return ((point1[0] + point2[0])/2, (point1[1] + point2[1])/2)

def rotateArray(phi, array):
	mat = np.array(((cos(phi), -sin(phi)), (sin(phi), cos(phi))))
	return np.dot(mat, array.transpose()).transpose()

class SimpleSprite(pygame.sprite.Sprite):
	def __init__(self, image):
		pygame.sprite.Sprite.__init__(self)
		self.src_image = pygame.image.load(image).convert()
		self.src_image.set_colorkey((255, 255, 255))
	def update(self, position, rot):
		self.image = pygame.transform.rotate(self.src_image, rot*180.0/pi)
		self.rect = self.image.get_rect()
		self.rect.center = position[0], position[1]

class RoboSprite:
	def __init__(self, position):
		self.body = pygame.sprite.Group(SimpleSprite('body.png'))
		rect = self.body.sprites()[0].src_image.get_rect()
		self.wheel_positions = np.array((midpoint(rect.topleft, rect.midleft),
						 midpoint(rect.bottomleft, rect.midleft),
						 midpoint(rect.topright, rect.midright),
						 midpoint(rect.bottomright, rect.midright)))
		self.wheels = pygame.sprite.Group(*[SimpleSprite('wheel.png') for n in (1,2,3,4)])
		self.update(position, 0, 0)
	def update(self, pos, rot, wheel_rot):
		self.body.update(pos, rot)
		rect = self.body.sprites()[0].rect
		if not hasattr(self, 'upper_left'):
			self.upper_left = np.array(rect.topleft) - np.array(pos)
		new_wheel_positions = (rotateArray(-rot, self.wheel_positions))
		for n in (0,1,2,3):
			wheel = self.wheels.sprites()[n]
			wheel.update(new_wheel_positions[n] + np.array(pos) + rotateArray(-rot, self.upper_left), wheel_rot)
		self.body.draw(screen)
		self.wheels.draw(screen)

class Robot:
	def __init__(self, position):
		self.x_pos, self.y_pos = position
		self.rot = self.wheel_rot = 0
		self.rot_vel = self.x_vel = self.y_vel = self.wheel_rot_vel = 0
		self.rot_acc = self.x_acc = self.y_acc = 0
		self.sprite = RoboSprite(position)
	def update(self, t):
		self.x_pos += self.x_vel * t
		self.y_pos += self.y_vel * t
		self.rot += self.rot_vel * t
		self.wheel_rot += self.wheel_rot_vel
		self.x_vel = self.x_acc * t
		self.y_vel = self.y_acc * t
		self.rot_vel += self.rot_acc * t
		self.sprite.update((self.x_pos, self.y_pos), self.rot, self.wheel_rot)
	def up(self, bool):
		if bool: self.y_acc = x_acc = -100
		else: self.y_acc = x_acc = 0
	def down(self, bool):
		if bool: self.y_acc = x_acc = 100
		else: self.y_acc = x_acc = 0
	def right(self, bool):
		if bool: self.rot_acc = -10
		else: self.rot_acc = 0
	def left(self, bool):
		if bool: self.rot_acc = 10
		else: self.rot_acc = 0

rect = screen.get_rect()
robot = Robot(rect.center)
while 1:
	t = 1.0 / clock.tick(30)
	for event in pygame.event.get():
		if not hasattr(event, 'key'): continue
		down = event.type == KEYDOWN
		if event.key == K_RIGHT: robot.right(down)
		elif event.key == K_LEFT: robot.left(down)
		elif event.key == K_UP: robot.up(down)
		elif event.key == K_DOWN: robot.down(down)
		elif event.key == K_ESCAPE: sys.exit(0)
	screen.fill((255, 255, 255))
	robot.update(t)
	pygame.display.flip()
