#Provide a sprite called sprite.png

import pygame, sys, math
from pygame.locals import *
screen = pygame.display.set_mode((1024, 768))
clock = pygame.time.Clock()

class DuckySprite(pygame.sprite.Sprite):
	def __init__(self, image, position):
		pygame.sprite.Sprite.__init__(self)
		self.src_image = pygame.image.load(image).convert()
		self.x_pos, self.y_pos = position
		self.rot = 0
		self.rot_vel = self.x_vel = self.y_vel = self.speed = 0
		self.rot_acc = self.lin_acc = 0
	
	def update(self, t):
		self.x_pos += self.x_vel * t
		self.y_pos += self.y_vel * t
		self.rot += self.rot_vel * t
		self.x_vel = math.sin(self.rot) * self.speed
		self.y_vel = math.cos(self.rot) * self.speed
		self.rot_vel += self.rot_acc * t
		self.speed += self.lin_acc * t
		self.image = pygame.transform.rotate(self.src_image, self.rot*180.0/math.pi)
		self.rect = self.image.get_rect()
		self.rect.center = self.x_pos, self.y_pos
	
	def up(self, bool):
		if bool: self.lin_acc = -100
		else: self.lin_acc = 0
	
	def down(self, bool):
		if bool: self.lin_acc = 100
		else: self.lin_acc = 0
	
	def right(self, bool):
		if bool: self.rot_acc = -10
		else: self.rot_acc = 0
	
	def left(self, bool):
		if bool: self.rot_acc = 10
		else: self.rot_acc = 0
		
rect = screen.get_rect()
ducky = DuckySprite('sprite.png', rect.center)
ducky_group = pygame.sprite.RenderPlain(ducky)
while 1:
	t = 1.0 / clock.tick(30)
	for event in pygame.event.get():
		if not hasattr(event, 'key'): continue
		down = event.type == KEYDOWN
		if event.key == K_RIGHT: ducky.right(down)
		elif event.key == K_LEFT: ducky.left(down)
		elif event.key == K_UP: ducky.up(down)
		elif event.key == K_DOWN: ducky.down(down)
		elif event.key == K_ESCAPE: sys.exit(0)
		
	
	screen.fill((255, 255, 255))
	ducky_group.update(t)
	ducky_group.draw(screen)
	pygame.display.flip()
