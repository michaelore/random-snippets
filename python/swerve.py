# Make sure body.png and wheel.png are in the working directory.

# Magic numbers!
# Warning: may be arbitrary.
min_speed = 2
max_speed = 100
friction_multiplier = 50 # Not to be confused with coefficient of friction

import threading
import pygame, sys
from math import *
import numpy as np
from pygame.locals import *
screen = pygame.display.set_mode((1024, 768))
clock = pygame.time.Clock()
rect = screen.get_rect()

def signum(num):
	if(num < 0): return -1
	elif(num > 0): return 1
	else: return num

def midpoint(point1, point2):
	return ((point1[0] + point2[0])/2, (point1[1] + point2[1])/2)

def rotate_array(phi, array):
	mat = np.array(((cos(phi), sin(phi)), (-sin(phi), cos(phi))))
	return np.dot(array, mat)

def limit(n, low, high):
	if n < low:
		return low
	elif n > high:
		return high
	else: return n

def periodic_limit(n, low, high):
	return low + n % (high - low)

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
		new_wheel_positions = (rotate_array(-rot, self.wheel_positions))
		for n in (0,1,2,3):
			wheel = self.wheels.sprites()[n]
			wheel.update(new_wheel_positions[n] + np.array(pos) + rotate_array(-rot, self.upper_left), wheel_rot + rot)
		self.body.draw(screen)
		self.wheels.draw(screen)

class Robot:
	def __init__(self):
		self.x_pos, self.y_pos = rect.center
		self.rot = self.wheel_rot = 0
		self.rot_vel = self.x_vel = self.y_vel = self.wheel_rot_vel = 0
		self.rot_acc = self.x_acc = self.y_acc = 0
		self.left_motor = self.right_motor = self.swerve_motor = 0
		self.sprite = RoboSprite(rect.center)
	def update(self, t):
		self.program(K_s, 1, t)
		self.x_pos = limit(self.x_pos + self.x_vel * t, 0, 1024)
		self.y_pos = limit(self.y_pos - self.y_vel * t, 0, 768)
		self.rot = periodic_limit(self.rot + self.rot_vel *t, 0, 2*pi)
		self.wheel_rot = limit(self.wheel_rot + self.wheel_rot_vel * t, -pi/2, pi/2)
		self.wheel_rot_vel = self.swerve_motor
		self.x_vel = limit(self.x_vel + self.x_acc * t, -max_speed, max_speed)
		self.y_vel = limit(self.y_vel + self.y_acc * t, -max_speed, max_speed)
		if abs(self.x_vel) < min_speed: self.x_vel = 0
		if abs(self.y_vel) < min_speed: self.y_vel = 0
		self.rot_vel += self.rot_acc * t
		self.rot_acc = self.x_acc = self.y_acc = 0
		self.case_one()
		self.case_two()
		self.case_three()
		self.case_four()
		self.case_five()
		self.case_six()
		self.case_seven()
		self.case_eight()
		self.sprite.update((self.x_pos, self.y_pos), self.rot, self.wheel_rot)
	def case_one(self):
		self.rot_acc += -cos(self.wheel_rot)*self.left_motor
		self.x_acc += cos(self.wheel_rot)*sin(-self.rot)*self.left_motor/2*100
		self.y_acc += cos(self.wheel_rot)*cos(-self.rot)*self.left_motor/2*100
	def case_two(self):
		self.rot_acc += cos(self.wheel_rot)*self.right_motor
		self.x_acc += cos(self.wheel_rot)*sin(-self.rot)*self.right_motor/2*100
		self.y_acc += cos(self.wheel_rot)*cos(-self.rot)*self.right_motor/2*100
	def case_three(self):
		self.x_acc += -cos(self.wheel_rot)*sin(self.rot)*signum(self.x_vel)*friction_multiplier
		self.y_acc += -cos(self.wheel_rot)*cos(self.rot)*signum(self.y_vel)*friction_multiplier
	def case_four(self):
		self.x_acc += -cos(self.wheel_rot)*cos(self.rot)*signum(self.x_vel)*friction_multiplier*2
		self.y_acc += -cos(self.wheel_rot)*sin(self.rot)*signum(self.y_vel)*friction_multiplier*2
	def case_five(self):
		if abs(self.left_motor) >= abs(self.right_motor):
			self.x_acc += -sin(self.wheel_rot)*cos(self.rot)*self.right_motor*100
			self.y_acc += -sin(self.wheel_rot)*sin(self.rot)*self.right_motor*100
		else:
			self.x_acc += -sin(self.wheel_rot)*cos(self.rot)*self.left_motor*100
			self.y_acc += -sin(self.wheel_rot)*sin(self.rot)*self.left_motor*100
	def case_six(self):
		self.x_acc += -sin(self.wheel_rot)*sin(self.rot)*signum(self.x_vel)*friction_multiplier*2
		self.y_acc += -sin(self.wheel_rot)*cos(self.rot)*signum(self.y_vel)*friction_multiplier*2
	def case_seven(self):
		self.x_acc += -sin(self.wheel_rot)*cos(self.rot)*signum(self.x_vel)*friction_multiplier
		self.y_acc += -sin(self.wheel_rot)*sin(self.rot)*signum(self.y_vel)*friction_multiplier
	def case_eight(self):
		self.rot_acc += -self.rot_vel*10
	def program(self, key, is_down, t):
		if key == K_7:
			if is_down:
				self.left_motor = 1
			else: self.left_motor = 0
		elif key == K_j:
			if is_down:
				self.left_motor = -1
			else: self.left_motor = 0
		elif key == K_9:
			if is_down:
				self.right_motor = 1
			else: self.right_motor = 0
		elif key == K_l:
			if is_down:
				self.right_motor = -1
			else: self.right_motor = 0
		elif key == K_8:
			if is_down:
				self.right_motor = robot.left_motor = 1
			else: self.right_motor = robot.left_motor = 0
		elif key == K_k:
			if is_down:
				self.right_motor = robot.left_motor = -1
			else: self.right_motor = robot.left_motor = 0
		elif key == K_u:
			if is_down:
				self.swerve_motor = 1
			else: self.swerve_motor = 0
		elif key == K_o:
			if is_down:
				self.swerve_motor = -1
			else: self.swerve_motor = 0
		elif key == K_s:
			pass
		else: pass

robot = Robot()

class MyThread(threading.Thread):
	def run(self):
		while 1:
			t = 0.03
			for event in pygame.event.get():
				if not hasattr(event, 'key'): continue
				is_down = event.type == KEYDOWN
				if event.key == K_ESCAPE: sys.exit(0)
				else: robot.program(event.key, is_down, t)
			screen.fill((255, 255, 255))
			robot.update(t)
			pygame.display.flip()

MyThread().start()
