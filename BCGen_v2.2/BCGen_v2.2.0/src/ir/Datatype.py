#!/usr/bin python
#utf-8

class Base():
    __slots__ = ["y", "m", "d", "h"]
    def __init__(self, y, m, d, h):
	self.y = y 
	self.m = m
	self.d = d
    	self.h = h

class Interval():
    __slots__ = ["m", "d", "h", "minute", "sec"]
    def __init__(self, m, d, h, minute, sec):
	self.m = m
	self.d = d
	self.h = h
        self.minute = minute
        self.sec = sec

class Time():
    __slots__ = ["base", "interval"]
    def __init__(self, base, interval):
        self.base = base
        self.interval = interval

class Domain():
    __slots__ = ["field", "path"]
    def __init__(self, field, path):
	self.field = field  
        self.path  = path
