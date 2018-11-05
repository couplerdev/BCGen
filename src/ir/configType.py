#!/usr/bin/python
#coding:utf-8
# This module define some config type
# reversion history
# 2018/11/1 alex: create this

class TimeConfig:
    def __init__(self):
        self.timeConf = {}
        self.timeConf['calendar']=('NO_LEAP', 'string')
        self.timeConf['stop_option'] = ('never', 'string')
        self.timeConf['stop_ymd'] = ('99990101', 'int')
        self.timeConf['restart_option'] = ('never','int')
        self.timeConf['restart_ymd'] = ('-999', 'int')
        self.timeConf['history_option'] = ('never','string')
        self.timeConf['history_n'] = ('-999', 'int')
        self.timeConf['history_ymd'] = ('-999', 'int')
        self.timeConf['histavg_option'] = ('never', 'string')
        self.timeConf['histavg_n'] = ('-999', 'int')
        self.timeConf['histavg_ymd'] = ('-999', 'int')
        self.timeConf['start_ymd'] = ('00010101', 'int')
        self.timeConf['start_tod'] = ('0','int')
        self.timeConf['comps'] = {}
        self.timeConf['end_restart'] = ('.false.','bool')

    def addCompInfo(self, key, value):
        self.timeConf['comps'][key]=(value, 'int')
