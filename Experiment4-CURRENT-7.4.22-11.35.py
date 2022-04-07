#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""
This experiment was created using PsychoPy3 Experiment Builder (v2021.1.2),
    on aprile 07, 2022, at 11:00
If you publish work using this script the most relevant publication is:

    Peirce J, Gray JR, Simpson S, MacAskill M, Höchenberger R, Sogo H, Kastman E, Lindeløv JK. (2019) 
        PsychoPy2: Experiments in behavior made easy Behav Res 51: 195. 
        https://doi.org/10.3758/s13428-018-01193-y

"""

from __future__ import absolute_import, division

from psychopy import locale_setup
from psychopy import prefs
from psychopy import sound, gui, visual, core, data, event, logging, clock, colors, parallel, microphone
from psychopy.constants import (NOT_STARTED, STARTED, PLAYING, PAUSED,
                                STOPPED, FINISHED, PRESSED, RELEASED, FOREVER)

import numpy as np  # whole numpy lib is available, prepend 'np.'
from numpy import (sin, cos, tan, log, log10, pi, average,
                   sqrt, std, deg2rad, rad2deg, linspace, asarray)
from numpy.random import random, randint, normal, shuffle, choice as randchoice
import os  # handy system and path functions
import sys  # to get file system encoding

from psychopy.hardware import keyboard



# Ensure that relative paths start from the same directory as this script
_thisDir = os.path.dirname(os.path.abspath(__file__))
os.chdir(_thisDir)

# Store info about the experiment session
psychopyVersion = '2021.1.2'
expName = 'Experiment4'  # from the Builder filename that created this script
expInfo = {'participant': '', 'group': ['exp', 'con']}
dlg = gui.DlgFromDict(dictionary=expInfo, sortKeys=False, title=expName)
if dlg.OK == False:
    core.quit()  # user pressed cancel
expInfo['date'] = data.getDateStr()  # add a simple timestamp
expInfo['expName'] = expName
expInfo['psychopyVersion'] = psychopyVersion

# Data file name stem = absolute path + name; later add .psyexp, .csv, .log, etc
filename = _thisDir + os.sep + u'data/%s_%s_%s' % (expInfo['participant'], expName, expInfo['date'])

# An ExperimentHandler isn't essential but helps with data saving
thisExp = data.ExperimentHandler(name=expName, version='',
    extraInfo=expInfo, runtimeInfo=None,
    originPath='D:\\Dati Utente\\ESPVicon\\Desktop\\Tom\\Experiment4.py',
    savePickle=True, saveWideText=True,
    dataFileName=filename)
# save a log file for detail verbose info
logFile = logging.LogFile(filename+'.log', level=logging.EXP)
logging.console.setLevel(logging.WARNING)  # this outputs to the screen, not a file

endExpNow = False  # flag for 'escape' or other condition => quit the exp
frameTolerance = 0.001  # how close to onset before 'same' frame

# Start Code - component code to be run after the window creation
wavDirName = filename + '_wav'
if not os.path.isdir(wavDirName):
    os.makedirs(wavDirName)  # to hold .wav files
wavDirName = filename + '_wav'
if not os.path.isdir(wavDirName):
    os.makedirs(wavDirName)  # to hold .wav files
wavDirName = filename + '_wav'
if not os.path.isdir(wavDirName):
    os.makedirs(wavDirName)  # to hold .wav files
wavDirName = filename + '_wav'
if not os.path.isdir(wavDirName):
    os.makedirs(wavDirName)  # to hold .wav files
wavDirName = filename + '_wav'
if not os.path.isdir(wavDirName):
    os.makedirs(wavDirName)  # to hold .wav files
wavDirName = filename + '_wav'
if not os.path.isdir(wavDirName):
    os.makedirs(wavDirName)  # to hold .wav files
wavDirName = filename + '_wav'
if not os.path.isdir(wavDirName):
    os.makedirs(wavDirName)  # to hold .wav files
wavDirName = filename + '_wav'
if not os.path.isdir(wavDirName):
    os.makedirs(wavDirName)  # to hold .wav files

# Setup the Window
win = visual.Window(
    size=[1920, 1080], fullscr=True, screen=0, 
    winType='pyglet', allowGUI=False, allowStencil=False,
    monitor='monitor1', color=[0,0,0], colorSpace='rgb',
    blendMode='avg', useFBO=True, 
    units='height')
win1 = visual.Window(
    size=[1920, 1080], fullscr=True, screen=1, 
    winType='pyglet', allowGUI=False, allowStencil=False,
    monitor='testMonitor', color=[0,0,0], colorSpace='rgb',
    blendMode='avg', useFBO=True, 
    units='height')
win2 = visual.Window(
    size=[1920, 1080], fullscr=True, screen=2, 
    winType='pyglet', allowGUI=False, allowStencil=False,
    monitor='monitor2', color=[0,0,0], colorSpace='rgb',
    blendMode='avg', useFBO=True, 
    units='height')

# Enable sound input/output:
microphone.switchOn()
# store frame rate of monitor if we can measure it
expInfo['frameRate'] = win.getActualFrameRate()
if expInfo['frameRate'] != None:
    frameDur = 1.0 / round(expInfo['frameRate'])
else:
    frameDur = 1.0 / 60.0  # could not measure, so guess

# create a default keyboard (e.g. to check for escape)
defaultKeyboard = keyboard.Keyboard()

# Initialize components for Routine "Start"
StartClock = core.Clock()
key_resp = keyboard.Keyboard()

# Initialize components for Routine "BeforeQList1"
BeforeQList1Clock = core.Clock()
key_resp_2 = keyboard.Keyboard()
text = visual.TextStim(win=win, name='text',
    text='Preparati per discutere sulle prime domande.\n\nAvrai 1 minuto per parlarne.',
    font='Open Sans',
    pos=(0, 0), height=0.1, wrapWidth=None, ori=0.0, 
    color='white', colorSpace='rgb', opacity=None, 
    languageStyle='LTR',
    depth=-1.0);

# Initialize components for Routine "QList1"
QList1Clock = core.Clock()
key_resp_3 = keyboard.Keyboard()
text_2 = visual.TextStim(win=win, name='text_2',
    text='',
    font='Open Sans',
    pos=(0, 0), height=0.05, wrapWidth=None, ori=0.0, 
    color='white', colorSpace='rgb', opacity=None, 
    languageStyle='LTR',
    depth=-1.0);
List1_port = parallel.ParallelPort(address='0xE050')

# Initialize components for Routine "BeforeQList2"
BeforeQList2Clock = core.Clock()
key_resp_4 = keyboard.Keyboard()
text_3 = visual.TextStim(win=win, name='text_3',
    text='Preparati per discutere sulle prossime domande.\n\nAvrai 3 minuti per parlarne.',
    font='Open Sans',
    pos=(0, 0), height=0.1, wrapWidth=None, ori=0.0, 
    color='white', colorSpace='rgb', opacity=None, 
    languageStyle='LTR',
    depth=-1.0);

# Initialize components for Routine "QList2"
QList2Clock = core.Clock()
key_resp_5 = keyboard.Keyboard()
text_4 = visual.TextStim(win=win, name='text_4',
    text='',
    font='Open Sans',
    pos=(0, 0), height=0.039, wrapWidth=None, ori=0.0, 
    color='white', colorSpace='rgb', opacity=None, 
    languageStyle='LTR',
    depth=-1.0);
List2_port = parallel.ParallelPort(address='0xE050')

# Initialize components for Routine "BeforeQList3"
BeforeQList3Clock = core.Clock()
key_resp_6 = keyboard.Keyboard()
text_5 = visual.TextStim(win=win, name='text_5',
    text='Preparati per discutere sulle ultime domande.\n\nAvrai 5 minuti per parlarne.',
    font='Open Sans',
    pos=(0, 0), height=0.1, wrapWidth=None, ori=0.0, 
    color='white', colorSpace='rgb', opacity=None, 
    languageStyle='LTR',
    depth=-1.0);

# Initialize components for Routine "QList3"
QList3Clock = core.Clock()
key_resp_7 = keyboard.Keyboard()
text_6 = visual.TextStim(win=win, name='text_6',
    text='',
    font='Open Sans',
    pos=(0, 0), height=0.043, wrapWidth=None, ori=0.0, 
    color='white', colorSpace='rgb', opacity=None, 
    languageStyle='LTR',
    depth=-1.0);
List3_port = parallel.ParallelPort(address='0xE050')

# Initialize components for Routine "BeforeDiapix"
BeforeDiapixClock = core.Clock()
key_resp_8 = keyboard.Keyboard()

# Initialize components for Routine "Diapix"
DiapixClock = core.Clock()
key_resp_9 = keyboard.Keyboard()
imageA = visual.ImageStim(
    win=win,
    name='imageA', 
    image='sin', mask=None,
    ori=0.0, pos=(0, 0), size=(1.75, 1),
    color=[1,1,1], colorSpace='rgb', opacity=None,
    flipHoriz=False, flipVert=False,
    texRes=128.0, interpolate=True, depth=-1.0)
imageB = visual.ImageStim(
    win=win1,
    name='imageB', 
    image='sin', mask=None,
    ori=0.0, pos=(0, 0), size=(1.75, 1),
    color=[1,1,1], colorSpace='rgb', opacity=None,
    flipHoriz=False, flipVert=False,
    texRes=128.0, interpolate=True, depth=-2.0)
Diapix_port = parallel.ParallelPort(address='0xE050')

# Initialize components for Routine "End"
EndClock = core.Clock()
text_7 = visual.TextStim(win=win, name='text_7',
    text='Grazie per la tua partecipazione!',
    font='Open Sans',
    pos=(0, 0), height=0.1, wrapWidth=None, ori=0.0, 
    color='white', colorSpace='rgb', opacity=None, 
    languageStyle='LTR',
    depth=0.0);
key_resp_10 = keyboard.Keyboard()

# Create some handy timers
globalClock = core.Clock()  # to track the time since experiment started
routineTimer = core.CountdownTimer()  # to track time remaining of each (non-slip) routine 

# ------Prepare to start Routine "Start"-------
continueRoutine = True
# update component parameters for each repeat
key_resp.keys = []
key_resp.rt = []
_key_resp_allKeys = []
# keep track of which components have finished
StartComponents = [key_resp]
for thisComponent in StartComponents:
    thisComponent.tStart = None
    thisComponent.tStop = None
    thisComponent.tStartRefresh = None
    thisComponent.tStopRefresh = None
    if hasattr(thisComponent, 'status'):
        thisComponent.status = NOT_STARTED
# reset timers
t = 0
_timeToFirstFrame = win.getFutureFlipTime(clock="now")
StartClock.reset(-_timeToFirstFrame)  # t0 is time of first possible flip
frameN = -1

# -------Run Routine "Start"-------
while continueRoutine:
    # get current time
    t = StartClock.getTime()
    tThisFlip = win.getFutureFlipTime(clock=StartClock)
    tThisFlipGlobal = win.getFutureFlipTime(clock=None)
    frameN = frameN + 1  # number of completed frames (so 0 is the first frame)
    # update/draw components on each frame
    
    # *key_resp* updates
    waitOnFlip = False
    if key_resp.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
        # keep track of start time/frame for later
        key_resp.frameNStart = frameN  # exact frame index
        key_resp.tStart = t  # local t and not account for scr refresh
        key_resp.tStartRefresh = tThisFlipGlobal  # on global time
        win.timeOnFlip(key_resp, 'tStartRefresh')  # time at next scr refresh
        key_resp.status = STARTED
        # keyboard checking is just starting
        waitOnFlip = True
        win.callOnFlip(key_resp.clock.reset)  # t=0 on next screen flip
        win.callOnFlip(key_resp.clearEvents, eventType='keyboard')  # clear events on next screen flip
    if key_resp.status == STARTED and not waitOnFlip:
        theseKeys = key_resp.getKeys(keyList=['y', 'n', 'left', 'right', 'space', 'return'], waitRelease=False)
        _key_resp_allKeys.extend(theseKeys)
        if len(_key_resp_allKeys):
            key_resp.keys = _key_resp_allKeys[-1].name  # just the last key pressed
            key_resp.rt = _key_resp_allKeys[-1].rt
            # a response ends the routine
            continueRoutine = False
    
    # check for quit (typically the Esc key)
    if endExpNow or defaultKeyboard.getKeys(keyList=["escape"]):
        core.quit()
    
    # check if all components have finished
    if not continueRoutine:  # a component has requested a forced-end of Routine
        break
    continueRoutine = False  # will revert to True if at least one component still running
    for thisComponent in StartComponents:
        if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
            continueRoutine = True
            break  # at least one component has not yet finished
    
    # refresh the screen
    if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
        win.flip()
        win1.flip()
        win2.flip()

# -------Ending Routine "Start"-------
for thisComponent in StartComponents:
    if hasattr(thisComponent, "setAutoDraw"):
        thisComponent.setAutoDraw(False)
# check responses
if key_resp.keys in ['', [], None]:  # No response was made
    key_resp.keys = None
thisExp.addData('key_resp.keys',key_resp.keys)
if key_resp.keys != None:  # we had a response
    thisExp.addData('key_resp.rt', key_resp.rt)
thisExp.addData('key_resp.started', key_resp.tStartRefresh)
thisExp.addData('key_resp.stopped', key_resp.tStopRefresh)
thisExp.nextEntry()
# the Routine "Start" was not non-slip safe, so reset the non-slip timer
routineTimer.reset()

# ------Prepare to start Routine "BeforeQList1"-------
continueRoutine = True
routineTimer.add(13.000000)
# update component parameters for each repeat
key_resp_2.keys = []
key_resp_2.rt = []
_key_resp_2_allKeys = []
# keep track of which components have finished
BeforeQList1Components = [key_resp_2, text]
for thisComponent in BeforeQList1Components:
    thisComponent.tStart = None
    thisComponent.tStop = None
    thisComponent.tStartRefresh = None
    thisComponent.tStopRefresh = None
    if hasattr(thisComponent, 'status'):
        thisComponent.status = NOT_STARTED
# reset timers
t = 0
_timeToFirstFrame = win.getFutureFlipTime(clock="now")
BeforeQList1Clock.reset(-_timeToFirstFrame)  # t0 is time of first possible flip
frameN = -1

# -------Run Routine "BeforeQList1"-------
while continueRoutine and routineTimer.getTime() > 0:
    # get current time
    t = BeforeQList1Clock.getTime()
    tThisFlip = win.getFutureFlipTime(clock=BeforeQList1Clock)
    tThisFlipGlobal = win.getFutureFlipTime(clock=None)
    frameN = frameN + 1  # number of completed frames (so 0 is the first frame)
    # update/draw components on each frame
    
    # *key_resp_2* updates
    waitOnFlip = False
    if key_resp_2.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
        # keep track of start time/frame for later
        key_resp_2.frameNStart = frameN  # exact frame index
        key_resp_2.tStart = t  # local t and not account for scr refresh
        key_resp_2.tStartRefresh = tThisFlipGlobal  # on global time
        win.timeOnFlip(key_resp_2, 'tStartRefresh')  # time at next scr refresh
        key_resp_2.status = STARTED
        # keyboard checking is just starting
        waitOnFlip = True
        win.callOnFlip(key_resp_2.clock.reset)  # t=0 on next screen flip
        win.callOnFlip(key_resp_2.clearEvents, eventType='keyboard')  # clear events on next screen flip
    if key_resp_2.status == STARTED:
        # is it time to stop? (based on global clock, using actual start)
        if tThisFlipGlobal > key_resp_2.tStartRefresh + 10.0-frameTolerance:
            # keep track of stop time/frame for later
            key_resp_2.tStop = t  # not accounting for scr refresh
            key_resp_2.frameNStop = frameN  # exact frame index
            win.timeOnFlip(key_resp_2, 'tStopRefresh')  # time at next scr refresh
            key_resp_2.status = FINISHED
    if key_resp_2.status == STARTED and not waitOnFlip:
        theseKeys = key_resp_2.getKeys(keyList=['y', 'n', 'left', 'right', 'space', 'return'], waitRelease=False)
        _key_resp_2_allKeys.extend(theseKeys)
        if len(_key_resp_2_allKeys):
            key_resp_2.keys = _key_resp_2_allKeys[-1].name  # just the last key pressed
            key_resp_2.rt = _key_resp_2_allKeys[-1].rt
            # a response ends the routine
            continueRoutine = False
    
    # *text* updates
    if text.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
        text.draw(win)
        text.draw(win1)
        text.draw(win2)
        # keep track of start time/frame for later
        text.frameNStart = frameN  # exact frame index
        text.tStart = t  # local t and not account for scr refresh
        text.tStartRefresh = tThisFlipGlobal  # on global time
        win.timeOnFlip(text, 'tStartRefresh')  # time at next scr refresh
#        text.setAutoDraw(True)
    if text.status == STARTED:
        # is it time to stop? (based on global clock, using actual start)
        if tThisFlipGlobal > text.tStartRefresh + 13.0-frameTolerance:
            # keep track of stop time/frame for later
            text.tStop = t  # not accounting for scr refresh
            text.frameNStop = frameN  # exact frame index
            win.timeOnFlip(text, 'tStopRefresh')  # time at next scr refresh
            text.setAutoDraw(False)
    
    # check for quit (typically the Esc key)
    if endExpNow or defaultKeyboard.getKeys(keyList=["escape"]):
        core.quit()
    
    # check if all components have finished
    if not continueRoutine:  # a component has requested a forced-end of Routine
        break
    continueRoutine = False  # will revert to True if at least one component still running
    for thisComponent in BeforeQList1Components:
        if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
            continueRoutine = True
            break  # at least one component has not yet finished
    
    # refresh the screen
    if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
        win.flip()
        win1.flip()
        win2.flip()

# -------Ending Routine "BeforeQList1"-------
for thisComponent in BeforeQList1Components:
    if hasattr(thisComponent, "setAutoDraw"):
        thisComponent.setAutoDraw(False)
# check responses
if key_resp_2.keys in ['', [], None]:  # No response was made
    key_resp_2.keys = None
thisExp.addData('key_resp_2.keys',key_resp_2.keys)
if key_resp_2.keys != None:  # we had a response
    thisExp.addData('key_resp_2.rt', key_resp_2.rt)
thisExp.addData('key_resp_2.started', key_resp_2.tStartRefresh)
thisExp.addData('key_resp_2.stopped', key_resp_2.tStopRefresh)
thisExp.nextEntry()
thisExp.addData('text.started', text.tStartRefresh)
thisExp.addData('text.stopped', text.tStopRefresh)

# set up handler to look after randomisation of conditions etc
trials = data.TrialHandler(nReps=1.0, method='random', 
    extraInfo=expInfo, originPath=-1,
    trialList=data.importConditions("lists"+expInfo['group']+".csv"),
    seed=None, name='trials')
thisExp.addLoop(trials)  # add the loop to the experiment
thisTrial = trials.trialList[0]  # so we can initialise stimuli with some values
# abbreviate parameter names if possible (e.g. rgb = thisTrial.rgb)
if thisTrial != None:
    for paramName in thisTrial:
        exec('{} = thisTrial[paramName]'.format(paramName))

for thisTrial in trials:
    currentLoop = trials
    # abbreviate parameter names if possible (e.g. rgb = thisTrial.rgb)
    if thisTrial != None:
        for paramName in thisTrial:
            exec('{} = thisTrial[paramName]'.format(paramName))
    
    # ------Prepare to start Routine "QList1"-------
    continueRoutine = True
    routineTimer.add(64.000000)
    # update component parameters for each repeat
    key_resp_3.keys = []
    key_resp_3.rt = []
    _key_resp_3_allKeys = []
    text_2.setText(questions1)
    List1A_mic = microphone.AdvAudioCapture(name='List1A_mic', saveDir=wavDirName, stereo=False, chnl=0)
    List1B_mic = microphone.AdvAudioCapture(name='List1B_mic', saveDir=wavDirName, stereo=False, chnl=1)
    # keep track of which components have finished
    QList1Components = [key_resp_3, text_2, List1_port, List1A_mic, List1B_mic]
    for thisComponent in QList1Components:
        thisComponent.tStart = None
        thisComponent.tStop = None
        thisComponent.tStartRefresh = None
        thisComponent.tStopRefresh = None
        if hasattr(thisComponent, 'status'):
            thisComponent.status = NOT_STARTED
    # reset timers
    t = 0
    _timeToFirstFrame = win.getFutureFlipTime(clock="now")
    QList1Clock.reset(-_timeToFirstFrame)  # t0 is time of first possible flip
    frameN = -1
    
    # -------Run Routine "QList1"-------
    while continueRoutine and routineTimer.getTime() > 0:
        # get current time
        t = QList1Clock.getTime()
        tThisFlip = win.getFutureFlipTime(clock=QList1Clock)
        tThisFlipGlobal = win.getFutureFlipTime(clock=None)
        frameN = frameN + 1  # number of completed frames (so 0 is the first frame)
        # update/draw components on each frame
        
        # *key_resp_3* updates
        waitOnFlip = False
        if key_resp_3.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            key_resp_3.frameNStart = frameN  # exact frame index
            key_resp_3.tStart = t  # local t and not account for scr refresh
            key_resp_3.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(key_resp_3, 'tStartRefresh')  # time at next scr refresh
            key_resp_3.status = STARTED
            # keyboard checking is just starting
            waitOnFlip = True
            win.callOnFlip(key_resp_3.clock.reset)  # t=0 on next screen flip
            win.callOnFlip(key_resp_3.clearEvents, eventType='keyboard')  # clear events on next screen flip
        if key_resp_3.status == STARTED:
            # is it time to stop? (based on global clock, using actual start)
            if tThisFlipGlobal > key_resp_3.tStartRefresh + 15.0-frameTolerance:
                # keep track of stop time/frame for later
                key_resp_3.tStop = t  # not accounting for scr refresh
                key_resp_3.frameNStop = frameN  # exact frame index
                win.timeOnFlip(key_resp_3, 'tStopRefresh')  # time at next scr refresh
                key_resp_3.status = FINISHED
        if key_resp_3.status == STARTED and not waitOnFlip:
            theseKeys = key_resp_3.getKeys(keyList=['y', 'n', 'left', 'right', 'space', 'return'], waitRelease=False)
            _key_resp_3_allKeys.extend(theseKeys)
            if len(_key_resp_3_allKeys):
                key_resp_3.keys = _key_resp_3_allKeys[-1].name  # just the last key pressed
                key_resp_3.rt = _key_resp_3_allKeys[-1].rt
                # a response ends the routine
                continueRoutine = False
        
        # *text_2* updates
        if text_2.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            text_2.draw(win)
            text_2.draw(win1)
            text_2.draw(win2)
            # keep track of start time/frame for later
            text_2.frameNStart = frameN  # exact frame index
            text_2.tStart = t  # local t and not account for scr refresh
            text_2.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(text_2, 'tStartRefresh')  # time at next scr refresh
#            text_2.setAutoDraw(True)
        if text_2.status == STARTED:
            # is it time to stop? (based on global clock, using actual start)
            if tThisFlipGlobal > text_2.tStartRefresh + 64.0-frameTolerance:
                # keep track of stop time/frame for later
                text_2.tStop = t  # not accounting for scr refresh
                text_2.frameNStop = frameN  # exact frame index
                win.timeOnFlip(text_2, 'tStopRefresh')  # time at next scr refresh
                text_2.setAutoDraw(False)
        # *List1_port* updates
        if List1_port.status == NOT_STARTED and t >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            List1_port.frameNStart = frameN  # exact frame index
            List1_port.tStart = t  # local t and not account for scr refresh
            List1_port.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(List1_port, 'tStartRefresh')  # time at next scr refresh
            List1_port.status = STARTED
            win.callOnFlip(List1_port.setData, int(1))
        if List1_port.status == STARTED:
            # is it time to stop? (based on global clock, using actual start)
            if tThisFlipGlobal > List1_port.tStartRefresh + 64.0-frameTolerance:
                # keep track of stop time/frame for later
                List1_port.tStop = t  # not accounting for scr refresh
                List1_port.frameNStop = frameN  # exact frame index
                win.timeOnFlip(List1_port, 'tStopRefresh')  # time at next scr refresh
                List1_port.status = FINISHED
                win.callOnFlip(List1_port.setData, int(0))
        
        # *List1A_mic* updates
        if List1A_mic.status == NOT_STARTED and t >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            List1A_mic.frameNStart = frameN  # exact frame index
            List1A_mic.tStart = t  # local t and not account for scr refresh
            List1A_mic.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(List1A_mic, 'tStartRefresh')  # time at next scr refresh
            List1A_mic.status = STARTED
            List1A_mic.record(sec=64.0, block=False)  # start the recording thread
        
        if List1A_mic.status == STARTED and not List1A_mic.recorder.running:
            List1A_mic.status = FINISHED
        
        # *List1B_mic* updates
        if List1B_mic.status == NOT_STARTED and t >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            List1B_mic.frameNStart = frameN  # exact frame index
            List1B_mic.tStart = t  # local t and not account for scr refresh
            List1B_mic.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(List1B_mic, 'tStartRefresh')  # time at next scr refresh
            List1B_mic.status = STARTED
            List1B_mic.record(sec=64.0, block=False)  # start the recording thread
        
        if List1B_mic.status == STARTED and not List1B_mic.recorder.running:
            List1B_mic.status = FINISHED
        
        # check for quit (typically the Esc key)
        if endExpNow or defaultKeyboard.getKeys(keyList=["escape"]):
            core.quit()
        
        # check if all components have finished
        if not continueRoutine:  # a component has requested a forced-end of Routine
            break
        continueRoutine = False  # will revert to True if at least one component still running
        for thisComponent in QList1Components:
            if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
                continueRoutine = True
                break  # at least one component has not yet finished
        
        # refresh the screen
        if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
            win.flip()
            win1.flip()
            win2.flip()
    
    # -------Ending Routine "QList1"-------
    for thisComponent in QList1Components:
        if hasattr(thisComponent, "setAutoDraw"):
            thisComponent.setAutoDraw(False)
    # check responses
    if key_resp_3.keys in ['', [], None]:  # No response was made
        key_resp_3.keys = None
    trials.addData('key_resp_3.keys',key_resp_3.keys)
    if key_resp_3.keys != None:  # we had a response
        trials.addData('key_resp_3.rt', key_resp_3.rt)
    trials.addData('key_resp_3.started', key_resp_3.tStartRefresh)
    trials.addData('key_resp_3.stopped', key_resp_3.tStopRefresh)
    trials.addData('text_2.started', text_2.tStartRefresh)
    trials.addData('text_2.stopped', text_2.tStopRefresh)
    if List1_port.status == STARTED:
        win.callOnFlip(List1_port.setData, int(0))
    trials.addData('List1_port.started', List1_port.tStart)
    trials.addData('List1_port.stopped', List1_port.tStop)
    # List1A_mic stop & responses
    List1A_mic.stop()  # sometimes helpful
    if not List1A_mic.savedFile:
        List1A_mic.savedFile = None
    # store data for trials (TrialHandler)
    trials.addData('List1A_mic.filename', List1A_mic.savedFile)
    trials.addData('List1A_mic.started', List1A_mic.tStart)
    trials.addData('List1A_mic.stopped', List1A_mic.tStop)
    # List1B_mic stop & responses
    List1B_mic.stop()  # sometimes helpful
    if not List1B_mic.savedFile:
        List1B_mic.savedFile = None
    # store data for trials (TrialHandler)
    trials.addData('List1B_mic.filename', List1B_mic.savedFile)
    trials.addData('List1B_mic.started', List1B_mic.tStart)
    trials.addData('List1B_mic.stopped', List1B_mic.tStop)
    thisExp.nextEntry()
    
# completed 1.0 repeats of 'trials'


# ------Prepare to start Routine "BeforeQList2"-------
continueRoutine = True
routineTimer.add(5.500000)
# update component parameters for each repeat
key_resp_4.keys = []
key_resp_4.rt = []
_key_resp_4_allKeys = []
# keep track of which components have finished
BeforeQList2Components = [key_resp_4, text_3]
for thisComponent in BeforeQList2Components:
    thisComponent.tStart = None
    thisComponent.tStop = None
    thisComponent.tStartRefresh = None
    thisComponent.tStopRefresh = None
    if hasattr(thisComponent, 'status'):
        thisComponent.status = NOT_STARTED
# reset timers
t = 0
_timeToFirstFrame = win.getFutureFlipTime(clock="now")
BeforeQList2Clock.reset(-_timeToFirstFrame)  # t0 is time of first possible flip
frameN = -1

# -------Run Routine "BeforeQList2"-------
while continueRoutine and routineTimer.getTime() > 0:
    # get current time
    t = BeforeQList2Clock.getTime()
    tThisFlip = win.getFutureFlipTime(clock=BeforeQList2Clock)
    tThisFlipGlobal = win.getFutureFlipTime(clock=None)
    frameN = frameN + 1  # number of completed frames (so 0 is the first frame)
    # update/draw components on each frame
    
    # *key_resp_4* updates
    waitOnFlip = False
    if key_resp_4.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
        # keep track of start time/frame for later
        key_resp_4.frameNStart = frameN  # exact frame index
        key_resp_4.tStart = t  # local t and not account for scr refresh
        key_resp_4.tStartRefresh = tThisFlipGlobal  # on global time
        win.timeOnFlip(key_resp_4, 'tStartRefresh')  # time at next scr refresh
        key_resp_4.status = STARTED
        # keyboard checking is just starting
        waitOnFlip = True
        win.callOnFlip(key_resp_4.clock.reset)  # t=0 on next screen flip
        win.callOnFlip(key_resp_4.clearEvents, eventType='keyboard')  # clear events on next screen flip
    if key_resp_4.status == STARTED:
        # is it time to stop? (based on global clock, using actual start)
        if tThisFlipGlobal > key_resp_4.tStartRefresh + 5.0-frameTolerance:
            # keep track of stop time/frame for later
            key_resp_4.tStop = t  # not accounting for scr refresh
            key_resp_4.frameNStop = frameN  # exact frame index
            win.timeOnFlip(key_resp_4, 'tStopRefresh')  # time at next scr refresh
            key_resp_4.status = FINISHED
    if key_resp_4.status == STARTED and not waitOnFlip:
        theseKeys = key_resp_4.getKeys(keyList=['y', 'n', 'left', 'right', 'space', 'return'], waitRelease=False)
        _key_resp_4_allKeys.extend(theseKeys)
        if len(_key_resp_4_allKeys):
            key_resp_4.keys = _key_resp_4_allKeys[-1].name  # just the last key pressed
            key_resp_4.rt = _key_resp_4_allKeys[-1].rt
            # a response ends the routine
            continueRoutine = False
    
    # *text_3* updates
    if text_3.status == NOT_STARTED and tThisFlip >= 0.5-frameTolerance:
        text_3.draw(win)
        text_3.draw(win1)
        text_3.draw(win2)
        # keep track of start time/frame for later
        text_3.frameNStart = frameN  # exact frame index
        text_3.tStart = t  # local t and not account for scr refresh
        text_3.tStartRefresh = tThisFlipGlobal  # on global time
        win.timeOnFlip(text_3, 'tStartRefresh')  # time at next scr refresh
#        text_3.setAutoDraw(True)
    if text_3.status == STARTED:
        # is it time to stop? (based on global clock, using actual start)
        if tThisFlipGlobal > text_3.tStartRefresh + 5.0-frameTolerance:
            # keep track of stop time/frame for later
            text_3.tStop = t  # not accounting for scr refresh
            text_3.frameNStop = frameN  # exact frame index
            win.timeOnFlip(text_3, 'tStopRefresh')  # time at next scr refresh
            text_3.setAutoDraw(False)
    
    # check for quit (typically the Esc key)
    if endExpNow or defaultKeyboard.getKeys(keyList=["escape"]):
        core.quit()
    
    # check if all components have finished
    if not continueRoutine:  # a component has requested a forced-end of Routine
        break
    continueRoutine = False  # will revert to True if at least one component still running
    for thisComponent in BeforeQList2Components:
        if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
            continueRoutine = True
            break  # at least one component has not yet finished
    
    # refresh the screen
    if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
        win.flip()
        win1.flip()
        win2.flip()

# -------Ending Routine "BeforeQList2"-------
for thisComponent in BeforeQList2Components:
    if hasattr(thisComponent, "setAutoDraw"):
        thisComponent.setAutoDraw(False)
# check responses
if key_resp_4.keys in ['', [], None]:  # No response was made
    key_resp_4.keys = None
thisExp.addData('key_resp_4.keys',key_resp_4.keys)
if key_resp_4.keys != None:  # we had a response
    thisExp.addData('key_resp_4.rt', key_resp_4.rt)
thisExp.addData('key_resp_4.started', key_resp_4.tStartRefresh)
thisExp.addData('key_resp_4.stopped', key_resp_4.tStopRefresh)
thisExp.nextEntry()
thisExp.addData('text_3.started', text_3.tStartRefresh)
thisExp.addData('text_3.stopped', text_3.tStopRefresh)

# set up handler to look after randomisation of conditions etc
trials_2 = data.TrialHandler(nReps=1.0, method='random', 
    extraInfo=expInfo, originPath=-1,
    trialList=data.importConditions("lists"+expInfo['group']+".csv"),
    seed=None, name='trials_2')
thisExp.addLoop(trials_2)  # add the loop to the experiment
thisTrial_2 = trials_2.trialList[0]  # so we can initialise stimuli with some values
# abbreviate parameter names if possible (e.g. rgb = thisTrial_2.rgb)
if thisTrial_2 != None:
    for paramName in thisTrial_2:
        exec('{} = thisTrial_2[paramName]'.format(paramName))

for thisTrial_2 in trials_2:
    currentLoop = trials_2
    # abbreviate parameter names if possible (e.g. rgb = thisTrial_2.rgb)
    if thisTrial_2 != None:
        for paramName in thisTrial_2:
            exec('{} = thisTrial_2[paramName]'.format(paramName))
    
    # ------Prepare to start Routine "QList2"-------
    continueRoutine = True
    routineTimer.add(185.000000)
    # update component parameters for each repeat
    key_resp_5.keys = []
    key_resp_5.rt = []
    _key_resp_5_allKeys = []
    text_4.setText(questions2)
    List2A_mic = microphone.AdvAudioCapture(name='List2A_mic', saveDir=wavDirName, stereo=False, chnl=0)
    List2B_mic = microphone.AdvAudioCapture(name='List2B_mic', saveDir=wavDirName, stereo=False, chnl=1)
    # keep track of which components have finished
    QList2Components = [key_resp_5, text_4, List2_port, List2A_mic, List2B_mic]
    for thisComponent in QList2Components:
        thisComponent.tStart = None
        thisComponent.tStop = None
        thisComponent.tStartRefresh = None
        thisComponent.tStopRefresh = None
        if hasattr(thisComponent, 'status'):
            thisComponent.status = NOT_STARTED
    # reset timers
    t = 0
    _timeToFirstFrame = win.getFutureFlipTime(clock="now")
    QList2Clock.reset(-_timeToFirstFrame)  # t0 is time of first possible flip
    frameN = -1
    
    # -------Run Routine "QList2"-------
    while continueRoutine and routineTimer.getTime() > 0:
        # get current time
        t = QList2Clock.getTime()
        tThisFlip = win.getFutureFlipTime(clock=QList2Clock)
        tThisFlipGlobal = win.getFutureFlipTime(clock=None)
        frameN = frameN + 1  # number of completed frames (so 0 is the first frame)
        # update/draw components on each frame
        
        # *key_resp_5* updates
        waitOnFlip = False
        if key_resp_5.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            key_resp_5.frameNStart = frameN  # exact frame index
            key_resp_5.tStart = t  # local t and not account for scr refresh
            key_resp_5.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(key_resp_5, 'tStartRefresh')  # time at next scr refresh
            key_resp_5.status = STARTED
            # keyboard checking is just starting
            waitOnFlip = True
            win.callOnFlip(key_resp_5.clock.reset)  # t=0 on next screen flip
            win.callOnFlip(key_resp_5.clearEvents, eventType='keyboard')  # clear events on next screen flip
        if key_resp_5.status == STARTED:
            # is it time to stop? (based on global clock, using actual start)
            if tThisFlipGlobal > key_resp_5.tStartRefresh + 15.0-frameTolerance:
                # keep track of stop time/frame for later
                key_resp_5.tStop = t  # not accounting for scr refresh
                key_resp_5.frameNStop = frameN  # exact frame index
                win.timeOnFlip(key_resp_5, 'tStopRefresh')  # time at next scr refresh
                key_resp_5.status = FINISHED
        if key_resp_5.status == STARTED and not waitOnFlip:
            theseKeys = key_resp_5.getKeys(keyList=['y', 'n', 'left', 'right', 'space', 'return'], waitRelease=False)
            _key_resp_5_allKeys.extend(theseKeys)
            if len(_key_resp_5_allKeys):
                key_resp_5.keys = _key_resp_5_allKeys[-1].name  # just the last key pressed
                key_resp_5.rt = _key_resp_5_allKeys[-1].rt
                # a response ends the routine
                continueRoutine = False
        
        # *text_4* updates
        if text_4.status == NOT_STARTED and tThisFlip >= 1.0-frameTolerance:
            text_4.draw(win)
            text_4.draw(win1)
            text_4.draw(win2)
            # keep track of start time/frame for later
            text_4.frameNStart = frameN  # exact frame index
            text_4.tStart = t  # local t and not account for scr refresh
            text_4.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(text_4, 'tStartRefresh')  # time at next scr refresh
#            text_4.setAutoDraw(True)
        if text_4.status == STARTED:
            # is it time to stop? (based on global clock, using actual start)
            if tThisFlipGlobal > text_4.tStartRefresh + 184.0-frameTolerance:
                # keep track of stop time/frame for later
                text_4.tStop = t  # not accounting for scr refresh
                text_4.frameNStop = frameN  # exact frame index
                win.timeOnFlip(text_4, 'tStopRefresh')  # time at next scr refresh
                text_4.setAutoDraw(False)
        # *List2_port* updates
        if List2_port.status == NOT_STARTED and t >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            List2_port.frameNStart = frameN  # exact frame index
            List2_port.tStart = t  # local t and not account for scr refresh
            List2_port.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(List2_port, 'tStartRefresh')  # time at next scr refresh
            List2_port.status = STARTED
            win.callOnFlip(List2_port.setData, int(1))
        if List2_port.status == STARTED:
            # is it time to stop? (based on global clock, using actual start)
            if tThisFlipGlobal > List2_port.tStartRefresh + 185.0-frameTolerance:
                # keep track of stop time/frame for later
                List2_port.tStop = t  # not accounting for scr refresh
                List2_port.frameNStop = frameN  # exact frame index
                win.timeOnFlip(List2_port, 'tStopRefresh')  # time at next scr refresh
                List2_port.status = FINISHED
                win.callOnFlip(List2_port.setData, int(0))
        
        # *List2A_mic* updates
        if List2A_mic.status == NOT_STARTED and t >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            List2A_mic.frameNStart = frameN  # exact frame index
            List2A_mic.tStart = t  # local t and not account for scr refresh
            List2A_mic.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(List2A_mic, 'tStartRefresh')  # time at next scr refresh
            List2A_mic.status = STARTED
            List2A_mic.record(sec=185.0, block=False)  # start the recording thread
        
        if List2A_mic.status == STARTED and not List2A_mic.recorder.running:
            List2A_mic.status = FINISHED
        
        # *List2B_mic* updates
        if List2B_mic.status == NOT_STARTED and t >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            List2B_mic.frameNStart = frameN  # exact frame index
            List2B_mic.tStart = t  # local t and not account for scr refresh
            List2B_mic.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(List2B_mic, 'tStartRefresh')  # time at next scr refresh
            List2B_mic.status = STARTED
            List2B_mic.record(sec=185.0, block=False)  # start the recording thread
        
        if List2B_mic.status == STARTED and not List2B_mic.recorder.running:
            List2B_mic.status = FINISHED
        
        # check for quit (typically the Esc key)
        if endExpNow or defaultKeyboard.getKeys(keyList=["escape"]):
            core.quit()
        
        # check if all components have finished
        if not continueRoutine:  # a component has requested a forced-end of Routine
            break
        continueRoutine = False  # will revert to True if at least one component still running
        for thisComponent in QList2Components:
            if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
                continueRoutine = True
                break  # at least one component has not yet finished
        
        # refresh the screen
        if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
            win.flip()
            win1.flip()
            win2.flip()
    
    # -------Ending Routine "QList2"-------
    for thisComponent in QList2Components:
        if hasattr(thisComponent, "setAutoDraw"):
            thisComponent.setAutoDraw(False)
    # check responses
    if key_resp_5.keys in ['', [], None]:  # No response was made
        key_resp_5.keys = None
    trials_2.addData('key_resp_5.keys',key_resp_5.keys)
    if key_resp_5.keys != None:  # we had a response
        trials_2.addData('key_resp_5.rt', key_resp_5.rt)
    trials_2.addData('key_resp_5.started', key_resp_5.tStartRefresh)
    trials_2.addData('key_resp_5.stopped', key_resp_5.tStopRefresh)
    trials_2.addData('text_4.started', text_4.tStartRefresh)
    trials_2.addData('text_4.stopped', text_4.tStopRefresh)
    if List2_port.status == STARTED:
        win.callOnFlip(List2_port.setData, int(0))
    trials_2.addData('List2_port.started', List2_port.tStart)
    trials_2.addData('List2_port.stopped', List2_port.tStop)
    # List2A_mic stop & responses
    List2A_mic.stop()  # sometimes helpful
    if not List2A_mic.savedFile:
        List2A_mic.savedFile = None
    # store data for trials_2 (TrialHandler)
    trials_2.addData('List2A_mic.filename', List2A_mic.savedFile)
    trials_2.addData('List2A_mic.started', List2A_mic.tStart)
    trials_2.addData('List2A_mic.stopped', List2A_mic.tStop)
    # List2B_mic stop & responses
    List2B_mic.stop()  # sometimes helpful
    if not List2B_mic.savedFile:
        List2B_mic.savedFile = None
    # store data for trials_2 (TrialHandler)
    trials_2.addData('List2B_mic.filename', List2B_mic.savedFile)
    trials_2.addData('List2B_mic.started', List2B_mic.tStart)
    trials_2.addData('List2B_mic.stopped', List2B_mic.tStop)
    thisExp.nextEntry()
    
# completed 1.0 repeats of 'trials_2'


# ------Prepare to start Routine "BeforeQList3"-------
continueRoutine = True
routineTimer.add(5.500000)
# update component parameters for each repeat
key_resp_6.keys = []
key_resp_6.rt = []
_key_resp_6_allKeys = []
# keep track of which components have finished
BeforeQList3Components = [key_resp_6, text_5]
for thisComponent in BeforeQList3Components:
    thisComponent.tStart = None
    thisComponent.tStop = None
    thisComponent.tStartRefresh = None
    thisComponent.tStopRefresh = None
    if hasattr(thisComponent, 'status'):
        thisComponent.status = NOT_STARTED
# reset timers
t = 0
_timeToFirstFrame = win.getFutureFlipTime(clock="now")
BeforeQList3Clock.reset(-_timeToFirstFrame)  # t0 is time of first possible flip
frameN = -1

# -------Run Routine "BeforeQList3"-------
while continueRoutine and routineTimer.getTime() > 0:
    # get current time
    t = BeforeQList3Clock.getTime()
    tThisFlip = win.getFutureFlipTime(clock=BeforeQList3Clock)
    tThisFlipGlobal = win.getFutureFlipTime(clock=None)
    frameN = frameN + 1  # number of completed frames (so 0 is the first frame)
    # update/draw components on each frame
    
    # *key_resp_6* updates
    waitOnFlip = False
    if key_resp_6.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
        # keep track of start time/frame for later
        key_resp_6.frameNStart = frameN  # exact frame index
        key_resp_6.tStart = t  # local t and not account for scr refresh
        key_resp_6.tStartRefresh = tThisFlipGlobal  # on global time
        win.timeOnFlip(key_resp_6, 'tStartRefresh')  # time at next scr refresh
        key_resp_6.status = STARTED
        # keyboard checking is just starting
        waitOnFlip = True
        win.callOnFlip(key_resp_6.clock.reset)  # t=0 on next screen flip
        win.callOnFlip(key_resp_6.clearEvents, eventType='keyboard')  # clear events on next screen flip
    if key_resp_6.status == STARTED:
        # is it time to stop? (based on global clock, using actual start)
        if tThisFlipGlobal > key_resp_6.tStartRefresh + 5.0-frameTolerance:
            # keep track of stop time/frame for later
            key_resp_6.tStop = t  # not accounting for scr refresh
            key_resp_6.frameNStop = frameN  # exact frame index
            win.timeOnFlip(key_resp_6, 'tStopRefresh')  # time at next scr refresh
            key_resp_6.status = FINISHED
    if key_resp_6.status == STARTED and not waitOnFlip:
        theseKeys = key_resp_6.getKeys(keyList=['y', 'n', 'left', 'right', 'space', 'return'], waitRelease=False)
        _key_resp_6_allKeys.extend(theseKeys)
        if len(_key_resp_6_allKeys):
            key_resp_6.keys = _key_resp_6_allKeys[-1].name  # just the last key pressed
            key_resp_6.rt = _key_resp_6_allKeys[-1].rt
            # a response ends the routine
            continueRoutine = False
    
    # *text_5* updates
    if text_5.status == NOT_STARTED and tThisFlip >= 0.5-frameTolerance:
        text_5.draw(win)
        text_5.draw(win1)
        text_5.draw(win2)
        # keep track of start time/frame for later
        text_5.frameNStart = frameN  # exact frame index
        text_5.tStart = t  # local t and not account for scr refresh
        text_5.tStartRefresh = tThisFlipGlobal  # on global time
        win.timeOnFlip(text_5, 'tStartRefresh')  # time at next scr refresh
#        text_5.setAutoDraw(True)
    if text_5.status == STARTED:
        # is it time to stop? (based on global clock, using actual start)
        if tThisFlipGlobal > text_5.tStartRefresh + 5.0-frameTolerance:
            # keep track of stop time/frame for later
            text_5.tStop = t  # not accounting for scr refresh
            text_5.frameNStop = frameN  # exact frame index
            win.timeOnFlip(text_5, 'tStopRefresh')  # time at next scr refresh
            text_5.setAutoDraw(False)
    
    # check for quit (typically the Esc key)
    if endExpNow or defaultKeyboard.getKeys(keyList=["escape"]):
        core.quit()
    
    # check if all components have finished
    if not continueRoutine:  # a component has requested a forced-end of Routine
        break
    continueRoutine = False  # will revert to True if at least one component still running
    for thisComponent in BeforeQList3Components:
        if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
            continueRoutine = True
            break  # at least one component has not yet finished
    
    # refresh the screen
    if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
        win.flip()
        win1.flip()
        win2.flip()

# -------Ending Routine "BeforeQList3"-------
for thisComponent in BeforeQList3Components:
    if hasattr(thisComponent, "setAutoDraw"):
        thisComponent.setAutoDraw(False)
# check responses
if key_resp_6.keys in ['', [], None]:  # No response was made
    key_resp_6.keys = None
thisExp.addData('key_resp_6.keys',key_resp_6.keys)
if key_resp_6.keys != None:  # we had a response
    thisExp.addData('key_resp_6.rt', key_resp_6.rt)
thisExp.addData('key_resp_6.started', key_resp_6.tStartRefresh)
thisExp.addData('key_resp_6.stopped', key_resp_6.tStopRefresh)
thisExp.nextEntry()
thisExp.addData('text_5.started', text_5.tStartRefresh)
thisExp.addData('text_5.stopped', text_5.tStopRefresh)

# set up handler to look after randomisation of conditions etc
trials_3 = data.TrialHandler(nReps=1.0, method='random', 
    extraInfo=expInfo, originPath=-1,
    trialList=data.importConditions("lists"+expInfo['group']+".csv"),
    seed=None, name='trials_3')
thisExp.addLoop(trials_3)  # add the loop to the experiment
thisTrial_3 = trials_3.trialList[0]  # so we can initialise stimuli with some values
# abbreviate parameter names if possible (e.g. rgb = thisTrial_3.rgb)
if thisTrial_3 != None:
    for paramName in thisTrial_3:
        exec('{} = thisTrial_3[paramName]'.format(paramName))

for thisTrial_3 in trials_3:
    currentLoop = trials_3
    # abbreviate parameter names if possible (e.g. rgb = thisTrial_3.rgb)
    if thisTrial_3 != None:
        for paramName in thisTrial_3:
            exec('{} = thisTrial_3[paramName]'.format(paramName))
    
    # ------Prepare to start Routine "QList3"-------
    continueRoutine = True
    routineTimer.add(305.000000)
    # update component parameters for each repeat
    key_resp_7.keys = []
    key_resp_7.rt = []
    _key_resp_7_allKeys = []
    text_6.setText(questions3)
    List3A_mic = microphone.AdvAudioCapture(name='List3A_mic', saveDir=wavDirName, stereo=False, chnl=0)
    List3B_mic = microphone.AdvAudioCapture(name='List3B_mic', saveDir=wavDirName, stereo=False, chnl=1)
    # keep track of which components have finished
    QList3Components = [key_resp_7, text_6, List3_port, List3A_mic, List3B_mic]
    for thisComponent in QList3Components:
        thisComponent.tStart = None
        thisComponent.tStop = None
        thisComponent.tStartRefresh = None
        thisComponent.tStopRefresh = None
        if hasattr(thisComponent, 'status'):
            thisComponent.status = NOT_STARTED
    # reset timers
    t = 0
    _timeToFirstFrame = win.getFutureFlipTime(clock="now")
    QList3Clock.reset(-_timeToFirstFrame)  # t0 is time of first possible flip
    frameN = -1
    
    # -------Run Routine "QList3"-------
    while continueRoutine and routineTimer.getTime() > 0:
        # get current time
        t = QList3Clock.getTime()
        tThisFlip = win.getFutureFlipTime(clock=QList3Clock)
        tThisFlipGlobal = win.getFutureFlipTime(clock=None)
        frameN = frameN + 1  # number of completed frames (so 0 is the first frame)
        # update/draw components on each frame
        
        # *key_resp_7* updates
        waitOnFlip = False
        if key_resp_7.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            key_resp_7.frameNStart = frameN  # exact frame index
            key_resp_7.tStart = t  # local t and not account for scr refresh
            key_resp_7.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(key_resp_7, 'tStartRefresh')  # time at next scr refresh
            key_resp_7.status = STARTED
            # keyboard checking is just starting
            waitOnFlip = True
            win.callOnFlip(key_resp_7.clock.reset)  # t=0 on next screen flip
            win.callOnFlip(key_resp_7.clearEvents, eventType='keyboard')  # clear events on next screen flip
        if key_resp_7.status == STARTED:
            # is it time to stop? (based on global clock, using actual start)
            if tThisFlipGlobal > key_resp_7.tStartRefresh + 15.0-frameTolerance:
                # keep track of stop time/frame for later
                key_resp_7.tStop = t  # not accounting for scr refresh
                key_resp_7.frameNStop = frameN  # exact frame index
                win.timeOnFlip(key_resp_7, 'tStopRefresh')  # time at next scr refresh
                key_resp_7.status = FINISHED
        if key_resp_7.status == STARTED and not waitOnFlip:
            theseKeys = key_resp_7.getKeys(keyList=['y', 'n', 'left', 'right', 'space', 'return'], waitRelease=False)
            _key_resp_7_allKeys.extend(theseKeys)
            if len(_key_resp_7_allKeys):
                key_resp_7.keys = _key_resp_7_allKeys[-1].name  # just the last key pressed
                key_resp_7.rt = _key_resp_7_allKeys[-1].rt
                # a response ends the routine
                continueRoutine = False
        
        # *text_6* updates
        if text_6.status == NOT_STARTED and tThisFlip >= 1.0-frameTolerance:
            text_6.draw(win)
            text_6.draw(win1)
            text_6.draw(win2)
            # keep track of start time/frame for later
            text_6.frameNStart = frameN  # exact frame index
            text_6.tStart = t  # local t and not account for scr refresh
            text_6.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(text_6, 'tStartRefresh')  # time at next scr refresh
#            text_6.setAutoDraw(True)
        if text_6.status == STARTED:
            # is it time to stop? (based on global clock, using actual start)
            if tThisFlipGlobal > text_6.tStartRefresh + 304.0-frameTolerance:
                # keep track of stop time/frame for later
                text_6.tStop = t  # not accounting for scr refresh
                text_6.frameNStop = frameN  # exact frame index
                win.timeOnFlip(text_6, 'tStopRefresh')  # time at next scr refresh
                text_6.setAutoDraw(False)
        # *List3_port* updates
        if List3_port.status == NOT_STARTED and t >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            List3_port.frameNStart = frameN  # exact frame index
            List3_port.tStart = t  # local t and not account for scr refresh
            List3_port.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(List3_port, 'tStartRefresh')  # time at next scr refresh
            List3_port.status = STARTED
            win.callOnFlip(List3_port.setData, int(1))
        if List3_port.status == STARTED:
            # is it time to stop? (based on global clock, using actual start)
            if tThisFlipGlobal > List3_port.tStartRefresh + 305.0-frameTolerance:
                # keep track of stop time/frame for later
                List3_port.tStop = t  # not accounting for scr refresh
                List3_port.frameNStop = frameN  # exact frame index
                win.timeOnFlip(List3_port, 'tStopRefresh')  # time at next scr refresh
                List3_port.status = FINISHED
                win.callOnFlip(List3_port.setData, int(0))
        
        # *List3A_mic* updates
        if List3A_mic.status == NOT_STARTED and t >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            List3A_mic.frameNStart = frameN  # exact frame index
            List3A_mic.tStart = t  # local t and not account for scr refresh
            List3A_mic.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(List3A_mic, 'tStartRefresh')  # time at next scr refresh
            List3A_mic.status = STARTED
            List3A_mic.record(sec=305.0, block=False)  # start the recording thread
        
        if List3A_mic.status == STARTED and not List3A_mic.recorder.running:
            List3A_mic.status = FINISHED
        
        # *List3B_mic* updates
        if List3B_mic.status == NOT_STARTED and t >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            List3B_mic.frameNStart = frameN  # exact frame index
            List3B_mic.tStart = t  # local t and not account for scr refresh
            List3B_mic.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(List3B_mic, 'tStartRefresh')  # time at next scr refresh
            List3B_mic.status = STARTED
            List3B_mic.record(sec=305.0, block=False)  # start the recording thread
        
        if List3B_mic.status == STARTED and not List3B_mic.recorder.running:
            List3B_mic.status = FINISHED
        
        # check for quit (typically the Esc key)
        if endExpNow or defaultKeyboard.getKeys(keyList=["escape"]):
            core.quit()
        
        # check if all components have finished
        if not continueRoutine:  # a component has requested a forced-end of Routine
            break
        continueRoutine = False  # will revert to True if at least one component still running
        for thisComponent in QList3Components:
            if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
                continueRoutine = True
                break  # at least one component has not yet finished
        
        # refresh the screen
        if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
            win.flip()
            win1.flip()
            win2.flip()
    
    # -------Ending Routine "QList3"-------
    for thisComponent in QList3Components:
        if hasattr(thisComponent, "setAutoDraw"):
            thisComponent.setAutoDraw(False)
    # check responses
    if key_resp_7.keys in ['', [], None]:  # No response was made
        key_resp_7.keys = None
    trials_3.addData('key_resp_7.keys',key_resp_7.keys)
    if key_resp_7.keys != None:  # we had a response
        trials_3.addData('key_resp_7.rt', key_resp_7.rt)
    trials_3.addData('key_resp_7.started', key_resp_7.tStartRefresh)
    trials_3.addData('key_resp_7.stopped', key_resp_7.tStopRefresh)
    trials_3.addData('text_6.started', text_6.tStartRefresh)
    trials_3.addData('text_6.stopped', text_6.tStopRefresh)
    if List3_port.status == STARTED:
        win.callOnFlip(List3_port.setData, int(0))
    trials_3.addData('List3_port.started', List3_port.tStart)
    trials_3.addData('List3_port.stopped', List3_port.tStop)
    # List3A_mic stop & responses
    List3A_mic.stop()  # sometimes helpful
    if not List3A_mic.savedFile:
        List3A_mic.savedFile = None
    # store data for trials_3 (TrialHandler)
    trials_3.addData('List3A_mic.filename', List3A_mic.savedFile)
    trials_3.addData('List3A_mic.started', List3A_mic.tStart)
    trials_3.addData('List3A_mic.stopped', List3A_mic.tStop)
    # List3B_mic stop & responses
    List3B_mic.stop()  # sometimes helpful
    if not List3B_mic.savedFile:
        List3B_mic.savedFile = None
    # store data for trials_3 (TrialHandler)
    trials_3.addData('List3B_mic.filename', List3B_mic.savedFile)
    trials_3.addData('List3B_mic.started', List3B_mic.tStart)
    trials_3.addData('List3B_mic.stopped', List3B_mic.tStop)
    thisExp.nextEntry()
    
# completed 1.0 repeats of 'trials_3'


# ------Prepare to start Routine "BeforeDiapix"-------
continueRoutine = True
# update component parameters for each repeat
key_resp_8.keys = []
key_resp_8.rt = []
_key_resp_8_allKeys = []
# keep track of which components have finished
BeforeDiapixComponents = [key_resp_8]
for thisComponent in BeforeDiapixComponents:
    thisComponent.tStart = None
    thisComponent.tStop = None
    thisComponent.tStartRefresh = None
    thisComponent.tStopRefresh = None
    if hasattr(thisComponent, 'status'):
        thisComponent.status = NOT_STARTED
# reset timers
t = 0
_timeToFirstFrame = win.getFutureFlipTime(clock="now")
BeforeDiapixClock.reset(-_timeToFirstFrame)  # t0 is time of first possible flip
frameN = -1

# -------Run Routine "BeforeDiapix"-------
while continueRoutine:
    # get current time
    t = BeforeDiapixClock.getTime()
    tThisFlip = win.getFutureFlipTime(clock=BeforeDiapixClock)
    tThisFlipGlobal = win.getFutureFlipTime(clock=None)
    frameN = frameN + 1  # number of completed frames (so 0 is the first frame)
    # update/draw components on each frame
    
    # *key_resp_8* updates
    waitOnFlip = False
    if key_resp_8.status == NOT_STARTED and tThisFlip >= 2.0-frameTolerance:
        # keep track of start time/frame for later
        key_resp_8.frameNStart = frameN  # exact frame index
        key_resp_8.tStart = t  # local t and not account for scr refresh
        key_resp_8.tStartRefresh = tThisFlipGlobal  # on global time
        win.timeOnFlip(key_resp_8, 'tStartRefresh')  # time at next scr refresh
        key_resp_8.status = STARTED
        # keyboard checking is just starting
        waitOnFlip = True
        win.callOnFlip(key_resp_8.clock.reset)  # t=0 on next screen flip
        win.callOnFlip(key_resp_8.clearEvents, eventType='keyboard')  # clear events on next screen flip
    if key_resp_8.status == STARTED and not waitOnFlip:
        theseKeys = key_resp_8.getKeys(keyList=['y', 'n', 'left', 'right', 'space', 'return'], waitRelease=False)
        _key_resp_8_allKeys.extend(theseKeys)
        if len(_key_resp_8_allKeys):
            key_resp_8.keys = _key_resp_8_allKeys[-1].name  # just the last key pressed
            key_resp_8.rt = _key_resp_8_allKeys[-1].rt
            # a response ends the routine
            continueRoutine = False
    
    # check for quit (typically the Esc key)
    if endExpNow or defaultKeyboard.getKeys(keyList=["escape"]):
        core.quit()
    
    # check if all components have finished
    if not continueRoutine:  # a component has requested a forced-end of Routine
        break
    continueRoutine = False  # will revert to True if at least one component still running
    for thisComponent in BeforeDiapixComponents:
        if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
            continueRoutine = True
            break  # at least one component has not yet finished
    
    # refresh the screen
    if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
        win.flip()
        win1.flip()
        win2.flip()

# -------Ending Routine "BeforeDiapix"-------
for thisComponent in BeforeDiapixComponents:
    if hasattr(thisComponent, "setAutoDraw"):
        thisComponent.setAutoDraw(False)
# check responses
if key_resp_8.keys in ['', [], None]:  # No response was made
    key_resp_8.keys = None
thisExp.addData('key_resp_8.keys',key_resp_8.keys)
if key_resp_8.keys != None:  # we had a response
    thisExp.addData('key_resp_8.rt', key_resp_8.rt)
thisExp.addData('key_resp_8.started', key_resp_8.tStartRefresh)
thisExp.addData('key_resp_8.stopped', key_resp_8.tStopRefresh)
thisExp.nextEntry()
# the Routine "BeforeDiapix" was not non-slip safe, so reset the non-slip timer
routineTimer.reset()

# set up handler to look after randomisation of conditions etc
trials_4 = data.TrialHandler(nReps=1.0, method='random', 
    extraInfo=expInfo, originPath=-1,
    trialList=data.importConditions('images.csv'),
    seed=None, name='trials_4')
thisExp.addLoop(trials_4)  # add the loop to the experiment
thisTrial_4 = trials_4.trialList[0]  # so we can initialise stimuli with some values
# abbreviate parameter names if possible (e.g. rgb = thisTrial_4.rgb)
if thisTrial_4 != None:
    for paramName in thisTrial_4:
        exec('{} = thisTrial_4[paramName]'.format(paramName))

for thisTrial_4 in trials_4:
    currentLoop = trials_4
    # abbreviate parameter names if possible (e.g. rgb = thisTrial_4.rgb)
    if thisTrial_4 != None:
        for paramName in thisTrial_4:
            exec('{} = thisTrial_4[paramName]'.format(paramName))
    
    # ------Prepare to start Routine "Diapix"-------
    continueRoutine = True
    # update component parameters for each repeat
    key_resp_9.keys = []
    key_resp_9.rt = []
    _key_resp_9_allKeys = []
    imageA.setImage(imagefileA)
    imageB.setImage(imagefileB)
    DiapixA_mic = microphone.AdvAudioCapture(name='DiapixA_mic', saveDir=wavDirName, stereo=False, chnl=0)
    DiapixB_mic = microphone.AdvAudioCapture(name='DiapixB_mic', saveDir=wavDirName, stereo=False, chnl=1)
    # keep track of which components have finished
    DiapixComponents = [key_resp_9, imageA, imageB, DiapixA_mic, DiapixB_mic, Diapix_port]
    for thisComponent in DiapixComponents:
        thisComponent.tStart = None
        thisComponent.tStop = None
        thisComponent.tStartRefresh = None
        thisComponent.tStopRefresh = None
        if hasattr(thisComponent, 'status'):
            thisComponent.status = NOT_STARTED
    # reset timers
    t = 0
    _timeToFirstFrame = win.getFutureFlipTime(clock="now")
    DiapixClock.reset(-_timeToFirstFrame)  # t0 is time of first possible flip
    frameN = -1
    
    # -------Run Routine "Diapix"-------
    while continueRoutine:
        # get current time
        t = DiapixClock.getTime()
        tThisFlip = win.getFutureFlipTime(clock=DiapixClock)
        tThisFlipGlobal = win.getFutureFlipTime(clock=None)
        frameN = frameN + 1  # number of completed frames (so 0 is the first frame)
        # update/draw components on each frame
        
        # *key_resp_9* updates
        waitOnFlip = False
        if key_resp_9.status == NOT_STARTED and tThisFlip >= 2.0-frameTolerance:
            # keep track of start time/frame for later
            key_resp_9.frameNStart = frameN  # exact frame index
            key_resp_9.tStart = t  # local t and not account for scr refresh
            key_resp_9.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(key_resp_9, 'tStartRefresh')  # time at next scr refresh
            key_resp_9.status = STARTED
            # keyboard checking is just starting
            waitOnFlip = True
            win.callOnFlip(key_resp_9.clock.reset)  # t=0 on next screen flip
            win.callOnFlip(key_resp_9.clearEvents, eventType='keyboard')  # clear events on next screen flip
        if key_resp_9.status == STARTED and not waitOnFlip:
            theseKeys = key_resp_9.getKeys(keyList=['y', 'n', 'left', 'right', 'space', 'return'], waitRelease=False)
            _key_resp_9_allKeys.extend(theseKeys)
            if len(_key_resp_9_allKeys):
                key_resp_9.keys = _key_resp_9_allKeys[-1].name  # just the last key pressed
                key_resp_9.rt = _key_resp_9_allKeys[-1].rt
                # a response ends the routine
                continueRoutine = False
        
        # *imageA* updates
        if imageA.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            imageA.draw(win)
            imageA.draw(win2)
            # keep track of start time/frame for later
            imageA.frameNStart = frameN  # exact frame index
            imageA.tStart = t  # local t and not account for scr refresh
            imageA.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(imageA, 'tStartRefresh')  # time at next scr refresh
#            imageA.setAutoDraw(True)
        
        # *imageB* updates
        if imageB.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            imageA.draw(win1)
            # keep track of start time/frame for later
            imageB.frameNStart = frameN  # exact frame index
            imageB.tStart = t  # local t and not account for scr refresh
            imageB.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(imageB, 'tStartRefresh')  # time at next scr refresh
#            imageB.setAutoDraw(True)
        
        # *DiapixA_mic* updates
        if DiapixA_mic.status == NOT_STARTED and t >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            DiapixA_mic.frameNStart = frameN  # exact frame index
            DiapixA_mic.tStart = t  # local t and not account for scr refresh
            DiapixA_mic.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(DiapixA_mic, 'tStartRefresh')  # time at next scr refresh
            DiapixA_mic.status = STARTED
            DiapixA_mic.record(sec=900, block=False)  # start the recording thread
        
        if DiapixA_mic.status == STARTED and not DiapixA_mic.recorder.running:
            DiapixA_mic.status = FINISHED
        
        # *DiapixB_mic* updates
        if DiapixB_mic.status == NOT_STARTED and t >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            DiapixB_mic.frameNStart = frameN  # exact frame index
            DiapixB_mic.tStart = t  # local t and not account for scr refresh
            DiapixB_mic.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(DiapixB_mic, 'tStartRefresh')  # time at next scr refresh
            DiapixB_mic.status = STARTED
            DiapixB_mic.record(sec=900, block=False)  # start the recording thread
        
        if DiapixB_mic.status == STARTED and not DiapixB_mic.recorder.running:
            DiapixB_mic.status = FINISHED
        # *Diapix_port* updates
        if Diapix_port.status == NOT_STARTED and t >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            Diapix_port.frameNStart = frameN  # exact frame index
            Diapix_port.tStart = t  # local t and not account for scr refresh
            Diapix_port.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(Diapix_port, 'tStartRefresh')  # time at next scr refresh
            Diapix_port.status = STARTED
            win.callOnFlip(Diapix_port.setData, int(1))
        if Diapix_port.status == STARTED:
            # is it time to stop? (based on global clock, using actual start)
            if tThisFlipGlobal > Diapix_port.tStartRefresh + 1800.0-frameTolerance:
                # keep track of stop time/frame for later
                Diapix_port.tStop = t  # not accounting for scr refresh
                Diapix_port.frameNStop = frameN  # exact frame index
                win.timeOnFlip(Diapix_port, 'tStopRefresh')  # time at next scr refresh
                Diapix_port.status = FINISHED
                win.callOnFlip(Diapix_port.setData, int(0))
        
        # check for quit (typically the Esc key)
        if endExpNow or defaultKeyboard.getKeys(keyList=["escape"]):
            core.quit()
        
        # check if all components have finished
        if not continueRoutine:  # a component has requested a forced-end of Routine
            break
        continueRoutine = False  # will revert to True if at least one component still running
        for thisComponent in DiapixComponents:
            if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
                continueRoutine = True
                break  # at least one component has not yet finished
        
        # refresh the screen
        if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
            win.flip()
            win1.flip()
            win2.flip()
    
    # -------Ending Routine "Diapix"-------
    for thisComponent in DiapixComponents:
        if hasattr(thisComponent, "setAutoDraw"):
            thisComponent.setAutoDraw(False)
    # check responses
    if key_resp_9.keys in ['', [], None]:  # No response was made
        key_resp_9.keys = None
    trials_4.addData('key_resp_9.keys',key_resp_9.keys)
    if key_resp_9.keys != None:  # we had a response
        trials_4.addData('key_resp_9.rt', key_resp_9.rt)
    trials_4.addData('key_resp_9.started', key_resp_9.tStartRefresh)
    trials_4.addData('key_resp_9.stopped', key_resp_9.tStopRefresh)
    trials_4.addData('imageA.started', imageA.tStartRefresh)
    trials_4.addData('imageA.stopped', imageA.tStopRefresh)
    trials_4.addData('imageB.started', imageB.tStartRefresh)
    trials_4.addData('imageB.stopped', imageB.tStopRefresh)
    # DiapixA_mic stop & responses
    DiapixA_mic.stop()  # sometimes helpful
    if not DiapixA_mic.savedFile:
        DiapixA_mic.savedFile = None
    # store data for trials_4 (TrialHandler)
    trials_4.addData('DiapixA_mic.filename', DiapixA_mic.savedFile)
    trials_4.addData('DiapixA_mic.started', DiapixA_mic.tStart)
    trials_4.addData('DiapixA_mic.stopped', DiapixA_mic.tStop)
    # DiapixB_mic stop & responses
    DiapixB_mic.stop()  # sometimes helpful
    if not DiapixB_mic.savedFile:
        DiapixB_mic.savedFile = None
    # store data for trials_4 (TrialHandler)
    trials_4.addData('DiapixB_mic.filename', DiapixB_mic.savedFile)
    trials_4.addData('DiapixB_mic.started', DiapixB_mic.tStart)
    trials_4.addData('DiapixB_mic.stopped', DiapixB_mic.tStop)
    if Diapix_port.status == STARTED:
        win.callOnFlip(Diapix_port.setData, int(0))
    trials_4.addData('Diapix_port.started', Diapix_port.tStart)
    trials_4.addData('Diapix_port.stopped', Diapix_port.tStop)
    # the Routine "Diapix" was not non-slip safe, so reset the non-slip timer
    routineTimer.reset()
    thisExp.nextEntry()
    
# completed 2.0 repeats of 'trials_4'


# ------Prepare to start Routine "End"-------
continueRoutine = True
# update component parameters for each repeat
key_resp_10.keys = []
key_resp_10.rt = []
_key_resp_10_allKeys = []
# keep track of which components have finished
EndComponents = [text_7, key_resp_10]
for thisComponent in EndComponents:
    thisComponent.tStart = None
    thisComponent.tStop = None
    thisComponent.tStartRefresh = None
    thisComponent.tStopRefresh = None
    if hasattr(thisComponent, 'status'):
        thisComponent.status = NOT_STARTED
# reset timers
t = 0
_timeToFirstFrame = win.getFutureFlipTime(clock="now")
EndClock.reset(-_timeToFirstFrame)  # t0 is time of first possible flip
frameN = -1

# -------Run Routine "End"-------
while continueRoutine:
    # get current time
    t = EndClock.getTime()
    tThisFlip = win.getFutureFlipTime(clock=EndClock)
    tThisFlipGlobal = win.getFutureFlipTime(clock=None)
    frameN = frameN + 1  # number of completed frames (so 0 is the first frame)
    # update/draw components on each frame
    
    # *text_7* updates
    if text_7.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
        text_7.draw(win)
        text_7.draw(win1)
        text_7.draw(win2)
        # keep track of start time/frame for later
        text_7.frameNStart = frameN  # exact frame index
        text_7.tStart = t  # local t and not account for scr refresh
        text_7.tStartRefresh = tThisFlipGlobal  # on global time
        win.timeOnFlip(text_7, 'tStartRefresh')  # time at next scr refresh
#        text_7.setAutoDraw(True)
    
    # *key_resp_10* updates
    waitOnFlip = False
    if key_resp_10.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
        # keep track of start time/frame for later
        key_resp_10.frameNStart = frameN  # exact frame index
        key_resp_10.tStart = t  # local t and not account for scr refresh
        key_resp_10.tStartRefresh = tThisFlipGlobal  # on global time
        win.timeOnFlip(key_resp_10, 'tStartRefresh')  # time at next scr refresh
        key_resp_10.status = STARTED
        # keyboard checking is just starting
        waitOnFlip = True
        win.callOnFlip(key_resp_10.clock.reset)  # t=0 on next screen flip
        win.callOnFlip(key_resp_10.clearEvents, eventType='keyboard')  # clear events on next screen flip
    if key_resp_10.status == STARTED and not waitOnFlip:
        theseKeys = key_resp_10.getKeys(keyList=['y', 'n', 'left', 'right', 'space', 'return'], waitRelease=False)
        _key_resp_10_allKeys.extend(theseKeys)
        if len(_key_resp_10_allKeys):
            key_resp_10.keys = _key_resp_10_allKeys[-1].name  # just the last key pressed
            key_resp_10.rt = _key_resp_10_allKeys[-1].rt
            # a response ends the routine
            continueRoutine = False
    
    # check for quit (typically the Esc key)
    if endExpNow or defaultKeyboard.getKeys(keyList=["escape"]):
        core.quit()
    
    # check if all components have finished
    if not continueRoutine:  # a component has requested a forced-end of Routine
        break
    continueRoutine = False  # will revert to True if at least one component still running
    for thisComponent in EndComponents:
        if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
            continueRoutine = True
            break  # at least one component has not yet finished
    
    # refresh the screen
    if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
        win.flip()
        win1.flip()
        win2.flip()

# -------Ending Routine "End"-------
for thisComponent in EndComponents:
    if hasattr(thisComponent, "setAutoDraw"):
        thisComponent.setAutoDraw(False)
thisExp.addData('text_7.started', text_7.tStartRefresh)
thisExp.addData('text_7.stopped', text_7.tStopRefresh)
# check responses
if key_resp_10.keys in ['', [], None]:  # No response was made
    key_resp_10.keys = None
thisExp.addData('key_resp_10.keys',key_resp_10.keys)
if key_resp_10.keys != None:  # we had a response
    thisExp.addData('key_resp_10.rt', key_resp_10.rt)
thisExp.addData('key_resp_10.started', key_resp_10.tStartRefresh)
thisExp.addData('key_resp_10.stopped', key_resp_10.tStopRefresh)
thisExp.nextEntry()
# the Routine "End" was not non-slip safe, so reset the non-slip timer
routineTimer.reset()

# Flip one final time so any remaining win.callOnFlip() 
# and win.timeOnFlip() tasks get executed before quitting
win.flip()
win1.flip()
win2.flip()

# these shouldn't be strictly necessary (should auto-save)
thisExp.saveAsWideText(filename+'.csv', delim='auto')
thisExp.saveAsPickle(filename)
logging.flush()
# make sure everything is closed down
thisExp.abort()  # or data files will save again on exit
win.close()
win1.close()
win2.close()
core.quit()
