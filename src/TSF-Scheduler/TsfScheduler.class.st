"
Copyright 2025 Georg Hagn
SPDX-License-Identifier: Apache-2.0


I am  the Worker, a Singleton and know nothing of time, only see work

- Function: I process a queue of *TsfTask* objects sequentially in a dedicated background process
- Thread-Safety: I protect my *taskQueue* with *Mutex*.
- Efficiency: I use a *workSignal* (Semaphore), to sleep in a CPU-efficent manner when the queue is empty.
"
Class {
	#name : 'TsfScheduler',
	#superclass : 'Object',
	#instVars : [
		'taskQueue',
		'workerProcess',
		'accessGuard',
		'workSignal',
		'isRunning',
		'globalErrorHandler',
		'logger'
	],
	#classInstVars : [
		'uniqueInstance'
	],
	#category : 'TSF-Scheduler-Core',
	#package : 'TSF-Scheduler',
	#tag : 'Core'
}

{ #category : 'accessing' }
TsfScheduler class >> current [
	
    uniqueInstance ifNil: [ uniqueInstance := self new ].
    ^ uniqueInstance
]

{ #category : 'class initialization' }
TsfScheduler class >> reset [

    uniqueInstance ifNotNil: [
        uniqueInstance stop.
        uniqueInstance := nil.
    ].

]

{ #category : 'actions' }
TsfScheduler class >> start [

	self current start
]

{ #category : 'actions' }
TsfScheduler class >> stop [ 

	uniqueInstance ifNotNil: [ uniqueInstance stop ].

]

{ #category : 'accessing' }
TsfScheduler >> globalErrorHandler: aBlock [
	
	globalErrorHandler := aBlock.
]

{ #category : 'error handling' }
TsfScheduler >> handleError: anError forTask: aTask [

	globalErrorHandler ifNotNil: [ 
		globalErrorHandler value: aTask value: anError 
	].
]

{ #category : 'initialization' }
TsfScheduler >> initialize [

	super initialize.
	
	taskQueue := OrderedCollection new.
	accessGuard := Mutex new. "protect the queue "
	workSignal := Semaphore new. "wake up worker"
	isRunning := false.

	"Default: simply write to transcript"
	globalErrorHandler := [ :task :err | 
		self logError: 'CRITICAL ERROR in Task ' , task name.
		self logError: err description.
	].

]

{ #category : 'accessing' }
TsfScheduler >> isRunning [ 

	^ isRunning
]

{ #category : 'error handling' }
TsfScheduler >> logError: aString [

    logger 
        ifNotNil: [ logger error: '[Scheduler] ', aString ]
        ifNil: [ Transcript show: '[Scheduler ERROR] '; show: aString; cr ]
]

{ #category : 'accessing' }
TsfScheduler >> logInfo: aString [
    "Die Weiche: Haben wir einen Logger? Wenn ja, nutze ihn. Wenn nein, Transcript."

    logger 
        ifNotNil: [ logger info: '[Scheduler] ', aString ]
        ifNil: [ Transcript show: '[Scheduler] ' ; show: aString; cr ]
]

{ #category : 'private - actions' }
TsfScheduler >> processNextTask [
	| task |

	"wait, until signal arising. (do work)"
	workSignal wait.
    
	"fetch next task thread-safe"
	accessGuard critical: [
		taskQueue ifNotEmpty: [ task := taskQueue removeFirst ]
	].

    "do task (aside the critical sections)"
    task ifNotNil: [ task execute ].

]

{ #category : 'private - actions' }
TsfScheduler >> runLoop [

	[ isRunning ] whileTrue: [
		self processNextTask.
	].

	self logInfo: 'TsfScheduler Worker gestoppt.'.
	
]

{ #category : 'actions' }
TsfScheduler >> scheduleBlock: aBlock [
	| task |

	task := TsfTask new setAction: aBlock; yourself.
	self scheduleTask: task.
	^ task
]

{ #category : 'actions' }
TsfScheduler >> scheduleTask: aTsfTask [

	accessGuard critical: [
		taskQueue add: aTsfTask.
	].

	workSignal signal.
]

{ #category : 'running' }
TsfScheduler >> start [

	isRunning ifTrue: [ ^ self ].

	isRunning := true.
	workerProcess := [ self runLoop ] forkAt: Processor userBackgroundPriority named: 'TSF Scheduler Worker'.

	self logInfo: 'TSF Scheduler Worker started'.

]

{ #category : 'running' }
TsfScheduler >> stop [

	isRunning ifFalse: [ ^ self ].
	isRunning := false.
    
	"WICHTIG: Den Worker aufwecken! Wenn er am workSignal wartet, 
	muss er aufwachen, um zu merken, dass isRunning jetzt false ist."
	workSignal signal.
    
	"Optional: Warten bis der Prozess wirklich tot ist"
	workerProcess ifNotNil: [
		[ workerProcess isTerminated ] whileFalse: [ (Delay forMilliseconds: 10) wait ].
		workerProcess := nil.
	].

	self logInfo: 'TSF Scheduler Worker stoped'.
	
]
