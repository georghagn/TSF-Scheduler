"
Copyright 2025 Georg Hagn
SPDX-License-Identifier: Apache-2.0


I, the Timer, i am Singleton, which manages the time and feeds the scheduler.

- Function: Manage recurring tasks in a priority queue (sorted by *nextRun*).
- Efficiency: Utilize ""interruptible wait"". I (my process) sleep until the next task is due, but can be woken up immediately by a *wakeUpSignal* (e.g., when adding a new task).


"
Class {
	#name : 'TsfCron',
	#superclass : 'Object',
	#instVars : [
		'tasks',
		'accessGuard',
		'wakeUpSignal',
		'workerProcess',
		'isRunning',
		'logger'
	],
	#classInstVars : [
		'uniqueInstance'
	],
	#category : 'TSF-Scheduler-Task',
	#package : 'TSF-Scheduler',
	#tag : 'Task'
}

{ #category : 'accessing' }
TsfCron class >> current [

	uniqueInstance ifNil: [ uniqueInstance := self new ].
	^ uniqueInstance
]

{ #category : 'class initialization' }
TsfCron class >> reset [

	uniqueInstance ifNotNil: [ 
		uniqueInstance stop. 
		uniqueInstance := nil.
	].

]

{ #category : 'class initialization' }
TsfCron class >> start [

	self current start.

]

{ #category : 'class initialization' }
TsfCron class >> stop [

	uniqueInstance ifNotNil: [ uniqueInstance stop ].
	
]

{ #category : 'adding' }
TsfCron >> addPeriodicTask: aTsfTask [

	aTsfTask nextRun ifNil: [ aTsfTask nextRun: DateAndTime now ].

	accessGuard critical: [
		tasks add: aTsfTask.
	].
    
	"Wake up the timer loop so it can check the new time."
	wakeUpSignal signal

]

{ #category : 'private - actions' }
TsfCron >> ensureTask: aTaskName class: aTaskClass frequency: aDuration [
	| task created |

	created := false.
	task := self findTaskByName: aTaskName.

	"1. Check if task is existing"
	task ifNil: [
		"new from  the passed class"
		task := aTaskClass new.
		task name: aTaskName.
		created := true.
		self addPeriodicTask: task.
	] ifNotNil: [
		"Optional: Check if the class is still valid (if you are refactoring)"
		task class = aTaskClass ifFalse: [
			"Alter Task ist falsche Klasse -> Rauswerfen und neu machen"
			self removeTask: task.
			^ self ensureTask: aTaskName class: aTaskClass frequency: aDuration.
		]
	].

	"2. update cofiguration"
	task frequency: aDuration.
    
	"We do NOT use an action (block) because the logic is already contained within the class!"
	task setAction: nil. 

	created 
		ifTrue: [ 
			self logInfo: 'Class-Task NEU: ' , aTaskName .
		] ifFalse: [ 
			self logInfo: 'Class-Task UPDATE: ' , aTaskName .
		].

	^ task
]

{ #category : 'private - actions' }
TsfCron >> ensureTaskNamed: aString frequency: aDuration action: aBlock [
	| task created |

	created := false.
    
	"1. Check if task is existing"
	task := self findTaskByName: aString.

	"2. If not found, create a new one and register"
	task ifNil: [
		task := TsfTask new.
		task name: aString.
		self addPeriodicTask: task.
		created := true.
	].

	"3. Update properties (whether new or old!)
		This allows you to change the code in the block live.."
	task frequency: aDuration.
	task setAction: aBlock.
    
	"4. If it was paused, it could be automatically resumed here (optional)."
	task resume.

	created ifTrue: [ 
		self logInfo: 'Task CREATED NEW: ' , aString .
	] ifFalse: [ 
		self logInfo: 'Task CREATED UPDATED: ' , aString .
	].


	^ task
]

{ #category : 'private - accessing' }
TsfCron >> findTaskByName: aString [

	accessGuard critical: [
		^ tasks detect: [ :t | t name = aString ] ifNone: [ nil ]
	].
]

{ #category : 'initialization' }
TsfCron >> initialize [

	super initialize.

	"The collection automatically sorts itself by time."
	tasks := SortedCollection sortBlock: [ :a :b | a nextRun < b nextRun ].

	accessGuard := Mutex new.
	wakeUpSignal := Semaphore new.
	isRunning := false.
]

{ #category : 'error handling' }
TsfCron >> logError: aString [

    logger 
        ifNotNil: [ logger error: '[TsfCron] ', aString ]
        ifNil: [ Transcript show: '[TsfCron ERROR] '; show: aString; cr ]
]

{ #category : 'accessing' }
TsfCron >> logInfo: aString [
    "Die Weiche: Haben wir einen Logger? Wenn ja, nutze ihn. Wenn nein, Transcript."

    logger 
        ifNotNil: [ logger info: '[TsfCron] ', aString ]
        ifNil: [ Transcript show: '[TsfCron] '; show: aString; cr ]
]

{ #category : 'accessing' }
TsfCron >> logger: aLoggerObject [

    "Dependency Injection: Hier kommt der echte TsfLogger rein"
    logger := aLoggerObject
]

{ #category : 'private - actions' }
TsfCron >> processDueTask [
	| task |

	accessGuard critical: [
		tasks ifEmpty: [ ^ self ].
		task := tasks first.

		"Security check: Is it really due? (It could have been a signal)"
		task nextRun <= DateAndTime now ifFalse: [ ^ self ].

		"remove task from collection"
		tasks removeFirst.
	].

	"Execute only - DO NOT add again immediately!"
	TsfScheduler current scheduleTask: task.
    
	"The task itself (or the scheduler) must ensure that it returns after completion."
	
]

{ #category : 'removing' }
TsfCron >> removeTask: aTask [

	accessGuard critical: [
		"We are trying to remove the task from the list."
		"ifAbsent: [] is important if it's already with the scheduler."
		tasks remove: aTask ifAbsent: [ ^ self ].
	].
    
	"Optional: We'll also mark it as cancelled as a precaution,
	in case it has already been handed off to the scheduler."
	aTask cancel.
]

{ #category : 'debugging' }
TsfCron >> report [

	Transcript clear.
	self 
		logInfo: '=== TSF SCHEDULER REPORT ===';
		logInfo: '=== TSF SCHEDULER REPORT ==='.

	self
		logInfo: 'Timer Status: ';
		logInfo: (isRunning ifTrue: ['RUNNING'] ifFalse: ['STOPPED']);
		logInfo: 'Worker Status: ';
		logInfo: (TsfScheduler current isRunning ifTrue: ['RUNNING'] ifFalse: ['STOPPED']).
    
	 self 
		logInfo: '------------------------------';
		logInfo: 'QUEUED TASKS (';
		logInfo: tasks size asString;
		 logInfo: '):'.
    
	accessGuard critical: [
		tasks do: [ :t |
				self
					logInfo: '[';
					logInfo:  t state asString;
					logInfo: '] ';
					logInfo: t name;
					logInfo: ' (Next: ';
					logInfo: t nextRun asTime asString;
					logInfo: ')'.
		]
	].

	self logInfo: '=============================='.

]

{ #category : 'private - actions' }
TsfCron >> runLoop [

	[ isRunning ] whileTrue: [
		self waitForNextTask.
	].

	"The process only arrives here if isRunning has been set to false.."
	self logInfo: 'TsfCron stopped.'.

]

{ #category : 'accessing' }
TsfCron >> start [

	isRunning ifTrue: [ ^ self ].

	isRunning := true.
	workerProcess := [ self runLoop ] forkAt: Processor userBackgroundPriority named: 'TSF Cron Timer'.
	
	self logInfo: 'TSF Cron Timer started'.


]

{ #category : 'accessing' }
TsfCron >> stop [

	isRunning ifFalse: [ ^ self ].
	
	isRunning := false.

    "IMPORTANT: We need to wake up the process! It's probably stuck in 
     wakeUpSignal wait or waitTimeoutSeconds."
    wakeUpSignal signal.

    workerProcess := nil.

	self logInfo: 'TSF Cron Timer stoped'.
]

{ #category : 'private - actions' }
TsfCron >> waitForNextTask [
	| delay nextTask now |

	now := DateAndTime now.
	delay := 0.

	"1. next task and delaytimed calculation"
	accessGuard critical: [
		tasks ifEmpty: [ 
			delay := nil. "wait for ever"
		] ifNotEmpty: [
			nextTask := tasks first.
			delay := (nextTask nextRun - now) asSeconds.
		]
	].

	"2. Waiting (Intelligent Sleep)"
	delay ifNil: [ 
		"Collection empty: sleep until next task ist ready"
		wakeUpSignal wait. 
	] ifNotNil: [
		delay <= 0 ifTrue: [ 
			"This task is overdue! Process it immediately"
			self processDueTask. 
		] ifFalse: [ 
			"Waiting: Either time expires OR interrupted by a new task"
			wakeUpSignal waitTimeoutSeconds: delay.

			"After waking up, the loop starts again and checks new."
		]
	].
]
