"
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
		'globalErrorHandler'
	],
	#classInstVars : [
		'uniqueInstance'
	],
	#category : 'TSF-Scheduler-Core',
	#package : 'TSF-Scheduler',
	#tag : 'Core'
}

{ #category : 'actions' }
TsfScheduler class >> start [

	self current start
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
		Transcript 
			show: 'CRITICAL ERROR in Task ';
			show: task name; cr.
		Transcript show: err description; cr. 
	].

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

	Transcript show: 'TsfScheduler Worker gestoppt.'; cr.
	
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
	Transcript show: 'TSF Scheduler Worker started' ; cr.

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

	Transcript show: 'TSF Scheduler Worker stoped' ; cr.
	
]
