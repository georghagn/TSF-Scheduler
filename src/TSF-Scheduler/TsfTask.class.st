"
I am the work unit

I encapsulate *what* needs to be done and its *state*.

- Dual execution mode: I support both ad-hoc blocks (`setAction:`) for scripts and clean subclassing by overriding *executeAction* (Command Pattern).

- State management: I manage states (#pending, #running, #finished, #failed, #cancelled, #skipped).
- Lifecycle control: I can be paused (pause) or canceled (cancel) before execution.
- Error handling: I catch errors in execute, store them (caughtError), call individual *onFailure: callbacks, and inform the scheduler's global handler.

In short:

I encapsulate the unit of work. I store both what needs to be done (a block or a subclass) and the current state of the task.


"
Class {
	#name : 'TsfTask',
	#superclass : 'Object',
	#instVars : [
		'action',
		'state',
		'result',
		'caughtError',
		'name',
		'successBlock',
		'failureBlock',
		'frequency',
		'nextRun',
		'isCancelled',
		'isPaused'
	],
	#category : 'TSF-Scheduler-Task',
	#package : 'TSF-Scheduler',
	#tag : 'Task'
}

{ #category : 'default' }
TsfTask class >> defaultName [ 

	^ 'Unnamed Task'.
]

{ #category : 'accessing' }
TsfTask >> action [

	^ action.

]

{ #category : 'accessing' }
TsfTask >> action: aBlock [

	action := aBlock 
]

{ #category : 'actions' }
TsfTask >> cancel [

	isCancelled := true.
]

{ #category : 'accessing' }
TsfTask >> caughtError [

	^ caughtError
]

{ #category : 'private' }
TsfTask >> execute [

	"1. Hard Stop: If canceled, get out immediately (do not reschedule)"
	isCancelled ifTrue: [ 
		state := #cancelled. 
		^ self 
	].

	state := #running.

	"2. Pause Check: If paused, skip the action, but keep the timer."
	isPaused ifFalse: [
		[
			result := self runAction.
			state := #finished.
			successBlock ifNotNil: [ successBlock value: result ].
		] on: Error do: [ :ex |
			"Error Handling as usual..."
			caughtError := ex.
			state := #failed.
			failureBlock ifNotNil: [ failureBlock value: ex ].
			TsfScheduler current handleError: ex forTask: self.
		].
	] ifTrue: [
		state := #skipped. "new state, useful for debugging"
	].

	"3. Rescheduling (only if not cancelled)"
	"Even paused tasks will reschedule themselves to keep them on schedule!"
	self rescheduleIfPeriodic.
	
]

{ #category : 'subclass creation' }
TsfTask >> executeAction [

	^ self subclassResponsibility

]

{ #category : 'accessing' }
TsfTask >> frequency [

	^ frequency
]

{ #category : 'accessing' }
TsfTask >> frequency: aDuration [

	frequency := aDuration 
]

{ #category : 'initialization' }
TsfTask >> initialize [

	super initialize.
	
	state := #pending.
	name := self class defaultName .
	
	isCancelled := false.
	isPaused := false.
]

{ #category : 'accessing' }
TsfTask >> isCancelled [

	^ isCancelled
]

{ #category : 'testing' }
TsfTask >> isFailed [

	^ state == #failed
]

{ #category : 'testing' }
TsfTask >> isFinished [

	^ state == #finished
]

{ #category : 'accessing' }
TsfTask >> isPaused [

	^ isPaused
]

{ #category : 'accessing' }
TsfTask >> name [

	^ name

]

{ #category : 'accessing' }
TsfTask >> name: aName [

	name := aName
]

{ #category : 'accessing' }
TsfTask >> nextRun [

	^ nextRun ifNil: [ nextRun := DateAndTime now ]

]

{ #category : 'accessing' }
TsfTask >> nextRun: aDateAndTime [

	nextRun := aDateAndTime.
]

{ #category : 'error handling' }
TsfTask >> onFailure: aBlock [


	failureBlock := aBlock.
]

{ #category : 'error handling' }
TsfTask >> onSuccess: aBlock [

	successBlock := aBlock.
]

{ #category : 'actions' }
TsfTask >> pause [

	isPaused := true.
]

{ #category : 'private - actions' }
TsfTask >> rescheduleIfPeriodic [

	"Security check: Dead tasks do not revive"
	isCancelled ifTrue: [ ^ self ].
	
	frequency ifNotNil: [
		"When should we continue? Now + a break. That will definitely prevent any piling up."
		nextRun := DateAndTime now + frequency.

		"back to Cron-Timer"
		TsfCron current addPeriodicTask: self.
	].
]

{ #category : 'initialization' }
TsfTask >> resetState [

	state := #pending.
	result := nil.
	caughtError := nil.
]

{ #category : 'accessing' }
TsfTask >> result [

	^ result
]

{ #category : 'stepping - auto' }
TsfTask >> resume [

	isPaused := false.
]

{ #category : 'actions' }
TsfTask >> runAction [

	"Priority 1: A block was set (Scripting Mode)"
	action ifNotNil: [ ^ action value ].
    
	"Priority 2: the subclass method calling (OO mode)"
	^ self executeAction
]

{ #category : 'private' }
TsfTask >> schedule [

	"1. Hard Stop: If canceled, get out immediately (do not reschedule)"
	isCancelled ifTrue: [ 
		state := #cancelled. 
		^ self 
	].

	state := #running.

	"2. Pause Check: If paused, skip the action, but keep the timer."
	isPaused ifFalse: [
		[
			result := self runAction.
			state := #finished.
			successBlock ifNotNil: [ successBlock value: result ].
		] on: Error do: [ :ex |
			"Error Handling as usual..."
			caughtError := ex.
			state := #failed.
			failureBlock ifNotNil: [ failureBlock value: ex ].
			TsfScheduler current handleError: ex forTask: self.
		].
	] ifTrue: [
		state := #skipped. "new state, useful for debugging"
	].

	"3. Rescheduling (only if not cancelled)"
	"Even paused tasks will reschedule themselves to keep them on schedule!"
	self rescheduleIfPeriodic.
	
]

{ #category : 'initialization' }
TsfTask >> setAction: aBlock [


	action := aBlock 
]
