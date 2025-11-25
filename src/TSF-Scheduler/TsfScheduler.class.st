"
Ich, **der Arbeiter / the Worker**

Ich bin ein Singleton und weiß nichts von Zeit, sondern sehe nur Arbeit

- **Funktion:** Ich arbeite eine Queue von `TsfTask`-Objekten sequenziell in einem eigenen Hintergrundprozess ab.
- **Thread-Sicherheit:** Schützte meine `taskQueue` mit einem `Mutex`.
- **Effizienz:** Nutzte ein `workSignal` (Semaphore), um CPU-schonend zu schlafen, wenn die Queue leer ist.
 
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
		'UniqueInstance'
	],
	#category : 'TSF-Scheduler-Scheduler',
	#package : 'TSF-Scheduler',
	#tag : 'Scheduler'
}

{ #category : 'accessing' }
TsfScheduler class >> current [
    UniqueInstance ifNil: [ UniqueInstance := self new ].
    ^ UniqueInstance

]

{ #category : 'initialization' }
TsfScheduler class >> reset [
    "Nützlich zum Neustarten während der Entwicklung"
    UniqueInstance ifNotNil: [
        UniqueInstance stop.
        UniqueInstance := nil.
    ].

]

{ #category : 'accessing' }
TsfScheduler class >> start [

	self current start
]

{ #category : 'accessing' }
TsfScheduler class >> stop [ 

	UniqueInstance ifNotNil: [ UniqueInstance stop ].
]

{ #category : 'accessing' }
TsfScheduler >> globalErrorHandler: aBlock [
	"Block erwartet zwei Argumente: den Task und den Fehler"
	
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
	accessGuard := Mutex new. "Schützt die Queue vor gleichzeitigem Zugriff"
	workSignal := Semaphore new. "Weckt den Worker auf"
	isRunning := false.

	"Default: Einfach in den Transcript schreiben"
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

	"Warte, bis ein Signal kommt (d.h. Arbeit ist da)"
	workSignal wait.
    
	"Hole den nächsten Task thread-safe"
	accessGuard critical: [
		taskQueue ifNotEmpty: [ task := taskQueue removeFirst ]
	].

    "Führe Task aus (außerhalb des Critical Sections!)"
    task ifNotNil: [ task schedule ].

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
"Convenience Methode: Erstellt Task und plant ihn ein"
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

	"Signalisiert dem Worker, dass Arbeit da ist"
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
