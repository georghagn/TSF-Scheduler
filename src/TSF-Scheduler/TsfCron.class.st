"
Ich, **der Zeitgeber / the Timer**

Ich bin Singleton, das die Zeit verwaltet und den Scheduler füttert.
- **Funktion:** Verwalte periodische Tasks in einer Priority Queue (nach `nextRun` sortiert).
-  **Effizienz:** Nutze ""Interruptible Wait"". Ich (mein Prozess) schlafe exakt bis zur Fälligkeit des nächsten Tasks, kann aber durch ein `wakeUpSignal` (z.B. beim Hinzufügen eines neuen Tasks) sofort geweckt werden.

"
Class {
	#name : 'TsfCron',
	#superclass : 'Object',
	#instVars : [
		'tasks',
		'accessGuard',
		'wakeUpSignal',
		'workerProcess',
		'isRunning'
	],
	#classInstVars : [
		'UniqueInstance'
	],
	#category : 'TSF-Scheduler-Task',
	#package : 'TSF-Scheduler',
	#tag : 'Task'
}

{ #category : 'accessing' }
TsfCron class >> current [

	UniqueInstance ifNil: [ UniqueInstance := self new ].
	^ UniqueInstance

]

{ #category : 'class initialization' }
TsfCron class >> reset [

	UniqueInstance ifNotNil: [ 
		UniqueInstance stop. 
		UniqueInstance := nil.
	].

]

{ #category : 'accessing' }
TsfCron class >> start [

	self current start.

]

{ #category : 'accessing' }
TsfCron class >> stop [

	UniqueInstance ifNotNil: [ UniqueInstance stop ].
]

{ #category : 'adding' }
TsfCron >> addPeriodicTask: aTsfTask [

	"Sicherstellen, dass nextRun gesetzt ist"
	aTsfTask nextRun ifNil: [ aTsfTask nextRun: DateAndTime now ].

	accessGuard critical: [
		tasks add: aTsfTask.
	].
    
	"Den Timer-Loop aufwecken, damit er die neue Zeit prüft"
	wakeUpSignal signal.
	
]

{ #category : 'private - actions' }
TsfCron >> ensureTask: aTaskName class: aTaskClass frequency: aDuration [
	| task created |

	created := false.
	task := self findTaskByName: aTaskName.

	"1. Prüfen ob Task existiert"
	task ifNil: [
		"Neu erstellen aus der übergebenen Klasse!"
		task := aTaskClass new.
		task name: aTaskName.
		created := true.
		self addPeriodicTask: task.
	] ifNotNil: [
		"Optional: Prüfen ob die Klasse noch stimmt (falls du refactorst)"
		task class = aTaskClass ifFalse: [
			"Alter Task ist falsche Klasse -> Rauswerfen und neu machen"
			self removeTask: task.
			^ self ensureTask: aTaskName class: aTaskClass frequency: aDuration.
		]
	].

	"2. Konfiguration aktualisieren"
	task frequency: aDuration.
    
	"Wir setzen KEINE action (block), da die Logik ja in der Klasse steckt!"
	task setAction: nil. 

	created 
		ifTrue: [ 
			Transcript 
				show: 'Class-Task NEU: ';
				showCr: aTaskName; cr.
		] ifFalse: [ 
			Transcript 
				show: 'Class-Task UPDATE: ';
				show: aTaskName; cr.
		].

	^ task
	
]

{ #category : 'private - actions' }
TsfCron >> ensureTaskNamed: aString frequency: aDuration action: aBlock [
	| task created |

	created := false.
    
	"1. Versuchen, den Task zu finden"
	task := self findTaskByName: aString.

	"2. Wenn nicht gefunden, neu erstellen und registrieren"
	task ifNil: [
		task := TsfTask new.
		task name: aString.
		self addPeriodicTask: task.
		created := true.
	].

	"3. Eigenschaften aktualisieren (egal ob neu oder alt!)"
	"Das erlaubt dir, den Code im Block live zu ändern."
	task frequency: aDuration.
	task setAction: aBlock.
    
	"4. Falls er pausiert war, könnte man ihn hier automatisch fortsetzen (optional)"
	"task resume."

	created ifTrue: [ 
		Transcript 
			show: 'Task NEU erstellt: ';
			show: aString; cr.
		] ifFalse: [ 
			Transcript show: 'Task AKTUALISIERT: ';
			show: aString; cr. ].

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

	"Die Collection sortiert sich automatisch nach der Zeit"
	tasks := SortedCollection sortBlock: [ :a :b | a nextRun < b nextRun ].

	accessGuard := Mutex new.
	wakeUpSignal := Semaphore new.
	isRunning := false.
	
]

{ #category : 'private - actions' }
TsfCron >> processDueTask [
	| task |

	accessGuard critical: [
		tasks ifEmpty: [ ^ self ].
		task := tasks first.

		"Sicherheitscheck: Ist er wirklich fällig? (Könnte ja ein Signal gewesen sein)"
		task nextRun <= DateAndTime now ifFalse: [ ^ self ].

		"Task aus der Liste nehmen"
		tasks removeFirst.
	].

	"Nur ausführen - NICHT sofort wieder hinzufügen!"
	TsfScheduler current scheduleTask: task.
    
	"Der Task selbst (oder der Scheduler) muss sich darum kümmern, dass er nach Abschluss wiederkommt."
	
]

{ #category : 'removing' }
TsfCron >> removeTask: aTask [

	accessGuard critical: [
		"Wir versuchen, den Task aus der Liste zu entfernen."
		"ifAbsent: [] ist wichtig, falls er gerade schon beim Scheduler liegt."
		tasks remove: aTask ifAbsent: [ ^ self ].
	].
    
	"Optional: Wir setzen ihn sicherheitshalber auch auf cancelled,"
	"falls er gerade schon an den Scheduler übergeben wurde."
	aTask cancel.
]

{ #category : 'private - actions' }
TsfCron >> runLoop [

	[ isRunning ] whileTrue: [
		self waitForNextTask.
	].

	"Hier kommt der Prozess nur hin, wenn isRunning auf false gesetzt wurde."
	Transcript show: 'TsfCron gestoppt.'; cr.
]

{ #category : 'actions' }
TsfCron >> start [

	isRunning ifTrue: [ ^ self ].

	isRunning := true.
	workerProcess := [ self runLoop ] forkAt: Processor userBackgroundPriority named: 'TSF Cron Timer'.
	
	Transcript show: 'TSF Cron Timer started' ; cr.


]

{ #category : 'accessing' }
TsfCron >> stop [

	isRunning ifFalse: [ ^ self ].
	
	isRunning := false.

    "WICHTIG: Wir müssen den Prozess aufwecken!"
    "Er hängt ja wahrscheinlich gerade in wakeUpSignal wait oder waitTimeoutSeconds."
    wakeUpSignal signal.

    workerProcess := nil.
]

{ #category : 'private - actions' }
TsfCron >> waitForNextTask [
	| delay nextTask now |

	now := DateAndTime now.
	delay := 0.

	"1. Nächsten Task und Wartezeit ermitteln"
	accessGuard critical: [
		tasks ifEmpty: [ 
			delay := nil. "Unendlich warten"
		] ifNotEmpty: [
			nextTask := tasks first.
			delay := (nextTask nextRun - now) asSeconds.
		]
	].

	"2. Warten (Intelligent Sleep)"
	delay ifNil: [ 
		"Liste leer: Schlafe bis ein Task hinzugefügt wird"
		wakeUpSignal wait. 
	] ifNotNil: [
		delay <= 0 ifTrue: [ 
			"Task ist überfällig! Sofort verarbeiten"
			self processDueTask. 
		] ifFalse: [ 
			"Warten: Entweder Zeit abgelaufen ODER unterbrochen durch neuen Task"
			wakeUpSignal waitTimeoutSeconds: delay.

			"Nach dem Aufwachen beginnt der Loop von vorn und prüft neu"
		]
	].

]
