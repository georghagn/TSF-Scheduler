"
Ich, **die Arbeitseinheit**

Ich kapsele *was* getan werden soll und in welchem *Zustand* es sich befindet.
- **Dualer Ausführungsmodus:** Ich unterstütze sowohl Ad-hoc-Blöcke (`setAction:`) für Skripte als auch sauberes Subclassing durch Überschreiben von `executeAction` (Command Pattern).
* **Zustandsmanagement:** Ich verwaltet Status (`#pending`, `#running`, `#finished`, `#failed`, `#cancelled`, `#skipped`).
* **Lifecycle-Kontrolle:** Ich, kann ""von innen heraus"" vor der Ausführung pausiert (`pause`) oder abgebrochen (`cancel`) werden.
- **Fehlerbehandlung:** Ich fange Fehler in `execute` ab, speichert sie (`caughtError`), rufe individuelle `onFailure:`-Callbacks auf und informiert den globalen Handler des Schedulers.

**Kurz gesagt:**
Ich kapsele die Arbeitseinheit. Ich speichere sowohl was getan werden soll (einen Block oder eine Subclass) als auch in welchem Zustand sich die Aufgabe befindet

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

{ #category : 'initialization' }
TsfTask >> action [
"Erwartet einen Block ohne Argumente, z.B. [ Transcript show: 'Hello' ]"

	^ action.
]

{ #category : 'initialization' }
TsfTask >> action: aBlock [
"Erwartet einen Block ohne Argumente, z.B. [ Transcript show: 'Hello' ]"

	action := aBlock 
]

{ #category : 'actions' }
TsfTask >> cancel [
"Stoppt den Task für immer. Er wird nicht mehr ausgeführt und plant sich nicht neu ein."

	isCancelled := true.

]

{ #category : 'accessing' }
TsfTask >> caughtError [

	^ caughtError
]

{ #category : 'private - actions' }
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
	name := 'Unnamed Task'.
	
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

{ #category : 'actions' }
TsfTask >> isPaused [
"Pausiert die Ausführung. Der Takt (Timer) läuft weiter, aber die Action wird übersprungen."

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
"Der Block erwartet 1 Argument: den Fehler (error)"

	failureBlock := aBlock.
]

{ #category : 'error handling' }
TsfTask >> onSuccess: aBlock [
"Der Block erwartet 1 Argument: das Ergebnis (result)"

	successBlock := aBlock.

]

{ #category : 'actions' }
TsfTask >> pause [
"Pausiert die Ausführung. Der Takt (Timer) läuft weiter, aber die Action wird übersprungen."

	isPaused := true.

]

{ #category : 'private - actions' }
TsfTask >> rescheduleIfPeriodic [

	"Sicherheitscheck: Tote Tasks stehen nicht wieder auf"
	isCancelled ifTrue: [ ^ self ].
	
	frequency ifNotNil: [
		"Wann soll es weitergehen? Jetzt + Pause. Damit verhindern wir das Stapeln garantiert."
		nextRun := DateAndTime now + frequency.

		"Zurück in den Cron-Timer"
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
"Nimmt die Arbeit wieder auf."

	isPaused := false.

]

{ #category : 'private - actions' }
TsfTask >> runAction [

	"Priorität 1: Ein expliziter Block wurde gesetzt (Scripting Mode)"
	action ifNotNil: [ ^ action value ].
    
	"Priorität 2: Die Methode der Subclass aufrufen (OO Mode)"
	^ self executeAction
	
]

{ #category : 'executing' }
TsfTask >> schedule [

	"1. Hard Stop: Wenn gecancelt, sofort raus (und NICHT neu einplanen)"
	isCancelled ifTrue: [ 
		state := #cancelled. 
		^ self 
	].

	state := #running.

	"2. Pause Check: Wenn pausiert, Action überspringen, aber Timer behalten"
	isPaused ifFalse: [
		[
			result := self runAction.
			state := #finished.
			successBlock ifNotNil: [ successBlock value: result ].
		] on: Error do: [ :ex |
			"Error Handling wie gehabt..."
			caughtError := ex.
			state := #failed.
			failureBlock ifNotNil: [ failureBlock value: ex ].
			TsfScheduler current handleError: ex forTask: self.
		].
	] ifTrue: [
		state := #skipped. "Neuer Status für Debugging nützlich"
	].

	"3. Wiederplanung (Nur wenn NICHT gecancelt)"
	"Auch pausierte Tasks planen sich neu ein, damit sie im Takt bleiben!"
	self rescheduleIfPeriodic.

]

{ #category : 'initialization' }
TsfTask >> setAction: aBlock [
"Erwartet einen Block ohne Argumente, z.B. [ Transcript show: 'Hello' ]"

	action := aBlock 
]
