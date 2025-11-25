"
Ich teste hier das ""echte Leben"". Ich starte die Threads und nutzen Semaphore, um den Test-Thread mit den Worker-Threads zu synchronisieren.
"
Class {
	#name : 'TsfMockTask',
	#superclass : 'TsfTask',
	#instVars : [
		'executionCountSem'
	],
	#category : 'TSF-Scheduler-Tests-Task',
	#package : 'TSF-Scheduler-Tests',
	#tag : 'Task'
}

{ #category : 'private - actions' }
TsfMockTask >> executeAction [
	"Das ist der Beweis! Wenn diese Methode läuft, funktioniert das Subclassing."

	Transcript show: 'Mock: executeAction startet...'; cr.

	"WICHTIG: Prüfen ob die Variable da ist, dann signalisieren"
	executionCountSem ifNil: [ 
		Transcript show: 'ALARM: Semaphore ist nil in executeAction!'; cr. 
	] ifNotNil: [
		executionCountSem signal.
		Transcript show: 'Mock: Signal gesendet!'; cr.
	].
	
]

{ #category : 'initialization' }
TsfMockTask >> setSemaphore: aSemaphore [

	executionCountSem := aSemaphore.
	Transcript show: 'Mock: Semaphore wurde gesetzt.'; cr.
]
