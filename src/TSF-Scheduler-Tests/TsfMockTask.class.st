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

{ #category : 'subclass creation' }
TsfMockTask >> executeAction [

	Transcript show: 'Mock: executeAction startet...'; cr.

	"IMPORTANT: Check if the variable exists, then signal"
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
	Transcript show: 'Mock: Semaphore was set.'; cr.
]
