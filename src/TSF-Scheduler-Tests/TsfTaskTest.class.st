"
Ich prüfe die Zustandsübergänge, Callbacks und die Abbrechen/Pausieren-Logik, ohne einen Scheduler zu benötigen.
"
Class {
	#name : 'TsfTaskTest',
	#superclass : 'TsfTestCase',
	#category : 'TSF-Scheduler-Tests-Task',
	#package : 'TSF-Scheduler-Tests',
	#tag : 'Task'
}

{ #category : 'tests' }
TsfTaskTest >> testBasicExecution [
	| task |

	task := TsfTask new.
	task setAction: [ 10 + 20 ].

	task schedule.

	self assert: task isFinished.
	self deny: task isFailed.
	self assert: task result equals: 30.

]

{ #category : 'tests' }
TsfTaskTest >> testCancellation [
	| task ran |

	ran := false.
	task := TsfTask new setAction: [ ran := true ].

	task cancel.
	task schedule.

	self assert: task isCancelled.
	self deny: ran description: 'Gecancelter Task darf action nicht ausführen'.

]

{ #category : 'tests' }
TsfTaskTest >> testErrorHandling [
	| task error caughtError |

	error := Error new messageText: 'Test Error'.
	task := TsfTask new.
	task setAction: [ error signal ].
	task onFailure: [ :err | caughtError := err ].

	task schedule.

	self assert: task isFailed.
	self assert: (task caughtError ) equals: error .
	self assert: caughtError equals: error.

]

{ #category : 'tests' }
TsfTaskTest >> testPauseResume [
	| task ran |

	ran := false.
	task := TsfTask new setAction: [ ran := true ].

	"1. Test Pause"
	task pause.
	task schedule.
	self assert: task isPaused.
	self deny: ran description: 'Pausierter Task darf action nicht ausführen'.

	"Der interne Status sollte auf skipped stehen, wenn man das implementiert hat"
	self assert: (task instVarNamed: 'state') equals: #skipped.

	"2. Test Resume"
	task resume.
	task schedule.
	self assert: ran description: 'Fortgesetzter Task muss action ausführen'.
	
]
