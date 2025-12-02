"
I check the state transitions, callbacks, and cancel/pause logic without needing a scheduler.
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

	task execute.

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
	task execute.

	self assert: task isCancelled.
	self deny: ran description: 'Cancelled Task must not execute the action.'.

]

{ #category : 'tests' }
TsfTaskTest >> testErrorHandling [
	| task error caughtError |

	error := Error new messageText: 'Test Error'.
	task := TsfTask new.
	task setAction: [ error signal ].
	task onFailure: [ :err | caughtError := err ].

	task execute.

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
	task execute.
	self assert: task isPaused.
	self deny: ran description: 'paused task must not execute the action'.

	self assert: (task instVarNamed: 'state') equals: #skipped.

	"2. Test Resume"
	task resume.
	task execute.
	self assert: ran description: 'The ongoing task must execute the action.'.
	
]
