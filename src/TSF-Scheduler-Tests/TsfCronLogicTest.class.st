"
I'm focusing on the logic.

I'm testing the priority queue and idempotency without actually having to start the timer thread. For this, I'm accessing the internal variable *tasks* (only during testing!).
"
Class {
	#name : 'TsfCronLogicTest',
	#superclass : 'TsfTestCase',
	#category : 'TSF-Scheduler-Tests-Task',
	#package : 'TSF-Scheduler-Tests',
	#tag : 'Task'
}

{ #category : 'tests' }
TsfCronLogicTest >> testIdempotencyEnsureTask [
	| cron taskA taskB internalTasks |

	cron := TsfCron current.

	"1. First Call"
	taskA := cron ensureTaskNamed: 'TestTask' frequency: 5 minutes action: [ 1 ].

	"2. Second Call with other parameter set"
	taskB := cron ensureTaskNamed: 'TestTask' frequency: 10 minutes action: [ 2 ].

	internalTasks := cron instVarNamed: 'tasks'.

	self assert: (internalTasks size = 1) description: 'It must be only one task'.
	self assert: (taskA == taskB) description: 'It must be an identically Object'.
	self assert: (taskA frequency = 10 minutes) description: 'Frequency must be updated'.
	
]

{ #category : 'tests' }
TsfCronLogicTest >> testPrioritySorting [
	| cron task1 task2 task3 internalTasks |

	cron := TsfCron current.

	task1 := TsfTask new nextRun: (DateAndTime now + 10 minutes).
	task2 := TsfTask new nextRun: (DateAndTime now + 1 minute).
	task3 := TsfTask new nextRun: (DateAndTime now + 5 minutes).

	cron addPeriodicTask: task1.
	cron addPeriodicTask: task2.
	cron addPeriodicTask: task3.

	"Access to private variable 'tasks' only for testing"
	internalTasks := cron instVarNamed: 'tasks'.

	self assert: internalTasks size equals: 3.
	self assert: (internalTasks first = task2) description: 'Task in 1 minute must be first'.
	self assert: internalTasks second equals: task3.
	self assert: internalTasks third equals: task1.
	
]

{ #category : 'tests' }
TsfCronLogicTest >> testSubclassingIntegration [
	| finishedSem task |

	finishedSem := Semaphore new.

	TsfScheduler current start.

	"HERE we use the Mock-Class:"
	task := TsfMockTask new.
	task name: 'Subclass Test'.
	
	"We'll give her the semaphore so she can let us know when she's finished."
	task setSemaphore: finishedSem.

	"Add it to the scheduler!"
	
	TsfScheduler current scheduleTask: task.

	"Since we know (according to the transcript) that the task is running and signaling, 
	 this call will return immediately once the task is finished. If the test hangs here 
	 indefinitely, we know that the signal never arrived."
	
	Transcript show: 'Test is waiting now (blocking)...'; cr.
	finishedSem wait.
	Transcript show: 'Test is wake up now (Signal receivedn!)'; cr.

	"We briefly hand over the CPU so that the scheduler process""
	""can execute the line 'state := #finished'."
	(Delay forMilliseconds: 1) wait.
	
    "If we arrive here, the wait was successful."
    self assert: task isFinished.


	
]
