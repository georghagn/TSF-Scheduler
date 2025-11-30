Class {
	#name : 'TsfIntegrationTest',
	#superclass : 'TsfTestCase',
	#category : 'TSF-Scheduler-Tests-Scheduler',
	#package : 'TSF-Scheduler-Tests',
	#tag : 'Scheduler'
}

{ #category : 'tests' }
TsfIntegrationTest >> testFixedDelayStackingPrevention [
	| runCount task |

	runCount := 0.

	TsfScheduler current start.
	TsfCron current start.

	task := TsfTask new.
	task frequency: 200 milliSeconds.
	task setAction: [ 
		"The execution takes longer than the frequency!"
		(Delay forMilliseconds: 400) wait.
		runCount := runCount + 1.
	].
    
	TsfCron current addPeriodicTask: task.
    
	"We wait 1 second."
	"With 'Fixed Rate' (incorrect), it would attempt to run 5 times (every 200ms)."
	"With 'Fixed Delay' (correct), it runs: Start (0ms) -> End (400ms) -> Pause (200ms) -> Start (600ms) -> End (1000ms)."
	"So it should only complete approximately 2 cycles in 1 second."
	(Delay forSeconds: 1) wait.
    
	task cancel.
    
	self assert: runCount <= 2 description: 'The number of tasks has piled up! There should be a maximum of 2 runs.'.
	self assert: runCount >= 1 description: 'The task did not run at all.'.
	
]

{ #category : 'tests' }
TsfIntegrationTest >> testFullSystemPeriodicExecution [
	"Tests the interaction between cron and the scheduler with real delays"
	"The test is very tricky. The result depends on many factors such as VM version, operating system,
	 CPU speed, etc. Even with my configuration, the test is sometimes successful and sometimes not."
	| runSemaphore task |

	runSemaphore := Semaphore new.

	"1. start system"
	TsfScheduler current start.
	TsfCron current start.

	"2. Create a task that signals the semaphore"
	task := TsfTask new.
	task name: 'Integration Test Task'.
	task frequency: 1 milliSeconds. "Very fast for test"
	task setAction: [ runSemaphore signal ].

	"3. Einplanen"
	TsfCron current addPeriodicTask: task.

	"4. Waiting and Counting We are waiting for the semaphore to signal 3 times."
	self assert: (runSemaphore waitTimeoutSeconds: 1). "Run 1"
	self assert: (runSemaphore waitTimeoutSeconds: 1). "Run 2"
	self assert: (runSemaphore waitTimeoutSeconds: 1). "Run 3"
    
	task cancel.
	"Wait a moment for the cancellation to take effect before the tearDown occurs."
	(Delay forMilliseconds: 150) wait.
	
]

{ #category : 'tests' }
TsfIntegrationTest >> testSubclassingIntegration [
	| finishedSem task |

	finishedSem := Semaphore new.

	TsfScheduler current start.

	task := TsfMockTask new.
	task name: 'Subclass Test'.
	task setSemaphore: finishedSem.

	"Directly into the scheduler (without cron for this test)"
	TsfScheduler current scheduleTask: task.

	"Wait until the task sends 'signal' (max 1 second)"
	(Delay forMilliseconds: 1) wait.
	finishedSem waitTimeoutSeconds: 1.
	
	self assert: task isFinished.

]
