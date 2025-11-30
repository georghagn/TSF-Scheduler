Class {
	#name : 'TsfGenericTaskTest',
	#superclass : 'TsfTestCase',
	#category : 'TSF-Scheduler-Tests-Task',
	#package : 'TSF-Scheduler-Tests',
	#tag : 'Task'
}

{ #category : 'tests' }
TsfGenericTaskTest >> testGenericBlock [
	| wasExecuted task |

	wasExecuted := false.
	"task := TsfGenericActionTask named: 'Test' do: [ wasExecuted := true ]."
	task := TsfTask named: 'Test' do: [ wasExecuted := true ].
	task execute.
	
	self assert: wasExecuted.
]

{ #category : 'tests' }
TsfGenericTaskTest >> testGenericTranscript [
	| obj task |

	obj := TsfMockTranscript new.

	task := TsfTask 
		named: 'TestTranscriptOutput' 
		receiver: obj 
		selector: #doOutput.
		
	task frequency: 10 seconds.
	
	TsfCron current start.
	TsfScheduler current start.

	task execute.

]
