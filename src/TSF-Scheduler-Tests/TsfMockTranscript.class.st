Class {
	#name : 'TsfMockTranscript',
	#superclass : 'Object',
	#category : 'TSF-Scheduler-Tests-Task',
	#package : 'TSF-Scheduler-Tests',
	#tag : 'Task'
}

{ #category : 'testing' }
TsfMockTranscript >> doOutput [

	Transcript show: 'Hello from Mock'; cr.

]
