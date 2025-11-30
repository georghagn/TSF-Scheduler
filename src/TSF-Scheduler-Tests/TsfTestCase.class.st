Class {
	#name : 'TsfTestCase',
	#superclass : 'TestCase',
	#category : 'TSF-Scheduler-Tests-Scheduler',
	#package : 'TSF-Scheduler-Tests',
	#tag : 'Scheduler'
}

{ #category : 'running' }
TsfTestCase >> setUp [

	super setUp.

	"Before each test, make sure everything is stopped and reset"
	TsfCron reset.
	TsfScheduler reset.

	"We're giving the systems a short time to really shut down."
	(Delay forMilliseconds: 10) wait.

]
