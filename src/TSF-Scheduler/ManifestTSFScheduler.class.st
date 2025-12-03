"
Please describe the package using the class comment of the included manifest class. The manifest class also includes other additional metadata for the package. These meta data are used by other tools such as the SmalllintManifestChecker and the critics Browser
"
Class {
	#name : 'ManifestTSFScheduler',
	#superclass : 'PackageManifest',
	#category : 'TSF-Scheduler-Manifest',
	#package : 'TSF-Scheduler',
	#tag : 'Manifest'
}

{ #category : 'code-critics' }
ManifestTSFScheduler class >> ruleCodeCruftLeftInMethodsRuleV1FalsePositive [

	<ignoreForCoverage>
	^ #(#(#(#RGMethodDefinition #(#TsfCron #report #false)) #'2025-12-03T08:33:11.505398+01:00') )
]
