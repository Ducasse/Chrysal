Class {
	#name : #BaselineOfChrysal,
	#superclass : #BaselineOf,
	#category : #BaselineOfChrysal
}

{ #category : #baselines }
BaselineOfChrysal >> baseline: spec [
	<baseline>
	
	spec
		baseline: 'ContainersPropertyEnvironment' 
		with: [ spec repository: 'github://Ducasse/Containers-PropertyEnvironment' ].
		
	spec 
		for: #pharo 
		do: [	spec 
					package: 'Chrysal' with: [ spec requires: #('ContainersPropertyEnvironment') ] ]
]
