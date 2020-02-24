"
Now introduce Chrysal-Runtime package containing a simple class whose other configurations will be subclass of. 
"
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
		with: [ spec repository: 'github://Ducasse/Containers-PropertyEnvironment:v1.0.0' ].
	spec
		for: #pharo
		do: [ 
			spec
				package: 'Chrysal-Runtime' with: [ spec requires: #('ContainersPropertyEnvironment') ].
			spec
				package: 'Chrysal-Tests'
				with: [ spec requires: #('Chrysal-Runtime' 'Chrysal') ].
			spec
				package: 'Chrysal'
				with: [ spec requires: #('Chrysal-Runtime') ] ]
]
