"
I'm a pillar specific class to manage code not generated for the configuration class. Pillar code should not use me but my subclass.
"
Class {
	#name : #ChrysalPillarishConfiguration,
	#superclass : #ChrysalConfiguration,
	#instVars : [
		'printer'
	],
	#category : #'Chrysal-ToBeMovedElsewhere'
}

{ #category : #accessing }
ChrysalPillarishConfiguration >> initialize [
	"This is a hack waiting for a better management of the printer for NodeTransformer."
	super initialize. 
	printer := PRTitlePrinter new.
]

{ #category : #accessing }
ChrysalPillarishConfiguration >> printer [

	^ printer
]
