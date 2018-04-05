"
I'm a dummy class just for the compositeConfigurationItem test. 
I represent a composite domain object.
"
Class {
	#name : #ChrysalCompositeDomain,
	#superclass : #Object,
	#instVars : [
		'level',
		'renderAs',
		'capitalization',
		'numbering',
		'headerSize',
		'autoAnchor'
	],
	#category : #'Chrysal-Tests-Mocks'
}

{ #category : #accessing }
ChrysalCompositeDomain >> autoAnchor [ 
	^ autoAnchor
]

{ #category : #accessing }
ChrysalCompositeDomain >> autoAnchor: aBoolean [
	autoAnchor := aBoolean
]

{ #category : #accessing }
ChrysalCompositeDomain >> capitalization [
	^ capitalization
]

{ #category : #accessing }
ChrysalCompositeDomain >> capitalization: anObject [
	capitalization := anObject
]

{ #category : #accessing }
ChrysalCompositeDomain >> headerSize [
	^ headerSize
]

{ #category : #accessing }
ChrysalCompositeDomain >> headerSize: anObject [
	headerSize := anObject
]

{ #category : #accessing }
ChrysalCompositeDomain >> initialize [

	super initialize. 
	level = 99. 
	capitalization := 'raw'.
	numbering := false.
	renderAs := 'letter'.
	headerSize := 25
]

{ #category : #accessing }
ChrysalCompositeDomain >> level [
	^ level
]

{ #category : #accessing }
ChrysalCompositeDomain >> level: anObject [
	level := anObject
]

{ #category : #accessing }
ChrysalCompositeDomain >> numbering [
	^ numbering
]

{ #category : #accessing }
ChrysalCompositeDomain >> numbering: anObject [
	numbering := anObject
]

{ #category : #accessing }
ChrysalCompositeDomain >> renderAs [
	^ renderAs
]

{ #category : #accessing }
ChrysalCompositeDomain >> renderAs: anObject [
	renderAs := anObject
]
