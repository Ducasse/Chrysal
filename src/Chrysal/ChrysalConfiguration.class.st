Class {
	#name : #ChrysalConfiguration,
	#superclass : #CTEnvironment,
	#category : #Chrysal
}

{ #category : #'instance creation' }
ChrysalConfiguration class >> newFromDictionary: stonDictionary [
	| conf |
	conf := self new.
	stonDictionary associations do: [ :aBinding | conf setConvertedValueOf: aBinding ].
	^ conf
]

{ #category : #'instance creation' }
ChrysalConfiguration class >> newFromFile: aFile [
	
	^ aFile readStreamDo: [ :input | self newFromStream: input ]
]

{ #category : #'instance creation' }
ChrysalConfiguration class >> newFromStream: aStream [

	^ self newFromDictionary: (STON fromStream: aStream)
]

{ #category : #'instance creation' }
ChrysalConfiguration class >> newFromString: aString [
	^ self newFromStream: aString readStream
]

{ #category : #conversion }
ChrysalConfiguration >> declaredItems [

	^ #()
]

{ #category : #conversion }
ChrysalConfiguration >> setConvertedValueOf: aBinding [
	"Set the converted value of aBinding value in the correct property of the receiver. aBinding should be property and a raw value as written in the configuration"

	(self declaredItems includes: aBinding key asSymbol)
		ifTrue: [ self setConvertedValueOfDeclaredProperty: aBinding ]
		ifFalse: [ self perform: #propertyAt:put: 
								with: aBinding key asSymbol 
								with: aBinding value ]
	
]

{ #category : #conversion }
ChrysalConfiguration >> setConvertedValueOfDeclaredProperty: aBinding [
	"Set the converted value of aBinding value in the correct property of the receiver. 
	aBinding should be property and a raw value as written in the configuration"

	| setter converted |
	setter := aBinding key capitalized , ':'.
	converted := self perform: (#convert , setter) asSymbol with: aBinding value.
	converted ifNil: [ self error: 'aBinding is not correctly spelt ' , aBinding ].
	self perform: setter uncapitalized asSymbol with: converted
]
