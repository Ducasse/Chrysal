"
I'm the superclass of the generated configuration classes. 
I'm needed because I define some behavior for the creation of instances of such generated classes. I also make sure that conversion methods are invoked when creating instances of such subclasses. 

I'm packaged in the Chrysal-Runtime package because users of Chrysal may want to package their configuration without the full Chrysal builder. 
"
Class {
	#name : #ChrysalConfiguration,
	#superclass : #CTEnvironment,
	#category : #'Chrysal-Runtime'
}

{ #category : #'instance creation others' }
ChrysalConfiguration class >> newFromConvertedDictionary: aDictionary [
	"Assuming that aDictionary contains values which are already objects (i.e., false and not 'false') create a configuration based on this dictionary"
	
	| conf |
	conf := self new.
	aDictionary associations do: [ :aBinding | conf propertyAt: aBinding key asSymbol  put: aBinding value ].
	^ conf
]

{ #category : #'instance creation' }
ChrysalConfiguration class >> newFromDictionary: stonDictionary [
	"It seems that pillar cocoon configuration semantics is to consider the argument as a ston dictionary, i.e., with strings and not objects as values."
	
	^ self newFromSTONDictionary: stonDictionary
]

{ #category : #'compatible with pillar' }
ChrysalConfiguration class >> newFromDictionary: aSTONDictionary extendedWith: anotherDictionary [

	| configuration |
	configuration :=  self newFromSTONDictionary: aSTONDictionary.
	anotherDictionary associations do: [ :aBinding | configuration propertyAt: aBinding key put: aBinding value].
	^ configuration
]

{ #category : #'instance creation' }
ChrysalConfiguration class >> newFromFile: aFile [
	
	^ aFile readStreamDo: [ :input | self newFromStream: input ]
]

{ #category : #'compatible with pillar' }
ChrysalConfiguration class >> newFromFile: aFile extendedWith: anotherDictionary [
	^ aFile readStreamDo: [ :input | self newFromStream: input extendedWith: anotherDictionary ]
]

{ #category : #'instance creation others' }
ChrysalConfiguration class >> newFromSTONDictionary: stonDictionary [
	"Create a configuration based on a dictionary with the objects as defined in STON. 'string' for files and not files. Now pay attention since some literal objects such as true, numbers are not represented as string in STON."
	
	| conf |
	conf := self new.
	stonDictionary associations do: [ :aBinding | conf setConvertedValueOf: aBinding ].
	conf postTreat.
	^ conf
]

{ #category : #'instance creation' }
ChrysalConfiguration class >> newFromStream: aStream [
	"Create a new configuration from the stream, values are converted accordingly to the properties.
	i.e., a string 'false' will be turned into a false object in the resulting configuration."
	^ self newFromDictionary: (STON fromStream: aStream)
]

{ #category : #'compatible with pillar' }
ChrysalConfiguration class >> newFromStream: aStream extendedWith: anotherDictionary [
	^ self newFromDictionary: (STON fromStream: aStream) extendedWith: anotherDictionary 
			
]

{ #category : #'instance creation' }
ChrysalConfiguration class >> newFromString: aString [

	^ self newFromStream: aString readStream
]

{ #category : #'compatible with pillar' }
ChrysalConfiguration class >> newFromString: aString extendedWith: anotherDictionary [
	^ self newFromStream: aString readStream extendedWith: anotherDictionary
]

{ #category : #conversion }
ChrysalConfiguration >> declaredItems [

	^ #()
]

{ #category : #'post hooks' }
ChrysalConfiguration >> postTreat [
	"I'm a hook so that my subclasses can perform some actions once a configuration is created."
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
